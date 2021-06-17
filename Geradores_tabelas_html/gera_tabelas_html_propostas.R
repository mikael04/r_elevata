fct_gera_tabelas_propostas <- function(debug){
  # debug = T
  #Lib q será futuramente usada pros painéis interativos
  #library(shiny)
  #Lib pra conexão com o banco
  #library(odbc)
  #Lib para ler (mais rapidamente) os csvs
  library(data.table)
  #lib com uma cacetada de outras libs para manipular dados
  #library(tidyverse)
  #Libs pra trabalhar com a base (cortes e funções similares ao SQL)
  library(dplyr) #Contido no tidyverse
  #lib para lidar com as palavras
  #library(stringr)
  #library(htmlwidgets)
  #library(viridis)
  #Lib usada pros treemaps
  #library(treemap)
  #Lib usada pros waffles
  #library(waffle)
  #usada para converter números em moeda
  #library(scales)
  #lib para plotar a imagem (s_dados)
  library(knitr)
  source("fct_tempo.R")
  source("fct_fmt_din.R")
  source("fct_fmt_data.R")
  source("fct_fmt_nome.R")
  source("Geradores_tabelas_html/fct_empresas_ativas.R")


  ###################################
  ## Variáveis "Globais"
  #### Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
  teste = T
  #### Variável usada para não plotar os gráficos na dash
  dash = T
  #### Variável de categoria
  tabela_categoria = 'propostas'

  ### Tempo
  if(!teste){
    ## Teste se estou gerando via rstudio (knit)
    if(as.integer(params$num_dias) == 0) {
      #### Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
      ano_atual = fct_ano_atual()
      #### Variavel global c/ mês atual (para comparação)
      mes_atual = fct_mes_atual()
      ## Apenas ano, para gerar títulos
      ano <- year(ano_atual)
      ## Execução normal, recebendo data do gerador de dashs
    }else{
      data <- (lubridate::today()-lubridate::days(params$num_dias))
      #### Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
      ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
      #### Variavel global c/ mês atual (para comparação)
      mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
      ## Apenas ano, para gerar títulos
      ano <- lubridate::year(ano_atual)
    }
  }else{
    ##Teste setando dia
    #data <- lubridate::ymd("2020-12-31")
    data <- lubridate::today()
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
    ## Apenas ano, para gerar títulos
    ano <- lubridate::year(ano_atual)
  }


  ##Teste, senão tiver parâmetro, estou fazendo o teste e entra no if, senão vai pro else
  # Vai receber da função que chamar
  # empresa = 1

  ###################################################################################
  ## Gerando tabela

  ##-> Collect cria o dataframe resultado da query, proposta será a tabela na qual estou lendo (FROM cliente)
  ##coleta todas as propostas
  proposta <- fread("Tabelas/proposta.csv", colClasses = c(proposta_id = "character", proposta_negocio_id = "character")) %>%
    dplyr::select(proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status)

  ##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
  negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
    dplyr::select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id, negocio_cliente_id, negocio_tipo_negocio)

  ##coleta proposta_pagamento
  proposta_pagamento <- fread("Tabelas/proposta_pagamento.csv", colClasses = c(pp_id = "character", pp_proposta_id = "character")) %>%
    dplyr::select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
    dplyr::filter(pp_ativo == 1) %>%
    dplyr::select(-pp_ativo)

  ##coleta proposta_produto
  proposta_produto <- fread("Tabelas/proposta_produto.csv", colClasses = c(pp_id = "character", pp_proposta_id = "character", pp_produto_id = "character")) %>%
    dplyr::select(pp_id, pp_proposta_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo) %>%
    dplyr::filter (pp_ativo == 1) %>% #Filtrando pp_ativo = true
    dplyr::select(-pp_ativo) %>%
    dplyr::mutate (pp_valor_tot = pp_valor*pp_quantidade)

  ##coleta todos os clientes
  cliente <- fread("Tabelas/cliente.csv") %>%
    dplyr::select(cliente_id, cliente_nome, cliente_empresa_id)

  #Arrumando encoding
  Encoding(cliente$cliente_nome) <- 'latin1' #'UTF-8'

  ##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
  produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character", produto_marca_id = "character", produto_categoria_id = "character")) %>%
    dplyr::select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

  #Arrumando #Encoding
  Encoding(produto$produto_nome) <- 'latin1' #'UTF-8'

  ##junção de proposta com negócio
  prop_ij_neg <- inner_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
    dplyr::select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro,
                   negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado, negocio_produto_id, negocio_cliente_id,
                   negocio_tipo_negocio)

  empresas_ativas <- fct_empresas_ativas ()
  ## Inicializando a lista de vendedores
  vendedores <- NULL
  for(i in (1:length(empresas_ativas))){
    if(debug){
      # i = 3
      print(i)
      print(empresas_ativas[[i]])
    }
    ##coleta todos os vendedores
    vendedor <- fread("Tabelas/vendedor.csv") %>%
      dplyr::select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
      dplyr::filter (vendedor_empresa_id == empresas_ativas[[i]])

    vendedor_a <- vendedor %>%
      dplyr::filter (vendedor_ativo == T)

    #Arrumando #Encoding
    Encoding(vendedor_a$vendedor_nome) <- 'latin1' #'UTF-8'
    vendedor_a$vendedor_nome <- sapply(vendedor_a$vendedor_nome, func_nome)

    if(empresas_ativas[[i]] == 16){
      vendedor_a$vendedor_nome[vendedor_a$vendedor_id == 723] <- "BRUNO PE.";
      vendedor_a$vendedor_nome[vendedor_a$vendedor_id == 812] <- "BRUNO PO.";
      vendedor_a$vendedor_nome[vendedor_a$vendedor_id == 942] <- "LUCAS V. I.";
    }

    ###################################

   ##Juntando com vendedor pra obter o nome do vendedor
    prop_ij_neg_ij_vend <- inner_join(prop_ij_neg, vendedor_a, by=c("negocio_vendedor_id" = "vendedor_id"))
    if(nrow(prop_ij_neg_ij_vend) > 0){
      if(debug){
        print("Debug ativo")
        print(paste0("Empresa = ", empresas_ativas[[i]]))
        print(paste0("Número de linhas = ", nrow(prop_ij_neg_ij_vend)))
      }

      p_ij_n_ij_v_ij_pp_n <- inner_join(prop_ij_neg_ij_vend, proposta_produto, by = c("proposta_id" = "pp_proposta_id"))

      ###################################################################################
      ## Criação das tabelas
      ######################
      ## joins e filtros adicionais
      #############################

      ## vendedor, cliente, produto e valor, status, data de cadastro (ordem de cadastro, mais novas primeiro), atualização
      p_ij_n_ij_pp_ij_prod <- dplyr::inner_join (p_ij_n_ij_v_ij_pp_n, produto, by= c('pp_produto_id' = 'produto_id')) %>%
        dplyr::select(proposta_id, vendedor_nome, negocio_cliente_id, proposta_data_cadastro, proposta_status, pp_valor_tot, produto_nome, negocio_tipo_negocio)
      p_ij_n_ij_pp_ij_prod_cli <- dplyr::inner_join (p_ij_n_ij_pp_ij_prod, cliente, by= c('negocio_cliente_id' = 'cliente_id')) %>%
        dplyr::select(-negocio_cliente_id)

      prop_ate_1ano_ant <- p_ij_n_ij_pp_ij_prod_cli %>%
        dplyr::filter(proposta_data_cadastro > ano_atual - years(1)) %>%
        dplyr::select(proposta_id, vendedor_nome, cliente_nome, proposta_data_cadastro, proposta_status, pp_valor_tot, produto_nome, negocio_tipo_negocio)

      prop_ate_1ano_ant <- prop_ate_1ano_ant %>%
        dplyr::group_by(proposta_id) %>%
        ## somo e já converto para impressão (usando a funçao pra formatar o dinheiro)
        dplyr::mutate (valor_proposta = func_fmt_din(sum(pp_valor_tot))) %>%
        dplyr::mutate (Produtos = paste0(produto_nome, collapse = " --- ")) %>%
        ## formato o link
        ##  onclick="window.open('http://google.com', '_blank')">LINK DA PROPOSTA

        dplyr::mutate(Link = paste0('onclick="', "window.open('", paste0("https://letmegooglethat.com/?q=", proposta_data_cadastro, "'"), ", '_blank')", '"', '>LINK DA PROPOSTA')) %>%
        ## formato a data para impressão
        dplyr::mutate ('Data de Cadastro' = func_fmt_data_d_m_Y(proposta_data_cadastro)) %>%
        #dplyr::mutate('Produto + Valor' = paste(Produtos, valor_proposta, sep = "  - ")) %>%
        # dplyr::select(-pp_valor_tot) %>%
        dplyr::distinct(proposta_id, .keep_all = T) %>%
        ## Organizando tabela
        dplyr::arrange(desc(proposta_data_cadastro)) %>%
        dplyr::ungroup () %>%

        ## Selecionando colunas e alterando nomes
        dplyr::select(-proposta_id, -valor_proposta, -proposta_data_cadastro) %>%
        dplyr::rename(Vendedor = vendedor_nome, Cliente = cliente_nome) %>%

        ## Selecionando colunas (com if e alterando tipo de data) e alterando nomes
        dplyr::select(-negocio_tipo_negocio) %>%
        dplyr::select(Cliente, Vendedor, Produtos , 'Data de Cadastro', Link) %>%
        dplyr::ungroup ()

      #Encoding(prop_ate_1ano_ant$Cliente) <- 'latin1'
      #Encoding(prop_ate_1ano_ant$Produtos) <- 'latin1'

      if(debug){
        print("Arquivo Existe, será gerado o html:");
        print(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[[i]] , '.html'))
      }

      # tabela_csv <- tabela_csv %>%
      #   mutate_if(is.character,fix_encoding)
      if(nrow(prop_ate_1ano_ant) > 0){
        vendedores[i] <- prop_ate_1ano_ant %>%
          dplyr::select(Vendedor) %>%
          dplyr::distinct(Vendedor) %>%
          dplyr::arrange(Vendedor)
      }else{
        vendedores[i] <- NULL
      }

      library(tableHTML)
      ## imprime em HTML (para facilitar modificação)
      tableHTML::write_tableHTML(tableHTML::tableHTML(prop_ate_1ano_ant), file = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[[i]] , '.html'))
    }else{
      if(debug){
        print(paste0("Empresa não tem funcionalidade  = ", empresas_ativas[[i]]))
        print(paste0("Número de linhas = ", nrow(prop_ate_1ano_ant)))
      }
    }
  }
  ##########################################################
  ## Retornando a lista (após rodar o for de todas as empresas) com os vendedores de cada empresa
  return(vendedores)
}

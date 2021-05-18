fct_gera_tabelas_negocios <- function(debug){
  #rm(list = ls())
  #debug = T
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
  tabela_categoria = 'negocios'

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

  ##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
  negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
    dplyr::select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id, negocio_cliente_id, negocio_tipo_negocio)

  ##negocio_produto para pegar os valores de cada negócio
  negocio_produto <- fread("Tabelas/negocio_produto.csv", colClasses = c(np_id = "character", np_negocio_id = "character", np_produto_id = "character")) %>%
    dplyr::select(np_id, np_negocio_id, np_produto_id, np_quantidade, np_ativo, np_valor) %>%
    dplyr::filter(np_ativo == T) %>%
    dplyr::mutate (np_valor_tot = np_valor*np_quantidade)

  ##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
  produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character", produto_marca_id = "character", produto_categoria_id = "character")) %>%
    dplyr::select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

  #Arrumando #Encoding
  Encoding(produto$produto_nome) <- 'latin1' #'UTF-8'

  #### Vou alterar o agrupamento de número de negócios para faturamento, portanto precisarei da tabela negocio_produto
  neg_ij_np <- dplyr::inner_join(negocio, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

  ## Juntando com produto para pegar nome do produto
  neg_ij_np <- dplyr::inner_join(neg_ij_np, produto, by=c("np_produto_id" = "produto_id")) %>%
    dplyr::select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado,
                  negocio_cliente_id, negocio_produto_id, negocio_tipo_negocio, np_valor_tot, produto_nome)


  ##coleta todos os clientes
  cliente <- fread("Tabelas/cliente.csv") %>%
    dplyr::select(cliente_id, cliente_nome, cliente_empresa_id)

  #Arrumando encoding
  Encoding(cliente$cliente_nome) <- 'latin1' #'UTF-8'


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

    ## Apenas clientes desta empresa
    cliente_emp <- cliente %>%
      dplyr::filter(cliente_empresa_id == empresas_ativas[[i]])

    ###################################
    ##Juntando com vendedor pra obter o nome do vendedor
    neg_ij_vend <- inner_join(neg_ij_np, vendedor_a, by=c("negocio_vendedor_id" = "vendedor_id"))

    neg_ij_vend_cli <- dplyr::inner_join(neg_ij_vend, cliente, by=c("negocio_cliente_id" = "cliente_id"))



    if(nrow(neg_ij_vend_cli) > 0){
      ###################################################################################
      ## Criação das tabelas
      ######################
      ## joins e filtros adicionais
      #############################

      neg_ij_vend_cli <- neg_ij_vend_cli %>%
        dplyr::select(negocio_id, vendedor_nome, cliente_nome, negocio_data_cadastro, negocio_negocio_situacao_id, np_valor_tot, produto_nome, negocio_tipo_negocio)

      tabela_negocios <- neg_ij_vend_cli %>%
        dplyr::group_by(negocio_id) %>%
        ## somo e já converto para impressão (usando a funçao pra formatar o dinheiro)
        dplyr::mutate (valor_negocio = func_fmt_din(sum(np_valor_tot))) %>%
        dplyr::mutate (Produtos = paste0(produto_nome, collapse = " --- ")) %>%
        ## formato o link
        ##  onclick="window.open('http://google.com', '_blank')">LINK DA PROPOSTA

        dplyr::mutate(Link = paste0('onclick="', "window.open('", paste0("https://letmegooglethat.com/?q=", negocio_data_cadastro, "'"), ", '_blank')", '"', '>LINK DO NEGOCIO')) %>%
        ## formato a data para impressão
        dplyr::mutate ('Data de Cadastro' = func_fmt_data_d_m_Y(negocio_data_cadastro)) %>%
        #dplyr::mutate('Produto + Valor' = paste(Produtos, valor_negocio, sep = "  - ")) %>%
        # dplyr::select(-pp_valor_tot) %>%
        dplyr::distinct(negocio_id, .keep_all = T) %>%
        ## Organizando tabela
        dplyr::arrange(desc(negocio_data_cadastro)) %>%
        dplyr::ungroup () %>%

        ## Selecionando colunas e alterando nomes
        dplyr::select(-negocio_id, -valor_negocio, -negocio_data_cadastro) %>%
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
      if(nrow(tabela_negocios) > 0){
        vendedores[i] <- tabela_negocios %>%
          dplyr::select(Vendedor) %>%
          dplyr::distinct(Vendedor) %>%
          dplyr::arrange(Vendedor)
      }else{
        vendedores[i] <- NULL
      }

      library(tableHTML)
      ## imprime em HTML (para facilitar modificação)
      tableHTML::write_tableHTML(tableHTML::tableHTML(tabela_negocios), file = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[[i]] , '.html'))
    }else{
      if(debug){
        print(paste0("Empresa não tem funcionalidade  = ", empresas_ativas[[i]]))
        print(paste0("Número de linhas = ", nrow(tabela_negocios)))
      }
    }
  }
  ##########################################################
  ## Retornando a lista (após rodar o for de todas as empresas) com os vendedores de cada empresa
  return(vendedores)
}

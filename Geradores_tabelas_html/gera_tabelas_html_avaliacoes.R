
fct_gera_tabelas_avaliacoes <- function(debug){
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
  tabela_categoria = 'avaliacoes'

  ### Tempo
  if(!teste){
    ##Teste se estou gerando via rstudio (knit)
    if(as.integer(params$num_dias) == 0) {
      ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
      ano_atual = fct_ano_atual()
      ####Variavel global c/ mês atual (para comparação)
      mes_atual = fct_mes_atual()
      ## Apenas ano, para gerar títulos
      ano <- year(ano_atual)
      ##Execução normal, recebendo data do gerador de dashs
    }else{
      data <- (lubridate::today()-lubridate::days(params$num_dias))
      ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
      ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
      ####Variavel global c/ mês atual (para comparação)
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

  ##-> Collect cria o dataframe resultado da query, negocio_usado será a tabela na qual estou lendo (FROM cliente)
  negocio_usado <- fread("Tabelas/negocio_usado.csv", colClasses = c(nu_id = "character", nu_produto_id = "character", nu_negocio_id = "character")) %>%
    dplyr::select(nu_id, nu_negocio_id, nu_produto_id, nu_valor, nu_estado, nu_dt_cadastro, nu_excluido, nu_ativo) %>%
    dplyr::filter(nu_excluido == FALSE, nu_ativo == TRUE)

  ## Coleta todos os negocios (para puxar negocio_vendedor_id)
  negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_cliente_id = "character")) %>%
    dplyr::select(negocio_id, negocio_vendedor_id, negocio_cliente_id)

  ## Junção de negócio com negócio uasdo
  negocio_usado_v <- inner_join(negocio_usado, negocio, by=c("nu_negocio_id" = "negocio_id"))

  ## Coleta todos os produtos
  produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character",
                                                         produto_marca_id = "character", produto_categoria_id = "character")) %>%
    dplyr::select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

  #Arrumando encoding
  Encoding(produto$produto_nome) <- 'latin1' # 'UTF-8'

  neg_us_v_ij_prod <- inner_join(negocio_usado_v, produto, by=c("nu_produto_id"="produto_id")) %>%
    dplyr::select(nu_id, nu_negocio_id, negocio_vendedor_id, negocio_cliente_id, nu_produto_id, nu_valor, nu_estado, nu_dt_cadastro,
                  produto_nome, produto_marca_id)

  ## Coleta todos os clientes
  cliente <- fread("Tabelas/cliente.csv") %>%
    dplyr::select(cliente_id, cliente_nome, cliente_empresa_id)

  #Arrumando encoding
  Encoding(cliente$cliente_nome) <- 'latin1' # 'UTF-8''

  neg_us_v_ij_prod_ij_cli <- inner_join(neg_us_v_ij_prod, cliente, by=c("negocio_cliente_id" = "cliente_id")) %>%
    dplyr::select(-nu_produto_id, -negocio_cliente_id)

  marcas <- fread("Tabelas/marca.csv", colClasses = c(marca_id = "character")) %>%
    dplyr::select(marca_id, marca_nome)

  #Arrumando encoding
  Encoding(marcas$marca_nome) <- 'latin1' # 'UTF-8'

  neg_v__prod__cli_ij_mar <- inner_join(neg_us_v_ij_prod_ij_cli, marcas, by=c("produto_marca_id" = "marca_id")) %>%
    dplyr::select(-produto_marca_id)

  ## Gera uma lista com códigos das empresas ativas
  empresas_ativas <- fct_empresas_ativas ()
  ## Gerando lista que salvará os vendedores (para retornar esta lista)
  vendedores <- NULL
  if(debug){
    length(empresas_ativas)
  }
  for(i in (1:length(empresas_ativas))){
    ## Inicializando a lista de vendedores
    if(debug){
      # i = 1
      print(i)
      print(empresas_ativas[[i]])
      # i = 10
    }
    ##coleta todos os vendedores
    vendedor <- fread("Tabelas/vendedor.csv") %>%
      dplyr::select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
      dplyr::filter (vendedor_empresa_id == empresas_ativas[[i]])

    vendedor_a <- vendedor %>%
      dplyr::filter (vendedor_ativo == T)

    #Arrumando encoding
    Encoding(vendedor_a$vendedor_nome) <- 'latin1' # 'UTF-8'
    vendedor_a$vendedor_nome <- func_nome(vendedor_a$vendedor_nome)

    if(empresas_ativas[[i]] == 16){
      vendedor_a$vendedor_nome[vendedor_a$vendedor_id == 723] <- "BRUNO PE.";
      vendedor_a$vendedor_nome[vendedor_a$vendedor_id == 812] <- "BRUNO PO.";
      vendedor_a$vendedor_nome[vendedor_a$vendedor_id == 942] <- "LUCAS V. I.";
    }


    ## negocio usado, produto, cliente, marcas (filtrando empresa para facilitar o join)
    neg_v__prod__cli__mar_empresa <- neg_v__prod__cli_ij_mar %>%
      dplyr::filter(cliente_empresa_id == empresas_ativas[[i]])

    ## junção com vendedor
    neg_v__prod__cli__mar__vend_empresa <- inner_join(neg_v__prod__cli__mar_empresa, vendedor_a, by=c("negocio_vendedor_id" = "vendedor_id")) %>%
      dplyr::select(-negocio_vendedor_id, vendedor_empresa_id, vendedor_ativo)

    ## criar tabela como deverá ser apresentada
    ## Cliente, vendedor, produtos, data, link no final

    ## Contando valores únicos
    ##length(unique(neg_v__prod__cli__mar__vend_empresa$nu_id))
    if(nrow(neg_v__prod__cli__mar__vend_empresa) > 0){
      if(debug){
        print("Debug ativo")
        print(paste0("Empresa = ", empresas_ativas[[i]]))
        print(paste0("Número de linhas = ", nrow(neg_v__prod__cli__mar__vend_empresa)))
      }
      ## Ordenando e removendo colunas repetidas
      tabela_final_avaliacoes <- neg_v__prod__cli__mar__vend_empresa %>%
        group_by(nu_negocio_id, produto_nome, nu_valor) %>%
        slice(c(which.max(nu_dt_cadastro))) %>%
        ungroup ()

      tabela_final_avaliacoes <- tabela_final_avaliacoes %>%
        dplyr::group_by(nu_negocio_id) %>%
        ## Deixarei o mutate pra valor, caso precise futuramente
        # dplyr::mutate (valor_negocio = func_fmt_din(sum(nu_valor))) %>%
        dplyr::mutate (Produto = paste0(produto_nome, collapse = " --- ")) %>%
        ## Após agrupamento e adição de nova coluna com nome de todos os produtos, ficar apenas com uma das linhas
        dplyr::slice(c(which.max(nu_id))) %>%
        dplyr::ungroup () %>%

        ##dplyr::select(nu_id, nu_valor, valor_negocio) ## Usado para verificar se todas as linhas foram convertidas corretamente
        ## formato o link
        ##  onclick="window.open('http://google.com', '_blank')">LINK DA AVALIACAO
        dplyr::mutate(Link = paste0('onclick="', "window.open('", paste0("https://letmegooglethat.com/?q=", nu_dt_cadastro, "'"), ", '_blank')", '"', '>LINK DA AVALIAÇÃO')) %>%
        ## formato a data para impressão
        dplyr::mutate ('Data de Cadastro' = func_fmt_data_d_m_Y(nu_dt_cadastro)) %>%

        ## Organizando tabela
        dplyr::arrange(desc(nu_dt_cadastro)) %>%

        ## Selecionando colunas e alterando nomes
        dplyr::select(cliente_nome, vendedor_nome, Produto , 'Data de Cadastro', Link) %>%
        dplyr::rename(Vendedor = vendedor_nome, Cliente = cliente_nome)

      # Encoding(tabela_final_avaliacoes$Cliente) <- 'latin1'
      # Encoding(tabela_final_avaliacoes$Vendedor) <- 'latin1'
      # Encoding(tabela_final_avaliacoes$'Produto + Valor') <- 'latin1'

      if(debug){
        print("Arquivo Existe, será gerado o html:");
        print(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[[i]] , '.html'))
      }

      # tabela_csv <- tabela_csv %>%
      #   mutate_if(is.character,fix_encoding)
      if(nrow(tabela_final_avaliacoes) > 0){
        vendedores[i] <- tabela_final_avaliacoes %>%
          dplyr::select(Vendedor) %>%
          dplyr::distinct(Vendedor) %>%
          dplyr::arrange(Vendedor)
      }else{
        vendedores[i] <- NULL
      }

      library(tableHTML)
      ## imprime em HTML (para facilitar modificação)
      tableHTML::write_tableHTML(tableHTML::tableHTML(tabela_final_avaliacoes), file = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[[i]] , '.html'))

    }else{
      if(debug){
        print(paste0("Empresa não tem funcionalidade  = ", empresas_ativas[[i]]))
        print(paste0("Número de linhas = ", nrow(tabela_final_avaliacoes)))
      }
    }
  }
  ##########################################################
  ## Retornando a lista (após rodar o for de todas as empresas) com os vendedores de cada empresa
  return(vendedores)
}

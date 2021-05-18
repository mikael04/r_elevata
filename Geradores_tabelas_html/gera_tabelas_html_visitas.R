fct_gera_tabelas_visitas <- function(debug){
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
  tabela_categoria = 'visitas'

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

  ##Collect cria o df resultado da query, nesse caso, visitas_cliente, já filtrando apenas ano atual
  visita_cliente <- fread("Tabelas/visita_cliente_obs.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
    select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro, vc_observacao) %>%
    filter (vc_data_cadastro >= ano_atual, vc_data_cadastro < (ano_atual+years(1)))

  #Arrumando encoding
  Encoding(visita_cliente$vc_observacao) <- 'latin1'

  ##Pra pegar o nome do resultado
  visita_resultado <- fread("Tabelas/visita_resultado.csv") %>%
    select(vr_id, vr_nome, vr_ativo) %>%
    filter(vr_ativo == 1) %>% ##Ja sao todos ativos
    select(-vr_ativo) %>%
    rename (resultado = vr_nome)

  ##Pra filtrar os resultados da empresa
  visita_resultado_empresa <- fread("Tabelas/visita_resultado_empresa.csv") %>%
    select(vre_resultado_id, vre_empresa_id, vre_ativo) %>%
    filter(vre_ativo == 1) %>% ##Ja sao todos ativos mas mantive pra manter o filtro, ja filtro a empresa
    select(-vre_ativo)
  #Arrumando encoding
  Encoding(visita_resultado$resultado) <- 'latin1'

  ##Pra pegar o nome do status
  visita_status <- fread("Tabelas/visita_status.csv") %>%
    select(vs_id, vs_nome, vs_ativo) %>%
    filter(vs_ativo == 1) %>%
    select(-vs_ativo) %>%
    rename (motivo = vs_nome)

  ##Pra filtrar os status da empresa
  visita_status_empresa <- fread("Tabelas/visita_status_empresa.csv") %>%
    select(vse_status_id, vse_empresa_id, vse_ativo) %>%
    filter(vse_ativo == 1) %>%
    select(-vse_ativo)

  #Arrumando encoding
  Encoding(visita_status$motivo) <- 'latin1'

  ##coleta todos os clientes
  cliente <- fread("Tabelas/cliente.csv") %>%
    dplyr::select(cliente_id, cliente_nome, cliente_empresa_id)

  #Arrumando encoding
  Encoding(cliente$cliente_nome) <- 'latin1' #'UTF-8'

  ## Inicializando a lista de vendedores
  vendedores <- NULL
  empresas_ativas <- fct_empresas_ativas ()
  for(i in (1:length(empresas_ativas))){
    if(debug){
      # i = 33
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

    ##junta as duas tabelas (resultado e resultado_empresa) pra pegar o id da empresa (vre_empresa_id) e o nome do resultado (vr_nome)
    vis_res_emp <- inner_join(visita_resultado, visita_resultado_empresa, by = c('vr_id'= 'vre_resultado_id')) %>%
      dplyr::filter(vre_empresa_id == empresas_ativas[[i]])
    ##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
    vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id')) %>%
      dplyr::filter(vse_empresa_id == empresas_ativas[[i]])

    vis_res_st_emp <- inner_join(vis_res_emp, vis_st_emp, by = c('vre_empresa_id' = 'vse_empresa_id'))

    ##Juntando visita_cliente com os resultados
    vc_ij_vre_vse <- inner_join(visita_cliente, vis_res_st_emp, by = c('vc_resultado_id' = 'vr_id', 'vc_status_id' = 'vs_id')) %>%
      dplyr::select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro, resultado, vc_observacao, motivo)

    ## Juntando com vendedores
    vc_ij_vre_vse_v <- inner_join(vc_ij_vre_vse, vendedor_a, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
      dplyr::select(-vendedor_ativo)

    ## Filtrando apenas clientes desta empresa
    cliente_emp <- cliente %>%
      dplyr::filter(cliente_empresa_id == empresas_ativas[[i]])

    ## Juntando com clientes
    vc_ij_vre_vse_v_c <- inner_join(vc_ij_vre_vse_v, cliente_emp, by = c('vc_cliente_id' = 'cliente_id'))

    if(nrow(vc_ij_vre_vse_v_c) > 0){
      if(debug){
        print("Debug ativo")
        print(paste0("Empresa = ", empresas_ativas[[i]]))
        print(paste0("Número de linhas = ", nrow(vc_ij_vre_vse_v_c)))
      }

      ###################################################################################
      ## Criação das tabelas
      ######################
      ## joins e filtros adicionais
      #############################

      ## Pego a tabela e já seleciono apenas as colunas que utilizarei
      tabela_final_avaliacoes <- vc_ij_vre_vse_v_c %>%
        dplyr::select(vendedor_nome, cliente_nome, resultado, motivo, vc_data_cadastro, vc_observacao)

      tabela_final_avaliacoes <- tabela_final_avaliacoes %>%
        ## formato a data para impressão
        dplyr::mutate ('Data de Cadastro' = func_fmt_data_d_m_Y(vc_data_cadastro)) %>%
        ## Organizando tabela
        dplyr::arrange(desc(vc_data_cadastro)) %>%

        ## Selecionando colunas e alterando nomes
        dplyr::rename(Cliente = cliente_nome, Vendedor = vendedor_nome, Motivo = motivo, Resultado = resultado,
                      'Observação' = vc_observacao) %>%
        ## Ordena e seleciona colunas que serão mostradas
        dplyr::select(Cliente, Vendedor, Motivo, Resultado, 'Observação' , 'Data de Cadastro', -vc_data_cadastro)

      #Encoding(prop_ate_1ano_ant$Cliente) <- 'latin1'
      #Encoding(prop_ate_1ano_ant$Produtos) <- 'latin1'

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
        print(paste0("Número de linhas = ", nrow(vc_ij_vre_vse_v_c)))
      }
    }
  }
  ##########################################################
  ## Retornando a lista (após rodar o for de todas as empresas) com os vendedores de cada empresa
  return(vendedores)
}

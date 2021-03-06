rm(list = ls())
#Lib q ser? futuramente usada pros pain?is interativos
#library(shiny)
#Lib pra conex?o com o banco
#library(odbc)
#Lib para ler (mais rapidamente) os csvs
library(data.table)
#lib com uma cacetada de outras libs para manipular dados
#library(tidyverse)
#Libs pra trabalhar com a base (cortes e fun??es similares ao SQL)
library(dplyr) #Contido no tidyverse
#lib pros gr?ficos mais "interativos"
library(plotly)
#library(htmlwidgets)
#Lib pra usar paletas de cores
library(RColorBrewer)
#library(viridis)
#Lib usada pros treemaps
#library(treemap)
#Lib usada pros waffles
#library(waffle)
#usada para converter n?meros em moeda
#library(scales)
#Lib para lidar com o tempo
library(lubridate)
source("fct_tempo.R")
source("fct_fmt_din.R")
source("fct_fmt_nome.R")


###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = T
####Variável usada para não plotar os gráficos na dash
dash = F
####Variável para testar dias anteriores
num_dias <- 0
if(!teste){
  ##Teste se estou gerando via rstudio (knit)
  if(as.integer(num_dias) == 0) {
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual = fct_ano_atual()
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = fct_mes_atual()
    ## Apenas ano, para gerar títulos
    ano <- year(ano_atual)
    ##Execução normal, recebendo data do gerador de dashs
  }else{
    data <- (lubridate::today()-lubridate::days(num_dias))
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
    ## Apenas ano, para gerar títulos
    ano <- lubridate::year(ano_atual)
  }
}else{
  ##Teste setando dia
  data <- lubridate::ymd("2020-12-31")
  ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
  ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
  ####Variavel global c/ mês atual (para comparação)
  mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
  ## Apenas ano, para gerar títulos
  ano <- lubridate::year(ano_atual)
}

##Teste, senão tiver parâmetro, estou fazendo o teste e entra no if, senão vai pro else
####Vari?vel global para ver se tem usados Ainda n?o usada
#usados = T

##plotando texto sem informações #usado para gráficos que não tiverem nenhuma informação no período
#caminho para imagem de sem dados
s_dados_path <- "s_dados.png"

#empresa = params$variable1
#teste
empresa = 86
vend_id = 1165
params <- NULL
params$dash_vend = F
params$dash_mob = F
###################################

#################################################################################
##Começando script visitas_clientes

##Collect cria o df resultado da query, nesse caso, visitas_cliente, já filtrando apenas ano atual
visita_cliente <- fread("Tabelas/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  dplyr::select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro) %>%
  dplyr::filter (vc_data_cadastro >= ano_atual, vc_data_cadastro < (ano_atual+years(1)))

##Pra pegar o nome do resultado
visita_resultado <- fread("Tabelas/visita_resultado.csv") %>%
  dplyr::select(vr_id, vr_nome, vr_ativo) %>%
  dplyr::filter(vr_ativo == 1) %>% ##Ja sao todos ativos
  dplyr::select(-vr_ativo) %>%
  dplyr::rename (resultado = vr_nome)

##Pra filtrar os resultados da empresa
visita_resultado_empresa <- fread("Tabelas/visita_resultado_empresa.csv") %>%
  dplyr::select(vre_resultado_id, vre_empresa_id, vre_ativo) %>%
  dplyr::filter(vre_ativo == 1, vre_empresa_id == empresa) %>% ##Ja sao todos ativos mas mantive pra manter o filtro, ja filtro a empresa
  dplyr::select(-vre_ativo)
#Arrumando encoding
Encoding(visita_resultado$resultado) <- 'latin1'

##junta as duas tabelas (resultado e resultado_empresa) pra pegar o id da empresa (vre_empresa_id) e o nome do resultado (vr_nome)
vis_res_emp <- inner_join(visita_resultado, visita_resultado_empresa, by = c('vr_id'= 'vre_resultado_id'))

##Pra pegar o nome do status
visita_status <- fread("Tabelas/visita_status.csv") %>%
  dplyr::select(vs_id, vs_nome, vs_ativo) %>%
  dplyr::filter(vs_ativo == 1) %>%
  dplyr::select(-vs_ativo) %>%
  dplyr::rename (motivo = vs_nome)

##Pra filtrar os status da empresa
visita_status_empresa <- fread("Tabelas/visita_status_empresa.csv") %>%
  dplyr::select(vse_status_id, vse_empresa_id, vse_ativo) %>%
  dplyr::filter(vse_ativo == 1, vse_empresa_id == empresa) %>%
  dplyr::select(-vse_ativo)

#Arrumando encoding
Encoding(visita_status$motivo) <- 'latin1'

##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
  dplyr::select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  dplyr::filter (vendedor_empresa_id == empresa)

## Filtro para dash do vendedor
if(params$dash_vend){
  vendedor <- vendedor %>%
    dplyr::filter(vendedor_id == vend_id)
}

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- sapply(vendedor$vendedor_nome, func_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

vendedor_a <- vendedor %>%
  dplyr::filter(vendedor_ativo == T)


##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id'))

##Juntando visita_cliente com os resultados
vc_ij_vre <- inner_join(visita_cliente, vis_res_emp, by = c('vc_resultado_id' = 'vr_id')) %>%
  dplyr::select (vc_id, vc_vendedor_id, vc_resultado_id, resultado)
##Juntando visita_cliente com os motivos (status)
vc_ij_vse <- inner_join(visita_cliente, vis_st_emp, by = c('vc_status_id' = 'vs_id'))%>%
  dplyr::select (vc_id, vc_vendedor_id, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vse_ij_v <- inner_join(vc_ij_vse, vendedor, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  dplyr::select (vc_id, vc_vendedor_id, vendedor_nome, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vre_ij_v <- inner_join(vc_ij_vre, vendedor, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  dplyr::select (vc_id, vc_vendedor_id, vendedor_nome, vc_resultado_id, resultado)

##Agrupando por vendedor e por motivo (vc_status_id, depois mostrar so motivo)
vc_ij_vse_ij_v_ag <- vc_ij_vse_ij_v %>%
  dplyr::group_by(vc_vendedor_id, vc_status_id) %>%
  dplyr::mutate(motivo_n = n()) %>%
  dplyr::distinct(vc_vendedor_id, .keep_all = T) %>%
  dplyr::ungroup()

n_vend <- vc_ij_vse_ij_v_ag %>%
  count(vendedor_nome)

n_vend <- nrow(n_vend)

### Gráfico v0 - Motivo das visitas (status) por vendedor em anat
if(nrow(vc_ij_vse_ij_v) > 0){
  ##descobrindo numero de status diferentes para criar paleta de cores
  n_m_color <- dplyr::n_distinct(vc_ij_vse_ij_v_ag$vc_status_id)
  if (n_m_color < 3){
    if (n_m_color <2){
      brbg_mot <- '#9ACD32'
    }else{
      brbg_mot <- c('#9ACD32', '#018571')
    }
  } else{
    if(n_m_color > 11){
      brbg_mot <- colorRampPalette(brewer.pal(name="Paired", n = 8)) (n_m_color)
    }else{
      brbg_mot <- brewer.pal(n_m_color,'Paired')
    }
  }

  axis_h <- list(
    title = "")
  v0 <- plot_ly(vc_ij_vse_ij_v_ag, type = "bar", orientation = 'h', x = ~motivo_n, y = ~reorder(vendedor_nome, desc(vendedor_nome)), color = ~motivo,
                colors = brbg_mot,
                name = ~motivo)
  if(n_vend > 15){
    v0 <- v0 %>%
      layout(barmode = ifelse(params$dash_vend, 'group', 'stack'),
             xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
             yaxis = list(title = ''),
             legend = list(x = 100, y = 0.5))
  }else{
    v0 <- v0 %>%
      layout(barmode = ifelse(params$dash_vend, 'group', 'stack'),
             xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
             yaxis = list(title = ''),
             legend = list(orientation = 'h'))
  }
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  v0 <- include_graphics(s_dados_path)
}
if(dash == F){
  v0
}


##Agrupando por vendedor e por resultado (vc_status_id, depois mostrar so motivo)
vc_ij_vre_ij_v_ag <- vc_ij_vre_ij_v %>%
  dplyr::group_by(vc_vendedor_id, vc_resultado_id) %>%
  dplyr::mutate(resultado_n = n()) %>%
  dplyr::distinct(vc_vendedor_id, .keep_all = T) %>%
  dplyr::ungroup()


### Gráfico v1 - Resultado das visitas (resultados) por vendedor em anat
if(nrow(vc_ij_vre_ij_v) > 0){
  ##descobrindo numero de status diferentes para criar paleta de cores
  n_r_color <- dplyr::n_distinct(vc_ij_vre_ij_v_ag$vc_resultado_id)
  if (n_r_color < 3){
    if (n_r_color <2){
      brbg_res <- '#9ACD32'
    }else{
      brbg_res <- c('#9ACD32', '#018571')
    }
  } else{
    if(n_r_color > 11){
      brbg_res <- c(brewer.pal(name="Paired", n = 8), brewer.pal(name="BrBG", n = 8))
    }else{
      brbg_res <- brewer.pal(n_r_color,'Paired')
    }
  }

  v1 <- plot_ly(vc_ij_vre_ij_v_ag, type = "bar", orientation = 'h', x = ~resultado_n, y = ~reorder(vendedor_nome, desc(vendedor_nome)), color = ~resultado,
                colors = brbg_res,
                name = ~resultado)
  if(n_vend > 15){
    v1 <- v1 %>%
      layout(barmode = ifelse(params$dash_vend, 'group', 'stack'),
             xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
             yaxis = list(title = ''),
             legend = list(x = 150, y = 0.5))
  }else{
    v1 <- v1 %>%
      layout(barmode = ifelse(params$dash_vend, 'group', 'stack'),
             xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
             yaxis = list(title = ''),
             legend = list(orientation = 'h'))
  }
  if(teste == F){
    #tabelas
    rm(visita_resultado_empresa, vis_res_emp, vc_ij_vre, vc_ij_vse,
       ##vis_st_emp, vc_ij_vse_ij_v, ##vou usar a vis_st_emp para uma junção (saber qual empresa são feitas as visitas)
       vc_ij_vre_ij_v, vc_ij_vre_ij_v_ag, visita_resultado,axis_h);
    #variáveis
    rm(brbg_mot, brbg_res, n_r_color, n_m_color);
  }
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  v1 <- include_graphics(s_dados_path)
}
if(dash == F){
  v1
}

################################################################################################
################################################################################################
### Tabelas e graficos de Clientes

cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  dplyr::select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_ultima_visita) %>%
  dplyr::filter(cliente_empresa_id == empresa)
## Filtro para dash do vendedor
if(params$dash_vend){
  cliente <- cliente %>%
    dplyr::filter(cliente_vendedor_id == vend_id)
}

##Clientes cadastrados por vendedor
cli_p_v <- cliente %>%
  dplyr::group_by(cliente_vendedor_id) %>%
  dplyr::mutate(n_clientes = n()) %>%
  dplyr::distinct(cliente_vendedor_id, .keep_all = T) %>%
  dplyr::select(cliente_vendedor_id, n_clientes) %>%
  dplyr::arrange(cliente_vendedor_id) %>%
  dplyr::ungroup()

##Juncao pra pegar nome do vendedor
cli_p_v_ij_vend <- inner_join(cli_p_v, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
  dplyr::select(cliente_vendedor_id, vendedor_nome, n_clientes)


##Já é filtrado em anat (filtra na tabela de visitas_cliente)
vc_ij_vse_ij_v_count <- vc_ij_vse_ij_v_ag %>%
  dplyr::select (vc_vendedor_id, motivo_n) %>% ##Não vou salvar o nome pq vou fazer uma junção com a outra tabela, então só preciso do id
  dplyr::group_by(vc_vendedor_id) %>%
  dplyr::mutate(n_visitas = sum(motivo_n)) %>%
  dplyr::distinct(vc_vendedor_id, .keep_all = T) %>%
  dplyr::ungroup ()

##Distribuição de clientes total, visita e negócios em anat por vendedor
vend_cli_vis <- left_join(cli_p_v_ij_vend, vc_ij_vse_ij_v_count, by = c("cliente_vendedor_id" = "vc_vendedor_id")) %>%
  dplyr::select (cliente_vendedor_id, vendedor_nome, n_clientes, n_visitas)

##Contar quantos negócios são feitos por vendedor em anat
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character", negocio_cliente_id = "character")) %>%
  dplyr::select(negocio_id, negocio_vendedor_id, negocio_cliente_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_produto_id)

####################################
###Clientes que tiveram negócios, quantas visitas recebem (histograma) e clientes que não tiveram negócios, quantas visitas recebem
####################################

##Contar quantos clientes aparecem em negócio
cli_ij_ng <- inner_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  dplyr::select(cliente_id, negocio_id)

#cli_ij_ng <- já tenho, clientes que já possuem negócios
##Clientes sem negócio
cli_s_neg <- anti_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  dplyr::select(cliente_id) %>%
  dplyr::group_by(cliente_id) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(cliente_id) %>%
  dplyr::mutate (neg = F)
##Clientes com negócio
cli_c_neg <- cli_ij_ng %>%
  dplyr::select (cliente_id) %>%
  dplyr::group_by(cliente_id) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(cliente_id) %>%
  dplyr::mutate (neg = T)

cli_s_neg_ij_vis <- inner_join(cli_s_neg, visita_cliente, by = c("cliente_id" = "vc_cliente_id"))
cli_c_neg_ij_vis <- inner_join(cli_c_neg, visita_cliente, by = c("cliente_id" = "vc_cliente_id"))

##Contanto visitas por cliente, dos que não tem negócios
cli_s_neg_ij_vis_cont <- cli_s_neg_ij_vis %>%
  dplyr::group_by(cliente_id) %>%
  count(name = "num_visitas") %>%
  dplyr::ungroup()

##Contanto visitas por cliente, dos que tem negócios
cli_c_neg_ij_vis_cont <- cli_c_neg_ij_vis %>%
  dplyr::group_by(cliente_id) %>%
  count(name = "num_visitas") %>%
  dplyr::ungroup()

### Histogramas
if(nrow(cli_c_neg_ij_vis_cont) > 0){

  ### Gráfico c5 - Histograma de distirbuição clientes por visitas, clientes sem negócios (clientes com visitas em anat)
  c5 <- plot_ly() %>%
    add_histogram(data =  cli_s_neg_ij_vis_cont,
                  x = ~num_visitas,
                  name = "",
                  nbinsx = max(cli_s_neg_ij_vis_cont$num_visitas),
                  hovertemplate = paste ("Existem %{y} clientes com %{x} visitas <br>",
                                         "<br>"),
                  marker = list(color = "darkorange"),
                  opacity = 0.6) %>%
    layout (xaxis = list(title = 'Intervalos'),
            yaxis = list(title = 'Número de clientes'))

  if(dash == F){
    c5
  }
  ### Gráfico c4 - Histograma de distirbuição clientes por visitas, clientes com negócios (clientes com visitas em anat)
  c4 <- plot_ly() %>%
    add_histogram(data =  cli_c_neg_ij_vis_cont,
                  x = ~num_visitas,
                  name = "",
                  nbinsx = max(cli_c_neg_ij_vis_cont$num_visitas),
                  hovertemplate = paste ("Existem %{y} clientes com %{x} visitas <br>",
                                         "<br>"),
                  marker = list(color = "blue"),
                  opacity = 0.6) %>%
    layout (xaxis = list(title = 'Intervalos'),
            yaxis = list(title = 'Número de clientes'))

  if(dash == F){
    c4
  }

  ### Gráfico c4_c5 - Ambos histogramas anteriores (c4 e c5)
  c4_c5 <- subplot(c4, c5) %>%
    layout(legend = list(x=0.7, y=0.9),
           title = "Clientes por intervalo",
           xaxis = list(title = ''),
           yaxis = list(title = 'Distribuição de clientes'))

  if(teste == F){
    #tabelas
    rm(visita_cliente, cliente, negocio, cli_ij_ng, cli_c_neg_ij_vis_cont, cli_s_neg_ij_vis_cont,
       cli_c_neg_ij_vis, cli_s_neg_ij_vis, cli_c_neg, cli_s_neg);
    #variáveis
    rm();
  }

  if(dash == F){
    c4_c5
  }
}else{
  c4 <- include_graphics(s_dados_path)
  c5 <- include_graphics(s_dados_path)
  c4_c5 <- include_graphics(s_dados_path)
}




########################################################

#########################################################################################################
#########################################################################################################
##Começando scripts de mapas
#########################################################################################################
#########################################################################################################

##Distribuição de clientes, mostrando no label nome do cliente, nome do vendedor e última visita
cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  dplyr::select (cliente_id, cliente_nome, cliente_latitude, cliente_longitude, cliente_vendedor_id, cliente_empresa_id)
#Arrumando encoding
Encoding(cliente$cliente_nome) <- 'latin1'

visita_cliente <- fread("Tabelas/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  dplyr::select (vc_id, vc_vendedor_id, vc_cliente_id, vc_data_cadastro)

vendedor <- fread("Tabelas/vendedor.csv") %>%
  dplyr::select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  dplyr::filter(vendedor_ativo == 1, vendedor_empresa_id == empresa) %>%
  dplyr::select(-vendedor_ativo)

## Filtro para dash do vendedor
if(params$dash_vend){
  vendedor <- vendedor %>%
    dplyr::filter(vendedor_id == vend_id)
}

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- sapply(vendedor$vendedor_nome, func_nome)

##Alterando os que não possuem latitude/longitude para 0 (que será filtrado depois)
cliente$cliente_latitude[cliente$cliente_latitude == ''] <- '0'
cliente$cliente_longitude[cliente$cliente_longitude == ''] <- '0'
cliente$cliente_latitude[cliente$cliente_latitude == '-'] <- '0'
cliente$cliente_longitude[cliente$cliente_longitude == '-'] <- '0'

##Clientes da empresa correta ##Clientes com valor NA, valores 0 e valores positivos de latlong (hemisf norte, leste do globo) removidos
cliente_c_loc <- cliente %>%
  dplyr::filter (cliente_empresa_id == empresa, cliente_latitude < 0, cliente_longitude < 0) %>%
  dplyr::rename(lat = cliente_latitude, long = cliente_longitude) %>%
  dplyr::mutate(lat = as.numeric(lat), long = as.numeric(long)) %>%
  dplyr::filter (!is.null(lat), !is.na(lat))

##Serão feitas as junções apenas com clientes que possuem
##Aqui já vou filtrar só por vendedores ativos, fazer pra inativos depois
cli_in_ven <- inner_join(cliente_c_loc, vendedor, by = c('cliente_vendedor_id'='vendedor_id'))

##pegar apenas última visita por cliente
vcUltVis <- visita_cliente %>%
  dplyr::arrange(desc(vc_data_cadastro)) %>%
  dplyr::group_by(vc_cliente_id) %>%
  dplyr::distinct(vc_cliente_id, .keep_all = T) %>%
  dplyr::ungroup ()


##Categorias de idades
idades_vis = c("Sem visita", "Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses")
##Cores das categorias
cor_idade_vis = c("#000000", "#32CD32", "#87CEFA" , "#FFD700" , "orange" , "#DE0D26")

##left join pra manter todos, mesmo os q não tem visita
cli_in_ven_in_vcUltVis <- left_join(cli_in_ven, vcUltVis, by = c("cliente_id" = "vc_cliente_id")) %>%
  dplyr::select(cliente_id, cliente_nome, lat, long, cliente_vendedor_id, vendedor_nome, vc_vendedor_id, vc_data_cadastro) %>%
  dplyr::mutate (idade = (as.integer(today() - as_date(vc_data_cadastro)))) %>%
  dplyr::mutate (idade_vis = "Sem Visita") %>%
  dplyr::arrange (idade)

cli_in_ven_in_vcUltVis$idade[is.na(cli_in_ven_in_vcUltVis$idade)] <- -1

cli_in_ven_in_vcUltVis$idade_vis <- with(cli_in_ven_in_vcUltVis, cut(idade, breaks = c(-2,0,60, 180, 365, 730, 10000),
                                                                     label = idades_vis))

pal = colorFactor(palette = cor_idade_vis, domain = cli_in_ven_in_vcUltVis$idade_vis)

### Gráfico m0 de distribuição dos clientes por empresa
if(nrow(cli_in_ven_in_vcUltVis) > 0){
  m0 <- leaflet(cli_in_ven_in_vcUltVis) %>%
    addTiles() %>%
    addCircleMarkers(lat = ~lat, lng = ~long, weight = 1,
                     popup = paste0( "Nome do cliente:", cli_in_ven_in_vcUltVis$cliente_nome,
                                     "<br>",
                                     "Vendedor: ", cli_in_ven_in_vcUltVis$vendedor_nome,
                                     "<br>",
                                     "Data da última visita: ", cli_in_ven_in_vcUltVis$vc_data_cadastro),
                     label = ~cliente_nome,
                     color = ~pal(idade_vis)
                     #clusterOptions = markerClusterOptions()
    ) %>%
    addLegend("topright",
              value = ~idade_vis,
              pal = pal,
              title = 'Tempo desde a última visita',
              opacity = .8)
  if(teste == F){
    #tabelas
    rm(cliente, pal, cli_in_ven, visita_cliente, vendedor, vcUltVis);
    #var
  }
}else{
  m0 <- include_graphics(s_dados_m_path)
}
#######################################################################

#######################################################################
### Criando distribuição de tempo de visitas por status


##Categorias de idades #já declarado, só para facilitar visualização
#idades_vis = c("Sem visita", "Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses")
##Cores das categorias #já declarado, só para facilitar visualização
#cor_idade_vis = c("#000000", "#32CD32", "#87CEFA" , "#FFD700" , "orange" , "#DE0D26")
df_cores <- data.frame(idades_vis, cor_idade_vis)

cli_in_ven_in_vcUltVis_cont <- cli_in_ven_in_vcUltVis %>%
  dplyr::group_by(idade_vis) %>%
  dplyr::mutate(cont = n()) %>%
  dplyr::distinct(idade_vis, .keep_all = T) %>%
  dplyr::select (idade, idade_vis, cont) %>%
  dplyr::ungroup()

## Agrupando para receber as cores do df_cores
cli_in_ven_in_vcUltVis_cont <- left_join(cli_in_ven_in_vcUltVis_cont, df_cores, by = c('idade_vis' = 'idades_vis'))
## Ordenando em ordem alfabética (o color do plotly faz isso automaticamente mas se perde nas cores)
cli_in_ven_in_vcUltVis_cont <- cli_in_ven_in_vcUltVis_cont %>%
  dplyr::arrange(idade_vis)

if(nrow(cli_in_ven_in_vcUltVis_cont) > 0){
  c6 <- plot_ly(cli_in_ven_in_vcUltVis_cont, type = 'bar', orientation = 'v',
                x = ~reorder(idade_vis, idade),
                y = ~cont,
                color = ~idade_vis,
                colors = ~cor_idade_vis,
                showlegend = F
  ) %>%
    layout(xaxis = list(title = ''),
           yaxis = list(title = ''))
}else{
  c6 <- include_graphics(s_dados_path)
}
if(dash == F){
  c6
}
if(teste == F){
  #tabelas
  rm(cli_in_ven_in_vcUltVis_cont, cli_in_ven_in_vcUltVis);
  #var
  rm(cor_idade_vis, idades_vis)
}
#######################################################################

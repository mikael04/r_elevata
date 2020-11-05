rm(list = ls())
#Lib q será futuramente usada pros painéis interativos
#library(shiny)
#Lib pra conexão com o banco
#library(odbc)
#Lib para ler (mais rapidamente) os csvs
library(data.table)
#lib com uma cacetada de outras libs para manipular dados
library(tidyverse)
#Libs pra trabalhar com a base (cortes e funções similares ao SQL)
#library(dplyr) #Contido no tidyverse
#Lib pro gráfico
library(ggplot2)
#lib pros gráficos mais "interativos"
library(plotly)
library(highcharter)
#library(htmlwidgets)
#Lib pra usar paletas de cores
library(RColorBrewer)
#library(viridis)
#Lib usada pros treemaps
#library(treemap)
#Lib usada pros waffles
#library(waffle)
#usada para converter números em moeda
#library(scales)
#Lib para funções de tempo
library(lubridate)
#usada para converter números em moeda
library(leaflet)


###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = F
####Variável usada para não plotar os gráficos na dash
dash = F
####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
ano_atual = ymd(today()) - months(month(today())-1) - days(day(today())-1)
####Variavel global c/ mês atual (para comparação)

mes_atual = month(today())
####Variável global para ver se tem usados Ainda não usada
#usados = T

##Teste
  empresa = 27

###################################

##Alterar o nome completo pra primeiro nome mais iniciais dos sobrenomes
func_nome <- function (nome_comp)
{
  lista <- str_split_fixed(nome_comp, " ", 4)
  lista <- lista[, -4]
  lista[,3] = str_sub(lista[,3], 1, 1)
  lista[,2] = str_sub(lista[,2], 1, 1)
  lista[,2] = paste(lista[,2], '.', sep='')
  lista[,3] = paste(lista[,3], '.', sep='')
  lista[,2] <- gsub('^.$', '',lista[,2])
  lista[,3] <- gsub('^.$', '',lista[,3])
  lista[,1] <- paste(lista[,1], lista[,2], lista[,3], sep=' ')
  return (lista[,1])
}

##Alterar o número apresentado na forma americana (com vírgula) para a forma brasileira (com ponto), através da transformação em string
func_fmt_numbr <- function(inteiro)
{
  inteiro_br <- paste("", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_br)
}

#################################################################################
##Começando scripts de visitas e clientes
#################################################################################
##Começando script visitas_clientes

##Collect cria o df resultado da query, nesse caso, visitas_cliente, já filtrando apenas ano atual
visita_cliente <- fread("Tabelas/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro) %>%
  filter (vc_data_cadastro >= ano_atual)

##Pra pegar o nome do resultado
visita_resultado <- fread("Tabelas/visita_resultado.csv") %>%
  select(vr_id, vr_nome, vr_ativo) %>%
  filter(vr_ativo == 1) %>% ##Ja sao todos ativos
  select(-vr_ativo) %>%
  rename (resultado = vr_nome)

##Pra filtrar os resultados da empresa
visita_resultado_empresa <- fread("Tabelas/visita_resultado_empresa.csv") %>%
  select(vre_resultado_id, vre_empresa_id, vre_ativo) %>%
  filter(vre_ativo == 1, vre_empresa_id == empresa) %>% ##Ja sao todos ativos mas mantive pra manter o filtro, ja filtro a empresa
  select(-vre_ativo)
#Arrumando encoding
Encoding(visita_resultado$resultado) <- 'latin1'

##junta as duas tabelas (resultado e resultado_empresa) pra pegar o id da empresa (vre_empresa_id) e o nome do resultado (vr_nome)
vis_res_emp <- inner_join(visita_resultado, visita_resultado_empresa, by = c('vr_id'= 'vre_resultado_id'))

##Pra pegar o nome do status
visita_status <- fread("Tabelas/visita_status.csv") %>%
  select(vs_id, vs_nome, vs_ativo) %>%
  filter(vs_ativo == 1) %>%
  select(-vs_ativo) %>%
  rename (motivo = vs_nome)

##Pra filtrar os status da empresa
visita_status_empresa <- fread("Tabelas/visita_status_empresa.csv") %>%
  select(vse_status_id, vse_empresa_id, vse_ativo) %>%
  filter(vse_ativo == 1, vse_empresa_id == empresa) %>%
  select(-vse_ativo)

#Arrumando encoding
Encoding(visita_status$motivo) <- 'latin1'

##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter (vendedor_empresa_id == empresa, vendedor_ativo == 1) %>%
  select(-vendedor_ativo)
#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

##plotando texto sem informações #usado para gráficos que não tiverem nenhuma informação no período
text <- paste("Não há informações para o período")
s_dados <- ggplot() +
  annotate("text", x = 1, y = 6, size = 8, label = text) +
  theme_void()

##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id'))

##Juntando visita_cliente com os resultados
vc_ij_vre <- inner_join(visita_cliente, vis_res_emp, by = c('vc_resultado_id' = 'vr_id')) %>%
  select (vc_id, vc_vendedor_id, vc_resultado_id, resultado)
##Juntando visita_cliente com os motivos (status)
vc_ij_vse <- inner_join(visita_cliente, vis_st_emp, by = c('vc_status_id' = 'vs_id'))%>%
  select (vc_id, vc_vendedor_id, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vse_ij_v <- inner_join(vc_ij_vse, vendedor, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  select (vc_id, vc_vendedor_id, vendedor_nome, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vre_ij_v <- inner_join(vc_ij_vre, vendedor, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  select (vc_id, vc_vendedor_id, vendedor_nome, vc_resultado_id, resultado)

##Agrupando por vendedor e por motivo (vc_status_id, depois mostrar so motivo)
vc_ij_vse_ij_v <- vc_ij_vse_ij_v %>%
  group_by(vc_vendedor_id, vc_status_id) %>%
  mutate(motivo_n = n()) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup()

### Gráfico v0 - Motivo das visitas (status) por vendedor em 2020
if(nrow(vc_ij_vse_ij_v) > 0){
  ##descobrindo numero de status diferentes para criar paleta de cores
  n_m_color <- n_distinct(vc_ij_vse_ij_v$vc_status_id)
  if (n_m_color < 3){
    if (n_m_color <2){
      brbg_mot <- '#9ACD32'
    }else{
      brbg_mot <- c('#9ACD32', '#018571')
    }
  } else{
    if(n_m_color > 11){
      brbg_mot <- colorRampPalette(brewer.pal(name="Dark2", n = 8)) (n_m_color)
    }else{
      brbg_mot <- brewer.pal(n_m_color,'BrBG')
    }
  }
  
  axis_h <- list(
    title = "")
  v0 <- plot_ly(vc_ij_vse_ij_v, type = "bar", orientation = 'h', x = ~motivo_n, y = ~reorder(vendedor_nome, desc(vendedor_nome)), color = ~motivo,
                colors = brbg_mot,
                name = ~motivo)
  v0 <- v0 %>%
    layout(barmode = 'stack',
           xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
           yaxis = list(title = ''))
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  v0 <- s_dados
}
if(dash == F){
  v0
}


##Agrupando por vendedor e por resultado (vc_status_id, depois mostrar so motivo)
vc_ij_vre_ij_v_ag <- vc_ij_vre_ij_v %>%
  group_by(vc_vendedor_id, vc_resultado_id) %>%
  mutate(resultado_n = n()) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup()

### Gráfico v1 - Resultado das visitas (resultados) por vendedor em 2020
if(nrow(vc_ij_vse_ij_v) > 0){
  ##descobrindo numero de status diferentes para criar paleta de cores
  n_r_color <- n_distinct(vc_ij_vre_ij_v_ag$vc_resultado_id)
  if (n_m_color < 3){
    if (n_m_color <2){
      brbg_res <- '#9ACD32'
    }else{
      brbg_res <- c('#9ACD32', '#018571')
    }
  } else{
    if(n_r_color > 11){
      brbg_res <- c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="BrBG", n = 8))
    }else{
      brbg_res <- brewer.pal(n_m_color,'BrBG')
    }
  }
  
  v1 <- plot_ly(vc_ij_vre_ij_v_ag, type = "bar", orientation = 'h', x = ~resultado_n, y = ~reorder(vendedor_nome, desc(vendedor_nome)), color = ~resultado,
                colors = brbg_res,
                name = ~resultado)
  
  v1 <- v1 %>%
    layout(barmode = 'stack',
           xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
           yaxis = list(title = ''))
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
  v1 <- s_dados
}
if(dash == F){
  v1
}

################################################################################################
################################################################################################
### Tabelas e graficos de Clientes

cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_ultima_visita) %>%
  filter(cliente_empresa_id == empresa)

##Clientes cadastrados por vendedor
cli_p_v <- cliente %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes) %>%
  arrange(cliente_vendedor_id) %>%
  ungroup()

##Juncao pra pegar nome do vendedor
cli_p_v_ij_vend <- inner_join(cli_p_v, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
  select(cliente_vendedor_id, vendedor_nome, n_clientes)


##Já é filtrado em 2020 (filtra na tabela de visitas_cliente)
vc_ij_vse_ij_v_count <- vc_ij_vse_ij_v %>%
  select (vc_vendedor_id, motivo_n) %>% ##Não vou salvar o nome pq vou fazer uma junção com a outra tabela, então só preciso do id
  group_by(vc_vendedor_id) %>%
  mutate(n_visitas = sum(motivo_n)) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup ()

##Distribuição de clientes total, visita e negócios em 2020 por vendedor
vend_cli_vis <- left_join(cli_p_v_ij_vend, vc_ij_vse_ij_v_count, by = c("cliente_vendedor_id" = "vc_vendedor_id")) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_visitas)

##Contar quantos negócios são feitos por vendedor em 2020
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character", negocio_cliente_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_cliente_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_produto_id)

##Juncao pra pegar a empresa do negócio (e vendedor)
neg_ij_vend <- inner_join(negocio, vendedor, by = c('negocio_vendedor_id' = 'vendedor_id')) %>%
  select(negocio_id, negocio_vendedor_id)


####################################
###Clientes que tiveram negócios, quantas visitas recebem (histograma) e clientes que não tiveram negócios, quantas visitas recebem
####################################

### Gráficos de pizza de clientes com e sem negócio
##Contando número de clientes geral e em 2020
n_clientes <- nrow(cliente)
##Clientes cadastrados em 2020 por vendedor
cliente_2020 <- cliente %>%
  filter(cliente_data_cadastro >= ano_atual)
n_clientes_2020 <- nrow(cliente_2020)

##Contar quantos clientes aparecem em negócio
cli_ij_ng <- inner_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  select(cliente_id, negocio_id)

#cli_ij_ng <- já tenho, clientes que já possuem negócios
##Clientes sem negócio
cli_s_neg <- anti_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  select(cliente_id) %>%
  mutate (neg = F)
##Clientes com negócio
cli_c_neg <- cli_ij_ng %>%
  select (cliente_id, negocio_id) %>%
  ungroup() %>%
  mutate (neg = T)

cli_s_neg_ij_vis <- inner_join(cli_s_neg, visita_cliente, by = c("cliente_id" = "vc_cliente_id"))
cli_c_neg_ij_vis <- inner_join(cli_c_neg, visita_cliente, by = c("cliente_id" = "vc_cliente_id"))

##Contanto visitas por cliente, dos que não tem negócios
cli_s_neg_ij_vis_cont <- cli_s_neg_ij_vis %>%
  group_by(cliente_id) %>%
  count(name = "num_visitas") %>%
  ungroup()

##Contanto visitas por cliente, dos que tem negócios
cli_c_neg_ij_vis_cont <- cli_c_neg_ij_vis %>%
  group_by(cliente_id, negocio_id) %>%
  count(name = "num_visitas") %>%
  ungroup()


### Gráfico c4 - Histograma de distirbuição clientes por visitas, clientes com negócios
if(nrow(cli_c_neg_ij_vis_cont > 0)){
  c4 <- plot_ly() %>%
    add_histogram(data =  cli_c_neg_ij_vis_cont, x = ~num_visitas, name = "Intervalo de visitas, clientes COM negócios", nbinsx = max(cli_c_neg_ij_vis_cont$num_visitas),
                  marker = list(color = "blue"), opacity = 0.6) %>%
    layout (xaxis = list(title = 'Intervalos'),
            yaxis = list(title = 'Número de clientes'))
  
  if(dash == F){
    c4
  }
  
  ### Gráfico c5 - Histograma de distirbuição clientes por visitas, clientes sem negócios
  c5 <- plot_ly() %>%
    add_histogram(data =  cli_s_neg_ij_vis_cont, x = ~num_visitas, name = "Intervalo de visitas, clientes SEM negócios", nbinsx = max(cli_c_neg_ij_vis_cont$num_visitas),
                  marker = list(color = "darkorange"), opacity = 0.6) %>%
    layout (xaxis = list(title = 'Intervalos'),
            yaxis = list(title = 'Número de clientes'))
  
  if(dash == F){
    c5
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
  c4 <- s_dados
  c5 <- s_dados
  c4_c5 <- s_dados
}




########################################################

#########################################################################################################
#########################################################################################################
##Começando scripts de mapas
#########################################################################################################
#########################################################################################################

##Distribuição de clientes, mostrando no label nome do cliente, nome do vendedor e última visita
cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_nome, cliente_latitude, cliente_longitude, cliente_vendedor_id, cliente_empresa_id)
#Arrumando encoding
Encoding(cliente$cliente_nome) <- 'latin1'

visita_cliente <- fread("Tabelas/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_data_cadastro)

vendedor <- fread("Tabelas/vendedor.csv") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter(vendedor_ativo == 1, vendedor_empresa_id == empresa) %>%
  select(-vendedor_ativo)

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

##Alterando os que não possuem latitude/longitude para 0 (que será filtrado depois)
cliente$cliente_latitude[cliente$cliente_latitude == ''] <- '0'
cliente$cliente_longitude[cliente$cliente_longitude == ''] <- '0'
cliente$cliente_latitude[cliente$cliente_latitude == '-'] <- '0'
cliente$cliente_longitude[cliente$cliente_longitude == '-'] <- '0'

##Clientes da empresa correta ##Clientes com valor NA, valores 0 e valores positivos de latlong (hemisf norte, leste do globo) removidos
cliente_c_loc <- cliente %>%
  filter (cliente_empresa_id == empresa, cliente_latitude < 0, cliente_longitude < 0) %>%
  rename(lat = cliente_latitude, long = cliente_longitude) %>%
  mutate(lat = as.numeric(lat), long = as.numeric(long)) %>%
  filter (!is.null(lat), !is.na(lat))

##Serão feitas as junções apenas com clientes que possuem
##Aqui já vou filtrar só por vendedores ativos, fazer pra inativos depois
cli_in_ven <- inner_join(cliente_c_loc, vendedor, by = c('cliente_vendedor_id'='vendedor_id'))

##pegar apenas última visita por cliente
vcUltVis <- visita_cliente %>%
  arrange(desc(vc_data_cadastro)) %>%
  group_by(vc_cliente_id) %>%
  distinct(vc_cliente_id, .keep_all = T) %>%
  ungroup ()


##Categorias de idades
idades_vis = c("Sem visita", "Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses")
##Cores das categorias
cor_idade_vis = c("#000000", "#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26")

##left join pra manter todos, mesmo os q não tem visita
cli_in_ven_in_vcUltVis <- left_join(cli_in_ven, vcUltVis, by = c("cliente_id" = "vc_cliente_id")) %>%
  select(cliente_id, cliente_nome, lat, long, cliente_vendedor_id, vendedor_nome, vc_vendedor_id, vc_data_cadastro) %>%
  mutate (idade = (as.integer(today() - as_date(vc_data_cadastro)))) %>%
  mutate (idade_vis = "Sem Visita") %>%
  arrange (idade)

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
                                     "Tempo desde a última visita: ", cli_in_ven_in_vcUltVis$vc_data_cadastro),
                     label = ~cliente_nome,
                     color = ~pal(idade_vis),
                     #clusterOptions = markerClusterOptions()
    ) %>%
    addLegend("topright",
              value = ~idade_vis,
              pal = pal,
              title = 'Tempo desde a última visita',
              opacity = .8)
  if(teste == F){
    #tabelas
    rm(cliente, pal, cli_in_ven, cli_in_ven_in_vcUltVis, visita_cliente, vendedor, vcUltVis);
    #var
    rm(cor_idade_vis, idades_vis)
  }
}else{
  m0 <- s_dados
}
#######################################################################
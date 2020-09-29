rm(list = ls())

#Lib pra conexão com o banco
library(odbc)
#Libs pra trabalhar com a base (cortes e funções similares ao SQL)
library(dplyr)
#library(dbplyr)
library(tidyverse)
#Lib pro gráfico
library(ggplot2)
#lib pros gráficos mais "interativos"
library(plotly)
library(highcharter)
library(htmlwidgets)
#Lib usada para porcentagem
#library(scales)
#Lib usada pra gerar cores aleatoriamente
library(RColorBrewer)

####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = T

####Variável usada para não apagar coisas na dash
dash = F

##Variável "Global"
empresa = 16 #Super
#empresa = 78 #Komatsu
ano_atual = '2020-01-01'

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

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = 'SQL Server',
                      Server = 'localhost\\SQLEXPRESS01',
                      Database = 'nhmobile_agriculture',
                      Trusted_Connection = 'True')

##Collect cria o df resultado da query, nesse caso, visitas_cliente
visita_cliente <- tbl(con,'visita_cliente') %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro) %>%
  collect ()
##Pra pegar o nome do resultado
visita_resultado <- tbl(con, "visita_resultado") %>%
  select(vr_id, vr_nome, vr_ativo) %>%
  #filter(vre_ativo == 1) %>% ##Ja sao todos ativos
  rename (resultado = vr_nome) %>%
  collect()
##Pra filtrar os resultados da empresa
visita_resultado_empresa <- tbl(con, "visita_resultado_empresa") %>%
  select(vre_resultado_id, vre_empresa_id, vre_ativo) %>%
  filter(vre_ativo == 1, vre_empresa_id == empresa) %>% ##Ja sao todos ativos mas mantive pra manter o filtro, ja filtro a empresa 
  collect()
#Arrumando encoding
Encoding(visita_resultado$resultado) <- 'latin1'

##junta as duas tabelas (resultado e resultado_empresa) pra pegar o id da empresa (vre_empresa_id) e o nome do resultado (vr_nome)
vis_res_emp <- inner_join(visita_resultado, visita_resultado_empresa, by = c('vr_id'= 'vre_resultado_id'))

##Pra pegar o nome do status
visita_status <- tbl(con,'visita_status') %>%
  select(vs_id, vs_nome, vs_ativo) %>%
  filter(vs_ativo == 1) %>%
  rename (motivo = vs_nome) %>%
  collect()
##Pra filtrar os status da empresa
visita_status_empresa <- tbl(con, "visita_status_empresa") %>%
  select(vse_status_id, vse_empresa_id, vse_ativo) %>%
  filter(vse_ativo == 1, vse_empresa_id == empresa) %>%
  collect()
#Arrumando encoding
Encoding(visita_status$motivo) <- 'latin1'

##coleta todos os vendedores
vendedor <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_id, vendedor_empresa_id, vendedor_ativo) %>%
  filter(vendedor_ativo == 1, vendedor_empresa_id == empresa) %>%
  collect()
#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)
  

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
  distinct(vc_vendedor_id, .keep_all = T)
  
### Gráfico v0 - Motivo das visitas (status) por vendedor

##descobrindo numero de status diferentes para criar paleta de cores
n_m_color <- n_distinct(vc_ij_vse_ij_v$vc_status_id)
brbg_mot <- brewer.pal(n_m_color,'BrBG')
axis_h <- list(
  title = ""
)
v0 <- plot_ly(vc_ij_vse_ij_v, type = "bar", x = ~vendedor_nome, y = ~motivo_n,# colors = ~I(motivo),
              colors = brbg_mot,
              name = ~motivo)

v0 <- v0 %>%
  layout(barmode = 'stack',
         xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
         yaxis = list(title = ''))
if(dash == F){
  v0
}


##Agrupando por vendedor e por resultado (vc_status_id, depois mostrar so motivo)
vc_ij_vre_ij_v <- vc_ij_vre_ij_v %>%
  group_by(vc_vendedor_id, vc_resultado_id) %>%
  mutate(resultado_n = n()) %>%
  distinct(vc_vendedor_id, .keep_all = T)

### Gráfico v1 - Resultado das visitas (resultados) por vendedor
##descobrindo numero de status diferentes para criar paleta de cores
n_r_color <- n_distinct(vc_ij_vre_ij_v$vc_resultado_id)
brbg_res <- brewer.pal(n_r_color,'BrBG')

v1 <- plot_ly(vc_ij_vre_ij_v, type = "bar", x = ~vendedor_nome, y = ~resultado_n,# colors = ~I(motivo),
              colors = brbg_res,
              name = ~resultado)
#marker = list(color = 'lightblue'))
#text = ~paste(categoria_nome,'<br>' , func_fmt_din(fat_med)),
#hoverinfo = "text")
#v0 <- v0 %>%
#  add_trace (y = y = ~motivo_n)

v1 <- v1 %>%
  layout(barmode = 'stack',
         xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
         yaxis = list(title = ''))
if(dash == F){
  v1
}

if(teste == F){
  #tabelas
  rm(visita_cliente, visita_resultado_empresa, vis_res_emp, vis_st_emp, vc_ij_vre, vc_ij_vse,
     vc_ij_vse_ij_v, vc_ij_vre_ij_v, vendedor, visita_resultado, visita_status, visita_status_empresa,
     axis_h);
  #variáveis
  rm(brbg_mot, brbg_res, n_r_color, n_m_color);
}

################################
### Tabelas e graficos de Clientes

cliente <- tbl(con,'cliente') %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_cidade, cliente_ultima_visita) %>%
  filter(cliente_empresa_id == empresa) %>%
  collect()

##coleta todos os vendedores
vendedor <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter(vendedor_ativo == 1, vendedor_empresa_id == empresa) %>%
  collect()
#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)


##Clientes cadastrados por vendedor ate 2020
cli_p_v <- cliente %>%
  filter(cliente_data_cadastro < ano_atual) %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes) %>%
  ungroup() %>%
  arrange(cliente_vendedor_id)

##Juncao pra pegar nome do vendedor
#cli_p_v <- inner_join(cli_p_v, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
#  select(cliente_vendedor_id, vendedor_nome, n_clientes) %>%
#  arrange(cliente_vendedor_id)

##Clientes cadastrados em 2020 por vendedor
cliente_2020 <- cliente %>%
  filter(cliente_data_cadastro >= ano_atual)

cli_p_v_2020 <- cliente_2020 %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes_2020 = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes_2020) %>%
  ungroup() %>%
  arrange(cliente_vendedor_id)

##Juncao pra pegar nome do vendedor
cli_p_v_2020 <- inner_join(cli_p_v_2020, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
  select(cliente_vendedor_id, vendedor_nome, n_clientes_2020)

##Juntando anos anteriores + 2020 em uma tabela (pro plotly)
cli_p_v_t_2020 <- left_join(cli_p_v_2020, cli_p_v, by = c('cliente_vendedor_id')) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_clientes_2020)

### Grafico v2 - Distribuicao de clientes cadastrados por vendedor

v2 <- plot_ly(cli_p_v_t_2020, x = ~vendedor_nome, y= ~n_clientes, type = 'bar',
              color = ~'',
              name = 'Até 2020')
v2 <- v2 %>%
  add_trace(y= ~n_clientes_2020, name = 'Em 2020')
v2 <- v2 %>%
  layout(yaxis = list(tittle = 'Clientes'), barmode = 'stack')

if(dash == F){
  v2
}

if(teste == F){
  #tabelas
  rm(cliente, cliente_2020, cli_p_v, cli_p_v_2020, cli_p_v_t_2020);
  #variáveis
  rm();
}
### Grafico v3 - Clientes cadastrados que possuem negocio (pizza)


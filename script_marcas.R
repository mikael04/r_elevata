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
#Lib para funções de tempo
library(lubridate)
#usada para converter números em moeda
#library(scales)
#Lib usada para emojis/fonts/box de valores
library(ggplot2)
#Lib usada para os mapas
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

##Teste script
empresa = 30
###################################

##Alterar o valor de inteiro para reais
func_fmt_din <- function(inteiro)
{
  inteiro_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_em_reais)
}
##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) -> posteriormente adicionar o "mi"
func_fmt_din_mi <- function(inteiro)
{
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1))
  return(inteiro_mi_em_reais)
}
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

###Começando script marcas
###########################################################################################################

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id)


##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
  select(vendedor_id, vendedor_empresa_id, vendedor_ativo) %>%
  filter (vendedor_empresa_id == empresa, vendedor_ativo == 1) %>%
  select(-vendedor_ativo)


##negocio_produto para pegar os valores de cada negócio
negocio_produto <- fread("Tabelas/negocio_produto.csv", colClasses = c(np_id = "character", np_negocio_id = "character", np_produto_id = "character")) %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor)


##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character", produto_marca_id = "character", produto_categoria_id = "character")) %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

##plotando texto sem informações #usado para gráficos que não tiverem nenhuma informação no período
text <- paste("Não há informações para o período")
s_dados <- ggplot() +
  annotate("text", x = 1, y = 6, size = 8, label = text) +
  theme_void()

##join de negocios e vendedores
negocio_ij_vendedor <- inner_join(negocio, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

########## Para o funil e para agrupar por faturamento
#### Vou alterar o agrupamento de número de negócios para faturamento, portanto precisarei da tabela negocio_produto
neg_ij_ven_ij_np <- inner_join(negocio_ij_vendedor, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##Filtrando empresa, vendedores ativos e negocios de anat
neg_ij_ven_ij_np_anat <- neg_ij_ven_ij_np %>%
  filter(negocio_data_cadastro >= ano_atual, negocio_negocio_situacao_id != 0)

historico_negocio_situacao <- fread("Tabelas/historico_negocio_situacao.csv", colClasses = c(historico_negocio_situacao_situacao_id = "character")) %>%
  select(historico_negocio_situacao_data, historico_negocio_situacao_negocio_id, historico_negocio_situacao_situacao_id)

historico_negocio_situacao_anat <- historico_negocio_situacao %>%
  filter(historico_negocio_situacao_data >= ano_atual)

##Right join de negocios, vendedores com histórico pra ter apenas a última
negocio_ij_historico_ij_vendedor_anat <- inner_join(negocio_ij_vendedor, historico_negocio_situacao_anat, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))
negocio_ij_historico_ij_vendedor_total <- inner_join(negocio_ij_vendedor, historico_negocio_situacao, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))

##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
negocio_ij_historico_ij_vendedor_anat <- negocio_ij_historico_ij_vendedor_anat[order(-negocio_ij_historico_ij_vendedor_anat$historico_negocio_situacao_situacao_id, negocio_ij_historico_ij_vendedor_anat$negocio_id),]
ng_ij_hist_ij_ven_anat <- negocio_ij_historico_ij_vendedor_anat[!duplicated(negocio_ij_historico_ij_vendedor_anat$negocio_id),]


##tabela vem dessa: ng_ij_hist_ij_ven_anat, já foi filtrada para histórico anat, filtrando empresa_id, vendedor_ativo
ng_ij_hist_ij_ven_anat <- ng_ij_hist_ij_ven_anat %>%
  filter(vendedor_empresa_id == empresa) %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_vendedor_id, negocio_data_cadastro, historico_negocio_situacao_data)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_anat <- ng_ij_hist_ij_ven_anat %>%
  filter(negocio_negocio_situacao_id != 0)

####Aqui é pra testar um top10 de uma empresa específica
##ng_ij_hist_ij_ven_top10 <-ng_ij_hist_ij_ven_anat

ng_ij_hist_ij_ven_ij_np_anat <- inner_join(ng_ij_hist_ij_ven_anat, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

ng_ij_hist_ij_ven_ij_np_ij_pd <- inner_join(ng_ij_hist_ij_ven_ij_np_anat, produto, by = c("np_produto_id" = "produto_id"))


##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- fread("Tabelas/categoria.csv", colClasses = c(categoria_id = 'character')) %>%
  select (categoria_id, categoria_nome, categoria_ativo) %>%
  filter(categoria_ativo == 1) %>% 
  select (-categoria_ativo)

Encoding(categoria$categoria_nome) <- 'latin1'


## Primeira vez pra verificar quais são os top10
ng_top10 <- ng_ij_hist_ij_ven_ij_np_ij_pd %>%
  select(produto_categoria_id, np_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(faturamento = sum(np_valor)) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(faturamento)) %>%
  collect ()

##pega só os top10
top10 <- head.matrix(ng_top10, n=10)

top10_ij <- inner_join(top10, categoria, by = c("produto_categoria_id"="categoria_id"))
#Removendo a contagem que ele fez de faturamento (usado pra gerar o top10)
top10_ij <- top10_ij[, -2:-3]
top10_ij <- as.data.frame(top10_ij)

## Transformando tudo em "OUTRA" -1
ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat <- left_join(ng_ij_hist_ij_ven_ij_np_ij_pd, top10_ij, by=c("produto_categoria_id" = "produto_categoria_id"))
ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat$produto_categoria_id[is.na(ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat$categoria_nome)] <- "-1"
ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat$categoria_nome[is.na(ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat$categoria_nome)] <- "OUTRA"

## Removendo os NA (três negócios cadastrados errados)
ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat <- ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat[!is.na(ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat$np_produto_id),]


## Agora pra agrupar todos (já trocado os anteriores pra categoria "OUTRA")
ng_top10_ag <- ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat %>%
  select(produto_categoria_id, np_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(faturamento = sum(np_valor)) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(faturamento)) %>%
  collect ()

##pega só os top10
top10 <- head.matrix(ng_top10_ag, n=10)


top10_ij <- inner_join(top10, categoria, by = c("produto_categoria_id"="categoria_id"))
#Removendo a contagem que ele fez de faturamento (usado pra gerar o top10)
top10_ij <- top10_ij[, -2]
top10_ij <- top10_ij[, -2]
top10_ij <- as.data.frame(top10_ij)

ng_top_ag <- inner_join(ng_top10_ag, top10_ij, by=c("produto_categoria_id" = "produto_categoria_id"))

#conversão de faturamento para texto
ng_top_ag <- ng_top_ag %>%
  mutate(fat_t = func_fmt_din(faturamento))

##Chart gerado para o treemap de categorias por faturamento em anat
chart_ng_top_ag <- ng_top_ag

### Gráfico hc_n4 - Valor financeiro de negócios em anat  (por categoria)
#################################################

if (nrow(chart_ng_top_ag) > 0){
  hc_n4 <- chart_ng_top_ag %>%
    hchart (
      type = "treemap",
      hcaes(x=categoria_nome, value=faturamento, color = faturamento, name = categoria_nome)) %>%
    hc_tooltip(formatter = JS("function () { return '<b>' + this.point.categoria_nome + '</b><br /> ' + ' <br />' + this.point.fat_t;}")) %>%
    hc_colorAxis(minColor = "#ADD8E6", maxColor = "#3182FF")
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  hc_n4 <- s_dados
}
if(dash == F){
  hc_n4
}



#################################################

### Aqui começa o gráfico de Tipos de máquina faturadas por categoria, em anat
#################################################


ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat_fat <-  ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat %>%
  filter(negocio_negocio_situacao_id == 4)

## Agora pra agrupar todos (já trocado os anteriores pra categoria "OUTRA")
ng_top10_ag_fat <- ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat_fat %>%
  select(produto_categoria_id, np_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(faturamento = sum(np_valor)) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(faturamento)) %>%
  collect ()


ng_top_ag_fat <- inner_join(ng_top10_ag_fat, top10_ij, by=c("produto_categoria_id" = "produto_categoria_id"))
ng_top_ag_fat <- ng_top_ag_fat %>%
  select (produto_categoria_id, categoria_nome, faturamento)

#Removendo tabelas não mais usadas
if (teste == F) {
  #tabelas
  rm(ng_top10, ng_top10_ag, top10, top10_ij, ng_ij_hist_ij_ven_ij_np_ij_pd, ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat)
}

#conversão de faturamento para texto
ng_top_ag_fat <- ng_top_ag_fat %>%
  mutate(fat_t = func_fmt_din(faturamento))

##Chart gerado para o treemap de categorias por faturamento em anat
chart_ng_top_ag_fat <- ng_top_ag_fat

### Gráfico hc_n5 - Máquinas faturadas em anat (por categoria)
if (nrow(chart_ng_top_ag_fat) > 0){
  hc_n5 <- chart_ng_top_ag_fat %>%
    hchart (
      "treemap",
      hcaes(x=categoria_nome, value=faturamento, color = faturamento)) %>%
    hc_tooltip(formatter = JS("function () { return '<b>' + this.point.categoria_nome + '</b><br /> ' + ' <br />' + this.point.fat_t;}")) %>%
    hc_colorAxis(minColor = "#90EE90", maxColor = "#32CD32")
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  hc_n5 <- s_dados
}
if(dash == F){
  hc_n5
}

if (teste == F) {
  rm(ng_top_ag, ng_top_ag_fat, ng_ij_hist_ij_ven_ij_np_ij_pd_ij_cat_fat, chart_ng_top_ag, chart_ng_top_ag_fat, ng_top10_ag_fat, ng_ij_hist_ij_ven_anat)
}
#################################################

##Começando scripts de mapas
#########################################################################################################
#########################################################################################################

##Distribuição de clientes, mostrando no label nome do cliente, nome do vendedor e última visita
cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_nome, cliente_latitude, cliente_longitude, cliente_vendedor_id, cliente_empresa_id)
#Arrumando encoding
Encoding(cliente$cliente_nome) <- 'latin1'

##Parque de máquinas
##Mostrar distribuição de máquinas por categoria
#######################################################################
parque_maquina <- fread("Tabelas/parque_maquina.csv", colClasses = c(pm_id = 'character', pm_cliente_id = 'character', pm_produto_id = 'character')) %>%
  select(pm_id, pm_cliente_id, pm_produto_id, pm_ano_modelo, pm_ativo) %>%
  filter (pm_ativo == 1)

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
marca <- fread("Tabelas/marca.csv", colClasses = c(marca_id = 'character')) %>%
  select(marca_id, marca_nome)

marca_categoria <- fread("Tabelas/marca_categoria.csv", colClasses = c(marca_categoria_marca_id = 'character',marca_categoria_categoria_id  = 'character'))

##Clientes da empresa correta ##Clientes com valor NA, valores 0 e valores positivos de latlong (hemisf norte, leste do globo) removidos
cliente_c_loc <- cliente %>%
  filter (cliente_empresa_id == empresa, cliente_latitude < 0, cliente_longitude < 0) %>%
  rename(lat = cliente_latitude, long = cliente_longitude) %>%
  mutate(lat = as.numeric(lat), long = as.numeric(long)) %>%
  filter (!is.null(lat), !is.na(lat))

cli_in_pm <- inner_join(cliente_c_loc, parque_maquina, by = c("cliente_id" = "pm_cliente_id")) %>%
  select (cliente_id, cliente_nome, lat, long, pm_id, pm_produto_id)

cli_in_pm_in_p <- inner_join(cli_in_pm, produto, by = c("pm_produto_id" = "produto_id")) %>%
  select (cliente_id, cliente_nome, lat, long, pm_id, pm_produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

cli_in_pm_in_p_in_m <- inner_join(cli_in_pm_in_p, marca, by = c("produto_marca_id" = "marca_id")) %>%
  select (cliente_id, cliente_nome, lat, long, pm_id, pm_produto_id, produto_nome, produto_marca_id, marca_nome, produto_categoria_id, produto_empresa_id)
#########################
##Começando o top para tratores
#########################
##Aqui obtenho todos os clientes e marcas, próximo passo é fazer o top5
cli_in_pm_cont_t <- cli_in_pm_in_p_in_m %>%
  select(cliente_id, cliente_nome, lat, long, produto_marca_id, marca_nome, produto_categoria_id) %>%
  filter(produto_categoria_id == 1) %>%
  group_by(cliente_id, produto_marca_id) %>%
  mutate(cont = n()) %>%
  distinct (cliente_id, .keep_all = T) %>%
  ungroup()


##Criando o top5 de categorias
cli_in_pm_cont_top_t <- cli_in_pm_cont_t %>%
  group_by(marca_nome) %>%
  mutate(cont = n()) %>%
  distinct(marca_nome, .keep_all = T) %>%
  select (marca_nome, produto_marca_id, cont) %>%
  arrange(desc(cont), marca_nome) %>%
  ungroup()


top5_t <- head.matrix(cli_in_pm_cont_top_t, 5)
top10_t <- head.matrix(cli_in_pm_cont_top_t, 10)
aux_t <- top5_t[,1]


cli_in_pm_cont_top_t_aux <- cli_in_pm_cont_t %>%
  filter (marca_nome %in% aux_t$marca_nome)

n_color <- nrow(cli_in_pm_cont_top_t_aux %>%
                  group_by(produto_marca_id) %>%
                  distinct(produto_marca_id) %>%
                  ungroup())

#Usando nome da marca como factor
cli_in_pm_cont_top_t_aux$marca_nome <- factor(cli_in_pm_cont_top_t_aux$marca_nome)

##Adicionando pequenas variações
cli_in_pm_cont_top_t_aux$lat <- jitter(cli_in_pm_cont_top_t_aux$lat, factor = .1, amount = 0)
cli_in_pm_cont_top_t_aux$long <- jitter(cli_in_pm_cont_top_t_aux$long, factor = .1, amount = 0)

#########################
##Começando o top para colheitadeiras
#########################
##Aqui obtenho todos os clientes e marcas, próximo passo é fazer o top5
cli_in_pm_cont_c <- cli_in_pm_in_p_in_m %>%
  select(cliente_id, cliente_nome, lat, long, produto_marca_id, marca_nome, produto_categoria_id) %>%
  filter(produto_categoria_id == 2) %>%
  group_by(cliente_id, produto_marca_id) %>%
  mutate(cont = n()) %>%
  distinct (cliente_id, .keep_all = T) %>%
  ungroup()


##Criando o top5 de categorias
cli_in_pm_cont_top_c <- cli_in_pm_cont_c %>%
  group_by(marca_nome) %>%
  mutate(cont = n()) %>%
  distinct(marca_nome, .keep_all = T) %>%
  select (marca_nome, produto_marca_id, cont) %>%
  arrange(desc(cont), marca_nome) %>%
  ungroup()


top5_c <- head.matrix(cli_in_pm_cont_top_c, 5)
top10_c <- head.matrix(cli_in_pm_cont_top_c, 10)
aux_c <- top5_c[,1]


cli_in_pm_cont_top_c_aux <- cli_in_pm_cont_c %>%
  filter (marca_nome %in% aux_c$marca_nome)

n_color <- nrow(cli_in_pm_cont_top_c_aux %>%
                  group_by(produto_marca_id) %>%
                  distinct(produto_marca_id) %>%
                  ungroup())

#Usando nome da marca como factor
cli_in_pm_cont_top_c_aux$marca_nome <- factor(cli_in_pm_cont_top_c_aux$marca_nome)

##Adicionando pequenas variações
cli_in_pm_cont_top_c_aux$lat <- jitter(cli_in_pm_cont_top_c_aux$lat, factor = .1, amount = 0)
cli_in_pm_cont_top_c_aux$long <- jitter(cli_in_pm_cont_top_c_aux$long, factor = .1, amount = 0)


##Se precisar consultar ícones, tamanho do ícone, marcas e marcas_ids
# marcas_ic_co <-read.csv("Icons/marcas_icon_csv.csv") %>%

marcas_icon <- iconList(
  '1' = makeIcon("Icons/NH_r.png", 23, 24),          ##Caso precise consultar, olhar o csv
  '3' = makeIcon("Icons/CI_r.png", 28, 24),
  '4' = makeIcon("Icons/JD_r.png", 26, 24),
  '5' = makeIcon("Icons/MF_r.png", 34, 24),
  '6' = makeIcon("Icons/agrale_r.png", 34, 24),
  '13' = makeIcon("Icons/jacto_r.png", 24, 24),
  '11' = makeIcon("Icons/valtra_r.png", 26, 24),
  '120191031172113' = makeIcon("Icons/ponsse_r.png", 24, 24),
  '120130518080852' = makeIcon("Icons/valmet_r.png", 26, 24),
  '120120724031949' = makeIcon("Icons/ideal_r.png", 19, 24),
  '120130802084245' = makeIcon("Icons/SLC_r.png", 26, 24),
  '120130522055326' = makeIcon("Icons/CBT_r.png", 28, 28),
  '120191031162533' = makeIcon("Icons/komatsu_r.png", 26, 24),
  '120191031171837' = makeIcon("Icons/JD_r.png", 26, 20),
  '120191031171708' = makeIcon("Icons/caterpillar_r.png", 38, 20),
  '120191031171942' = makeIcon("Icons/logmax_r.png", 29, 20),
  '120191031172239' = makeIcon("Icons/volvo_r.png", 24, 20),
  '120191031171807' = makeIcon("Icons/hyundai_r.png", 45, 20)
)

### Gráfico m1 de distribuição das marcas (top5) m1_t = tratores, m1_c = colheitadeiras
###################
##Caso não haja informações para plotar o mapa (texto informando que não há informações)
text <- paste("Não há informações para gerar o mapa")
s_dados_m <- ggplot() +
  annotate("text", x = 1, y = 6, size = 8, label = text) +
  theme_void()
##Tratores
if (nrow(cli_in_pm_cont_top_t_aux) > 0){
  m1_t <- leaflet(cli_in_pm_cont_top_t_aux) %>%
    addTiles() %>%
    addMarkers(lat = ~lat, lng = ~long, icon = ~marcas_icon[produto_marca_id],
               popup = paste0("Nome do cliente: ", cli_in_pm_cont_top_c_aux$cliente_nome,
                              "<br>",
                              "Quantidade de máquinas ", cli_in_pm_cont_top_c_aux$marca_nome,
                              " : ", cli_in_pm_cont_top_c_aux$cont),
               label = ~marca_nome,
               #clusterOptions = markerClusterOptions(),
               group = "Ícones")
}else {
  ##Caso não haja informações para plotar o mapa (texto informando que não há informações)
  m1_t <- s_dados_m
}

if (nrow(cli_in_pm_cont_top_c_aux) > 0){
  m1_c<- leaflet(cli_in_pm_cont_top_c_aux) %>%
    addTiles() %>%
    addMarkers(lat = ~lat, lng = ~long, icon = ~marcas_icon[produto_marca_id],
               popup = paste0("Nome do cliente: ", cli_in_pm_cont_top_c_aux$cliente_nome,
                              "<br>",
                              "Quantidade de máquinas ", cli_in_pm_cont_top_c_aux$marca_nome,
                              " : ", cli_in_pm_cont_top_c_aux$cont),
               label = ~marca_nome,
               #clusterOptions = markerClusterOptions(),
               group = "Ícones")
}else {
  ##Caso não haja informações para plotar o mapa (texto informando que não há informações)
  m1_c <- s_dados_m
}

if(teste == F){
  #tabelas
  rm(top5_t, top5_c, aux_c, aux_t, n_color, cli_in_pm_cont_c, cli_in_pm_cont_t, cliente_c_loc,
     cli_in_pm_cont_top_c_aux, cli_in_pm_cont_top_t_aux,parque_maquina, produto, marca, marca_categoria,
     marcas_icon, categoria);
  #variáveis
  rm();
}

###################
##Será usado apenas caso queira plotar todas as marcas como "Outros"
##Top5 + Outra, transformando as demais em OUTRA
#Tratores
if(nrow(cli_in_pm_cont_top_t) > 5)
{
  for(i in 6:nrow(cli_in_pm_cont_top_t)){
    cli_in_pm_cont_top_t[i,1] <- 'OUTRAS*'
    cli_in_pm_cont_top_t[i,2] <- '-1'
  }
}

##Criando o top5 (+ Outra) de categorias ##PARA TRATORES
cli_in_pm_cont_top_t <- cli_in_pm_cont_top_t %>%
  group_by(marca_nome) %>%
  mutate(sum = sum(cont)) %>%
  distinct(marca_nome, .keep_all = T) %>%
  select (marca_nome, produto_marca_id, sum) %>%
  arrange(desc(sum), marca_nome) %>%
  ungroup()

## Vou usar para consultar as cores de cada marca para o m2_t e m2_c
#marcas_cores <- read.csv("marcas_cores.csv")

##Cria factor e ordena o meu gráfico
cli_in_pm_cont_top_t$marca_nome <- reorder(cli_in_pm_cont_top_t$marca_nome, desc(cli_in_pm_cont_top_t$sum))

marcas_ic_co <-read.csv("Icons/marcas_icon_csv.csv") %>%
  select (marca_id_i, cor)
marcas_ic_co$marca_id_i <- as.character(marcas_ic_co$marca_id_i)
#cores_t
cli_in_pm_cont_top_t <- inner_join(cli_in_pm_cont_top_t, marcas_ic_co, by = c("produto_marca_id" = "marca_id_i"))
cores_t <- as.vector(cli_in_pm_cont_top_t$cor)
#Colheitadeiras
if(nrow(cli_in_pm_cont_top_c) > 5)
{
  for(i in 6:nrow(cli_in_pm_cont_top_c)){
    cli_in_pm_cont_top_c[i,1] <- 'OUTRAS*'
    cli_in_pm_cont_top_c[i,2] <- '-1'
  }
}

##Criando o top5 (+ Outra) de categorias ##PARA COLHEITADEIRAS
cli_in_pm_cont_top_c <- cli_in_pm_cont_top_c %>%
  group_by(marca_nome) %>%
  mutate(sum = sum(cont)) %>%
  distinct(marca_nome, .keep_all = T) %>%
  select (marca_nome, produto_marca_id, sum) %>%
  arrange(desc(sum), marca_nome) %>%
  ungroup()


##Cria factor e ordena o meu gráfico
cli_in_pm_cont_top_c$marca_nome <- reorder(cli_in_pm_cont_top_c$marca_nome, desc(cli_in_pm_cont_top_c$sum))
#cores_c
cli_in_pm_cont_top_c <- inner_join(cli_in_pm_cont_top_c, marcas_ic_co, by = c("produto_marca_id" = "marca_id_i"))
cores_c <- as.vector(cli_in_pm_cont_top_c$cor)
#############################################################################################
##Caso não haja informações para plotar o gráfico(texto informando que não há informações p/ o período)
text <- paste("Não há informações para gerar o gráfico")
s_dados_g <- ggplot() +
  annotate("text", x = 1, y = 6, size = 8, label = text) +
  theme_void()
### Gráfico m2 de distribuição das marcas (top5)
###################
if (nrow(cli_in_pm_cont_top_t) > 0){
  m2_t <- plot_ly(cli_in_pm_cont_top_t, type = 'bar', orientation = 'v',
                  x = ~marca_nome,
                  y = ~sum,
                  # color = ~pal(marca_nome), ##Pal deixou de funcionar, procurar depois
                  color = ~marca_nome,     #Se quiser setar pela tabela, com join, senáo usar o factor como é feito acima
                  colors = cores_t,
                  showlegend = FALSE)
  
  m2_t <- m2_t %>%
    layout(xaxis = list(title = ''),
           yaxis = list(title = ''))
}else {
  ##Caso não haja informações para plotar o gráfico(texto informando que não há informações p/ o período)
  m2_t <- s_dados_g
}
if(dash == F){
  m2_t
}

## Gráfico m2 de distribuição das marcas (top5)
###################
if (nrow(cli_in_pm_cont_top_c) > 0){
  m2_c <- plot_ly(cli_in_pm_cont_top_c, type = 'bar', orientation = 'v',
                  x = ~marca_nome,
                  y = ~sum,
                  # color = ~pal(marca_nome),
                  color = ~marca_nome,     #Se quiser setar pela tabela, com join, senáo usar o factor como é feito acima
                  colors = cores_c,
                  showlegend = FALSE)
  
  m2_c <- m2_c %>%
    layout(xaxis = list(title = ''),
           yaxis = list(title = ''))
}else{
  ##Caso não haja informações para plotar o gráfico(texto informando que não há informações p/ o período)
  m2_c <- s_dados_g
  
}
if(dash == F){
  m2_c
}
if(teste == F){
  #tabelas
  rm(cli_in_pm_cont_top_t, cli_in_pm_cont_top_c, cli_in_pm_in_p_in_m, cli_in_pm_in_p, cli_in_pm, top10_c, top10_t,
     marcas_ic_co);
  #variáveis
  rm(cores_c, cores_t);
}

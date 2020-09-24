rm(list = ls())
#Lib q será futuramente usada pros painéis interativos
#library(shiny)
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
#library(htmlwidgets)
#Lib pra usar paletas de cores
#library(RColorBrewer)
#library(viridis)
#Lib usada pros treemaps
#library(treemap)
#Lib usada pros waffles
#library(waffle)
#usada para converter números em moeda
library(scales)



####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = T
####Variável usada para não plotar os gráficos na dash
dash = F

##Variável "Global"
#empresa = 16 #Super

##Variável "Global"
empresa = 78 #Komatsu

func_fmt_din <- function(inteiro)
{
  inteiro_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_em_reais)
}

func_fmt_din_mi <- function(inteiro)
{
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1))
  return(inteiro_mi_em_reais)
}

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = "localhost\\SQLEXPRESS01",
                      Database = "nhmobile_agriculture",
                      Trusted_Connection = "True")

###Começando scripts negocio_scripts
###########################################################################################################

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
negocio <- tbl(con, "negocio") %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro) %>%
  collect()

##coleta todos os vendedores
vendedor_todos <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_id, vendedor_empresa_id,vendedor_ativo) %>%
  collect()

#Arrumando encode
Encoding(vendedor_todos$vendedor_nome) <- 'latin1'

##negocio_produto para pegar os valores de cada negócio
negocio_produto <- tbl(con, "negocio_produto") %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  collect()

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()

##join de negocios e vendedores
negocio_ij_vendedor <- inner_join(negocio, vendedor_todos, by=c("negocio_vendedor_id" = "vendedor_id"))

########## Para o funil e para agrupar por faturamento
#### Vou alterar o agrupamento de número de negócios para faturamento, portanto precisarei da tabela negocio_produto
neg_ij_ven_ij_np <- inner_join(negocio_ij_vendedor, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##Filtrando empresa, vendedores ativos e negocios de 2020
corte_1 <- neg_ij_ven_ij_np %>%
  filter(vendedor_empresa_id == empresa, vendedor_ativo == TRUE, negocio_data_cadastro >= as.Date("2020-01-01"), negocio_negocio_situacao_id != 0)


##removendo elementos não mais usados
if (teste == F) {
  rm(negocio, vendedor_todos) #vou precisar dele pro funil mais pra baixo negocio_ij_historico_ij_vendedor_2020)
}

##agrupamento de negocios por vendedor, contando faturamento por status e por vendedor
##Aqui eu poderia fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_ij_vn_ij_np_fat <- corte_1 %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, vendedor_nome, negocio_vendedor_id, np_valor) %>%
  group_by(negocio_negocio_situacao_id, negocio_vendedor_id) %>%
  mutate(total_fat = sum(np_valor)) %>%
  distinct (negocio_vendedor_id, .keep_all = TRUE) %>%
  collect ()

###remover status "desconsiderar (erro cadastro)"
ng_ij_vn_ij_np_fat <- ng_ij_vn_ij_np_fat[!(ng_ij_vn_ij_np_fat$negocio_negocio_situacao_id==9),]

status = c("1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "5 - faturado", "6 - financiamento nao aprovado", "7 - desistencia do cliente", "8 - perdemos para concorrencia", "4 - financiamento aprovado", "0 - intencao ou prospeccao")
##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_vn_ij_np_fat$negocio_status <- with(ng_ij_vn_ij_np_fat, cut(negocio_negocio_situacao_id, breaks = c(0,1,2,3,4,5,6,7,8,10), labels = status))
#precisa usar o factor pra colocar na ordem que quero
ng_ij_vn_ij_np_fat$negocio_status = factor(ng_ij_vn_ij_np_fat$negocio_status, levels = c("0 - intencao ou prospeccao", "1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "4 - financiamento aprovado", "5 - faturado", "6 - financiamento nao aprovado", "7 - desistencia do cliente", "8 - perdemos para concorrencia"))

#usando função pra criar outra coluna com número formatado (em Real, com pontos)
ng_ij_vn_ij_np_fat <- ng_ij_vn_ij_np_fat %>%
  mutate(total_fat_t = func_fmt_din(total_fat))

###Gráfico 0 - Número de clientes por vendedor
##############################################
n0 <- ggplot(ng_ij_vn_ij_np_fat, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), total_fat, fill=(negocio_status),
                                     text = paste('Valor neste status:', total_fat_t))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  ylab("Número de clientes") +
  #ggtitle("Volume de negócios cadastrados por vendedor, ano 2020") +
  theme (axis.text.x = element_text(hjust = 1), axis.title = element_blank())+
  scale_fill_manual(values = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF", "#32CD32", "yellow", "orange", "#DE0D26"))+
  scale_y_continuous(labels = scales::label_number())+
  coord_flip(expand = F)


n0 <- ggplotly(n0, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.1, y = -0.15))

###Gráfico 0 - Número de clientes por vendedor
if(dash == F){
  n0
}

##vou manter ng_ij_vn_ij_np_fat pra fazer o join mais pro final
if (teste == F) {
  rm(ng_ij_vn_ij_np_fat, corte_1, status)
}
##############################################

historico_negocio_situacao <- tbl(con, "historico_negocio_situacao") %>%
  select(historico_negocio_situacao_data, historico_negocio_situacao_negocio_id, historico_negocio_situacao_situacao_id) %>%
  collect()

historico_negocio_situacao_2020 <- historico_negocio_situacao %>%
  filter(historico_negocio_situacao_data >= as.Date("2020-01-01 00:00:00"))

##Right join de negocios, vendedores com histórico pra ter apenas a última
negocio_ij_historico_ij_vendedor_2020 <- inner_join(negocio_ij_vendedor, historico_negocio_situacao_2020, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))
negocio_ij_historico_ij_vendedor_total <- inner_join(negocio_ij_vendedor, historico_negocio_situacao, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))

##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
negocio_ij_historico_ij_vendedor_2020 <- negocio_ij_historico_ij_vendedor_2020[order(-negocio_ij_historico_ij_vendedor_2020$historico_negocio_situacao_situacao_id, negocio_ij_historico_ij_vendedor_2020$negocio_id),]
ng_ij_hist_ij_ven_2020 <- negocio_ij_historico_ij_vendedor_2020[!duplicated(negocio_ij_historico_ij_vendedor_2020$negocio_id),]


##tabela vem dessa: ng_ij_hist_ij_ven_2020, já foi filtrada para histórico 2020, filtrando empresa_id, vendedor_ativo
ng_ij_hist_ij_ven_2020 <- ng_ij_hist_ij_ven_2020 %>%
  filter(vendedor_empresa_id ==empresa, vendedor_ativo == TRUE) %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_vendedor_id, vendedor_ativo, vendedor_nome, negocio_data_cadastro, historico_negocio_situacao_data)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_2020 <- ng_ij_hist_ij_ven_2020 %>%
  filter(negocio_negocio_situacao_id != 0)

####Aqui é pra testar um top10 de uma empresa específica
##ng_ij_hist_ij_ven_top10 <-ng_ij_hist_ij_ven_2020

ng_ij_hist_ij_ven_ij_np_2020 <- inner_join(ng_ij_hist_ij_ven_2020, negocio_produto, by=c("negocio_id" = "np_negocio_id"))


#rm(ng_cn_d_vn, ng_ij_hist_ij_ven_2020)
#########################################################

###Começar a junção dos gráficos
##aqui caso o filtro seja mais de um, usamos o %in% ff
###Cuidar que status 4 e 5 estão trocados
## 5 - faturado, status[4], 7 - desistencia do cliente, status[6], 8 - perdemos para a concorrencia, status[8]
ff <- c(4, 6, 7)
#ggplotly(p1)

ng_ij_hist_ij_ven_2020_ij_np_fec <-  ng_ij_hist_ij_ven_ij_np_2020%>%
  filter(negocio_negocio_situacao_id %in% ff)

ng_ij_hist_ij_ven_2020_ij_np_fec <- ng_ij_hist_ij_ven_2020_ij_np_fec %>%
  mutate(negocio_status = negocio_negocio_situacao_id)


status_4_6_7 = c("5 - faturado", "7 - desistencia do cliente", "8 - perdemos para concorrencia")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
ng_ij_hist_ij_ven_2020_ij_np_fec$negocio_status <- with(ng_ij_hist_ij_ven_2020_ij_np_fec, cut(negocio_negocio_situacao_id, breaks = c(0,4,6,7),
                                                                                              labels = status_4_6_7))

ng_ij_hist_ij_ven_2020_ij_np_fec <- ng_ij_hist_ij_ven_2020_ij_np_fec %>% 
  filter (np_ativo == TRUE)

##agrupamento de negocios por vendedor, contando número de negócios por status e por vendedor
##Aqui eu poderia fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_ij_vn_ij_np_fech_fat <- ng_ij_hist_ij_ven_2020_ij_np_fec %>%
  select(negocio_id, negocio_vendedor_id, negocio_status, vendedor_nome, negocio_vendedor_id, np_valor) %>%
  group_by(negocio_status, negocio_vendedor_id) %>%
  mutate(total_fat = sum(np_valor)) %>%
  distinct (negocio_vendedor_id, .keep_all = TRUE) %>%
  collect ()

#usando função pra criar outra coluna com número formatado (em Real, com pontos)
ng_ij_vn_ij_np_fech_fat <- ng_ij_vn_ij_np_fech_fat %>%
  mutate(total_fat_t = func_fmt_din(total_fat))

### Gráfico 1 - Faturamento de negócios fechados em 2020
#########################################################
n3 <- ggplot(ng_ij_vn_ij_np_fech_fat, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), total_fat, fill=factor(negocio_status),
                                          text = paste('Valor neste status:', total_fat_t))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.title = element_blank(), axis.text.x = element_blank())+
  scale_fill_manual(values = c("#32CD32", "orange", "#DE0D26"),)+
  #scale_y_continuous(labels = scales::label_number())+
  coord_flip(expand = F)

n3 <- ggplotly(n3, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.2, y = -0.05))

### Gráfico 1 - Faturamento de negócios fechados em 2020
if(dash == F){
  n3
}


if (teste == F){
  #tabelas
  rm(historico_negocio_situacao_2020, negocio_ij_vendedor, negocio_ij_historico_ij_vendedor_2020, historico_negocio_situacao, ng_ij_vn_ij_np_fech_fat, ng_ij_hist_ij_ven_2020_ij_np_fec)
  #variáveis
  rm (ff, status_4_6_7)
}

#########################################################

### Negócios por categoria
###########################################################################################################################################
###Aqui começa o gráfico de Tipos de máquina faturadas por categoria, em 2020
#################################################

negocio_produto <- tbl(con, "negocio_produto") %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  collect()

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()

## Inner join pra não pegar os np_produto_id = 0
ngp_ij_pd <- inner_join(negocio_produto, produto, by=c("np_produto_id"="produto_id"))%>%
  filter (np_ativo == TRUE) %>%
  select (np_negocio_id, np_produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id, np_quantidade, np_valor)

if (teste == F) {
  rm(produto)
}

##tabela usada pra termos negócios, data de atualização (hist), nome do vendedor (vend) e produtos (negocio produto e produto)
##Aqui temos já com alguns cortes (np_ativo)
ng_ij_hist_ij_ven_ij_ngp_ij_pd <- inner_join(ng_ij_hist_ij_ven_2020, ngp_ij_pd, by = c("negocio_id" = "np_negocio_id"))
## Removendo os inativos (np_ativo == 0)


##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- tbl(con, "categoria") %>%
  select(categoria_id, categoria_nome) %>%
  collect()

Encoding(categoria$categoria_nome) <- 'latin1'


## Primeira vez pra verificar quais são os top10
ng_top10 <- ng_ij_hist_ij_ven_ij_ngp_ij_pd %>%
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
ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat <- left_join(ng_ij_hist_ij_ven_ij_ngp_ij_pd, top10_ij, by=c("produto_categoria_id" = "produto_categoria_id"))
ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat$produto_categoria_id[is.na(ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat$categoria_nome)] <- "-1"
ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat$categoria_nome[is.na(ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat$categoria_nome)] <- "OUTRA"

## Removendo os NA (três negócios cadastrados errados)
ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat <- ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat[!is.na(ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat$np_produto_id),]


## Agora pra agrupar todos (já trocado os anteriores pra categoria "OUTRA")
ng_top10_ag <- ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat %>%
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

##Chart gerado para o treemap de categorias por faturamento em 2020
chart_ng_top_ag <- ng_top_ag

##Gráfico 2 - Valor financeiro de negócios em 2020  (por categoria)
#################################################

hc_n4 <- chart_ng_top_ag %>%
  hchart (
    type = "treemap",
    hcaes(x=categoria_nome, value=faturamento, color = faturamento, name = categoria_nome)) %>%
  hc_tooltip(formatter = JS("function () { return '<b>' + this.point.categoria_nome + '</b><br /> ' + ' <br />' + this.point.fat_t;}")) %>%
  hc_colorAxis(minColor = "#ADD8E6", maxColor = "#3182FF")

if(dash == F){
  hc_n4
}

#################################################

### Aqui começa o gráfico de Tipos de máquina faturadas por categoria, em 2020
#################################################


ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat_fat <-  ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat %>%
  filter(negocio_negocio_situacao_id == 4)

## Agora pra agrupar todos (já trocado os anteriores pra categoria "OUTRA")
ng_top10_ag_fat <- ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat_fat %>%
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
  rm(categoria, ng_top10, ng_top10_ag, top10, top10_ij, ng_ij_hist_ij_ven_ij_ngp_ij_pd, ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat, ngp_ij_pd)
}

#conversão de faturamento para texto
ng_top_ag_fat <- ng_top_ag_fat %>%
  mutate(fat_t = func_fmt_din(faturamento))

##Chart gerado para o treemap de categorias por faturamento em 2020
chart_ng_top_ag_fat <- ng_top_ag_fat

##Gráfico 3 - Máquinas faturadas em 2020 (por categoria)
hc_n5 <- chart_ng_top_ag_fat %>%
  hchart (
    "treemap",
    hcaes(x=categoria_nome, value=faturamento, color = faturamento)) %>%
  hc_tooltip(formatter = JS("function () { return '<b>' + this.point.categoria_nome + '</b><br /> ' + ' <br />' + this.point.fat_t;}")) %>%
  hc_colorAxis(minColor = "#90EE90", maxColor = "#32CD32")

if(dash == F){
  hc_n5
}

if (teste == F) {
  rm(ng_top_ag, ng_top_ag_fat, ng_ij_hist_ij_ven_ij_ngp_ij_pd_ij_cat_fat, chart_ng_top_ag, chart_ng_top_ag_fat, ng_top10_ag_fat, ng_ij_hist_ij_ven_2020)
}
#################################################

### Idade dos negócios
###################################################################################################################################
##Filtrar vendedores ativos e empresa
ng_ij_hist_ij_ven_2020 <- negocio_ij_historico_ij_vendedor_total %>%
  filter (vendedor_ativo == TRUE, vendedor_empresa_id == empresa)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_2020 <- ng_ij_hist_ij_ven_2020 %>%
  filter(negocio_negocio_situacao_id != 0)

## Vou ordenar por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais novas, ficar só com a primeira atualização após mudança de status
ng_ij_hist_ij_ven_2020 <- ng_ij_hist_ij_ven_2020[order(ng_ij_hist_ij_ven_2020$historico_negocio_situacao_situacao_id, ng_ij_hist_ij_ven_2020$negocio_id), ]

##Aqui estou removendo duplicatas (de outra forma), que removem apenas quando o histórico_situacao e negocio_id se repetem
ng_ij_hist_ij_ven_2020 <- subset(ng_ij_hist_ij_ven_2020, !duplicated(subset(ng_ij_hist_ij_ven_2020, select=c(historico_negocio_situacao_situacao_id, negocio_id))))

##filtrando apenas negócios não fechados negocio_situacao_id != 4,5,6,7,9
ff = c(4,5,6,7,9)

ng_ij_hist_ij_ven_ab <- ng_ij_hist_ij_ven_2020 %>%
  filter(!negocio_negocio_situacao_id %in% ff)


status_aberto = c("1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "4 - financiamento aprovado", "0 - intencao ou prospeccao")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
ng_ij_hist_ij_ven_ab$negocio_status <- with(ng_ij_hist_ij_ven_ab, cut(negocio_negocio_situacao_id, breaks = c(0,1,2,3,8,10),
                                                                      labels = status_aberto))

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_ab <- ng_ij_hist_ij_ven_ab %>%
  filter(negocio_negocio_situacao_id != 0)

idades = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses")

ng_ij_hist_ij_ven_idd <- ng_ij_hist_ij_ven_ab %>%
  select (negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, vendedor_nome, historico_negocio_situacao_data) %>%
  mutate(idade = (as.integer(as.POSIXct(Sys.time()) - as.POSIXct(negocio_data_cadastro)))) %>%
  mutate(idade_cat = NA) %>%
  arrange(idade)


#ng_ij_hist_ij_ven_2020$idade <- as.numeric(ng_ij_hist_ij_ven_2020$idade)
ng_ij_hist_ij_ven_idd <- ng_ij_hist_ij_ven_idd %>%
  select (negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, vendedor_nome, historico_negocio_situacao_data, idade) %>%
  mutate(idade = as.integer(idade)) %>%
  arrange(idade)

#typeof(ng_ij_hist_ij_ven_idd$idade)


ng_ij_hist_ij_ven_idd$idade_cat[ng_ij_hist_ij_ven_idd$idade < 10000]<- idades[5]
ng_ij_hist_ij_ven_idd$idade_cat[ng_ij_hist_ij_ven_idd$idade < 735] <- idades[4]
ng_ij_hist_ij_ven_idd$idade_cat[ng_ij_hist_ij_ven_idd$idade < 366] <- idades[3]
ng_ij_hist_ij_ven_idd$idade_cat[ng_ij_hist_ij_ven_idd$idade < 181] <- idades[2]
ng_ij_hist_ij_ven_idd$idade_cat[ng_ij_hist_ij_ven_idd$idade < 61] <- idades[1]


##Aqui eu posso fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios c/status) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_ij_hist_ij_ven_num <- ng_ij_hist_ij_ven_idd %>%
  select(negocio_id, negocio_vendedor_id, vendedor_nome, idade, idade_cat) %>%
  group_by(idade_cat, negocio_vendedor_id) %>%
  mutate(num_negocios_idades = n()) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  collect ()

##Usado pra ordenar o gráfico
ng_ij_hist_ij_ven_num$idade_cat = factor(ng_ij_hist_ij_ven_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))




##Gráfico 4 - Idade dos negócios abertos, por vendedor, por idade do negocio, barras
n6 <- ggplot(ng_ij_hist_ij_ven_num, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), num_negocios_idades, fill=idade_cat, label = num_negocios_idades,
                                        text = paste('Número de negócios neste intervalo:', num_negocios_idades))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank())+
  scale_fill_manual(values = c("#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26"))+
  coord_flip(expand = F)

n6 <- ggplotly(n6, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.3, y = -0.08))

##Gráfico 4 - Idade dos negócios abertos, por vendedor, por idade do negocio
if(dash == F){
  n6
}


if (teste == F) {
  #tabelas
  rm(ng_ij_hist_ij_ven_num, ng_ij_hist_ij_ven_ab)
  #variáveis
  rm(ff, idades, status_aberto)
}
###########################################



###Negócios abertos da empresa
############################################################################
ng_ij_hist_ij_emp_num <- ng_ij_hist_ij_ven_idd %>%
  select(idade, idade_cat) %>%
  group_by(idade_cat) %>%
  mutate(num_negocios_idades = n()) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  collect ()

total_negocios = sum(ng_ij_hist_ij_emp_num$num_negocios_idades)

ng_ij_hist_ij_emp_num <- ng_ij_hist_ij_emp_num %>%
  select(idade, idade_cat, num_negocios_idades) %>%
  group_by(idade_cat) %>%
  mutate(porcentagem = num_negocios_idades/total_negocios) %>%
  mutate(porcent = scales::percent(porcentagem)) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  arrange(idade) %>%
  collect ()

ng_ij_hist_ij_emp_num$idade_cat = factor(ng_ij_hist_ij_emp_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))

###Gráfico 5 - Negócios abertos da empresa, pizza

colors_pie <- c("#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26")
n7 <- plot_ly(ng_ij_hist_ij_emp_num, labels = ~idade_cat, values = ~num_negocios_idades, type = 'pie', sort = F,
              texttemplate = "%{value} (%{percent})",
              hovertemplate = paste ("%{label} <br>",
                                     "Equivalente a %{percent}",
                                     "<extra></extra>"),
              marker = list(colors = colors_pie))

if(dash == F){
  n7
}

if (teste == F) {
  #tabelas
  rm(ng_ij_hist_ij_ven_idd, ng_ij_hist_ij_emp_num)
  #variáveis
  rm(colors_pie, total_negocios)
}
#######################################################################################################

### Funil de vendas (vendas abertas)
########################################################################
##Status aberto
st_a = c(1,2,3,8,10)
##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##Tempo: Total
##status abertos apenas
ng_ij_hist_ij_ven_funil_ab <- neg_ij_ven_ij_np %>%
  filter(negocio_negocio_situacao_id %in% st_a)

##filtrar apenas por empresa
ng_ij_hist_ij_ven_funil_ab <- ng_ij_hist_ij_ven_funil_ab %>%
  filter(vendedor_empresa_id == empresa, vendedor_ativo == T)

status = c("Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado", "Intenção ou prospecção")
#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_ij_hist_ij_ven_funil_ab <- ng_ij_hist_ij_ven_funil_ab %>%
  mutate(negocio_status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_hist_ij_ven_funil_ab$negocio_status <- with(ng_ij_hist_ij_ven_funil_ab, cut(negocio_negocio_situacao_id, breaks = c(0,1,2,3,8,10),
                                                                                  labels = status))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_ab %>%
  select (negocio_status, np_valor, negocio_negocio_situacao_id) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  collect()


##conversão de faturamento para texto
ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_fat %>%
  mutate(tot_fat_t = func_fmt_din(total_faturado))

##Começando a gambiarra (criar nova columa com nome da categoria + valor da categoria)
ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_fat %>%
  mutate(nome_cat_valor_fat = paste(negocio_status, "\n", tot_fat_t))

##Vou converter o id de situação pra 0, pra poder ordenar (intenção seria o status 0, é o primeiro)
ng_ij_hist_ij_ven_funil_fat$negocio_negocio_situacao_id[ng_ij_hist_ij_ven_funil_fat$negocio_negocio_situacao_id == 10] <- 0
ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_fat %>%
  arrange(negocio_negocio_situacao_id)

###Funil agrupado por faturamento
n9 <- plot_ly (ng_ij_hist_ij_ven_funil_fat) %>%
  add_trace(
    type ="funnelarea",
    values = ng_ij_hist_ij_ven_funil_fat$total_faturado,
    text = ng_ij_hist_ij_ven_funil_fat$nome_cat_valor_fat,
    textinfo = "text",
    hovertemplate = paste ("%{text} <br>",
                           "Equivalente a %{percent}",
                           "<extra></extra>"),
    marker = list(colors = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF")),
    showlegend = FALSE
  )
#n9 <- n9 %>%
#  layout(uniformtext=list(minsize=10, mode='show'))
if(dash == F){
  n9
}


if (teste == F){
  #tabelas
  rm(neg_ij_ven_ij_np, ng_ij_hist_ij_ven_funil_ab, ng_ij_hist_ij_ven_funil_fat)
  #variáveis
  rm(st_a, status)
}
#############################################################

##Fechados do mês
##Status fechado
st_f = c(4,5,6,7)
status_f = c("Faturado", "Financiamento não aprovado", "Desistencia do cliente", "Perdemos para concorrência")

##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##Tempo: Total
##status abertos
#ng_rj já havia feito o corte apenas em fechados após 2020
ng_ij_hist_ij_ven_fec_2020 <- ng_ij_hist_ij_ven_ij_np_2020 %>%
  filter(negocio_negocio_situacao_id %in% st_f)

##Funil já filtrou (na verdade ng_ij_hist_ij_ven_2020 já filtrou) vendedor ativo e empresa = 16

#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_ij_hist_ij_ven_fec_2020 <- ng_ij_hist_ij_ven_fec_2020 %>%
  mutate(negocio_status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_hist_ij_ven_fec_2020$negocio_status <- with(ng_ij_hist_ij_ven_fec_2020, cut(negocio_negocio_situacao_id, breaks = c(0,4,5,6,7),
                                                                                  labels = status_f))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_hist_ij_ven_funil_fat_fec_2020 <- ng_ij_hist_ij_ven_fec_2020 %>%
  select (negocio_status, np_valor) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  collect()

##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_ij_hist_ij_ven_funil_fat_fec_2020  <- ng_ij_hist_ij_ven_funil_fat_fec_2020  %>%
  select (negocio_status, total_faturado) %>%
  mutate(tot_fat_m = as.integer(total_faturado/1000000)) %>%
  collect()

ng_ij_hist_ij_ven_funil_fat_fec_2020 <- ng_ij_hist_ij_ven_funil_fat_fec_2020 %>%
  mutate(total_fat_t = func_fmt_din_mi(total_faturado))

##Gráfico de pizza fechados do ano 2020
##############################################
colors_pie <- c("#32CD32", "yellow" , "orange" , "#DE0D26")
n10 <- plot_ly(ng_ij_hist_ij_ven_funil_fat_fec_2020, labels = ~negocio_status, values = ~total_faturado, type = 'pie', sort = F,
               text = ~total_fat_t,
               texttemplate = "%{text}mi (%{percent})",
               hovertemplate = paste ("%{label} <br>",
                                      "%{text}mi <br>",
                                      "Equivalente a %{percent}",
                                      "<extra></extra>"),
               marker = list(colors = colors_pie))

if(dash == F){
  n10
}

##############################################

#Criando nova tabela com dados apenas do mês
ng_ij_hist_ij_ven_funil_fat_fec_2020_mes <- ng_ij_hist_ij_ven_fec_2020 %>%
  filter(historico_negocio_situacao_data >= '2020-08-01') %>%
  mutate(negocio_status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_hist_ij_ven_funil_fat_fec_2020_mes$negocio_status <- with(ng_ij_hist_ij_ven_funil_fat_fec_2020_mes, cut(negocio_negocio_situacao_id, breaks = c(0,4,5,6,7),
                                                                                                              labels = status_f))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_hist_ij_ven_funil_fat_fec_2020_mes <- ng_ij_hist_ij_ven_funil_fat_fec_2020_mes %>%
  select (negocio_status, np_valor) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  collect()

##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_ij_hist_ij_ven_funil_fat_fec_2020_mes  <- ng_ij_hist_ij_ven_funil_fat_fec_2020_mes  %>%
  select (negocio_status, total_faturado) %>%
  mutate(tot_fat_m = as.integer(total_faturado/1000000)) %>%
  collect()

#criando uma coluna nova para usar como texto dentro do gráfico
ng_ij_hist_ij_ven_funil_fat_fec_2020_mes <- ng_ij_hist_ij_ven_funil_fat_fec_2020_mes %>%
  mutate(total_fat_t = func_fmt_din_mi(total_faturado))

##Gráfico de pizza fechados do mês
##############################################

colors_pie <- c("#32CD32", "yellow" , "orange" , "#DE0D26")
n11 <- plot_ly(ng_ij_hist_ij_ven_funil_fat_fec_2020_mes, labels = ~negocio_status, values = ~total_faturado, type = 'pie', sort = F,
               text = ~total_fat_t,
               texttemplate = "%{text}mi (%{percent})",
               hovertemplate = paste ("%{label} <br>",
                                      "%{text}mi <br>",
                                      "Equivalente a %{percent}",
                                      "<extra></extra>"),
               marker = list(colors = colors_pie))

if(dash == F){
  n11
}

if (teste == F){
  #tabelas
  rm(ng_ij_hist_ij_ven_funil_fat_fec_2020, ng_ij_hist_ij_ven_funil_fat_fec_2020_mes, ng_ij_hist_ij_ven_fec_2020)
  #variáveis
  rm(colors_pie, st_f, status_f)
}

### Começando código p/ gráficos highchart de distribuição no tempo
###################################################################

##Filtrando empresa, vendedores ativos e negocios de 2020
ng_ij_hist_ij_ven_2020_fat <- ng_ij_hist_ij_ven_ij_np_2020 %>%
  filter(negocio_negocio_situacao_id == 4)

fat_2020_mes <- ng_ij_hist_ij_ven_2020_fat %>%
  mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
  group_by (ym) %>%
  summarize(ym_sum = sum(np_valor))
#as.xts(order.by = .$ym) #convertendo em xts

##Usando highcharts
#hc_n12 <- hchart(fat_2020_mes, type = 'line', hcaes(x = ym, y = ym_sum))
#hc_n12

#n12 <- plot_ly(fat_2020_mes, type = 'scatter', mode = 'line', x = ~ym, y =~ym_sum)


##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
negocio_ij_historico_ij_vendedor_total <- negocio_ij_historico_ij_vendedor_total[order(-negocio_ij_historico_ij_vendedor_total$historico_negocio_situacao_situacao_id, negocio_ij_historico_ij_vendedor_total$negocio_id),]
ng_ij_hist_ij_ven_total <- negocio_ij_historico_ij_vendedor_total[!duplicated(negocio_ij_historico_ij_vendedor_total$negocio_id),]


##tabela vem dessa: negocio_ij_historico_ij_vendedor_total, todos os vendedores, filtrando empresa_id, vendedor_ativo
ng_ij_hist_ij_ven_total <- ng_ij_hist_ij_ven_total %>%
  filter(vendedor_empresa_id == empresa, vendedor_ativo == TRUE) %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_vendedor_id, vendedor_ativo, vendedor_nome, negocio_data_cadastro, historico_negocio_situacao_data)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_total <- ng_ij_hist_ij_ven_total %>%
  filter(negocio_negocio_situacao_id != 0)


ng_ij_hist_ij_ven_ij_np_total <- inner_join(ng_ij_hist_ij_ven_total, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##Começando o cálculo do faturamento médio dos três anos anteriores

##primeiro vou selecionar por ano, e depois apenas status faturado
ng_ij_hist_ij_ven_ij_np_2019_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
  filter (historico_negocio_situacao_data < '2020-01-01' & historico_negocio_situacao_data > '2018-12-31' & negocio_negocio_situacao_id == 4)
ng_ij_hist_ij_ven_ij_np_2018_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
  filter (historico_negocio_situacao_data < '2019-01-01' & historico_negocio_situacao_data > '2017-12-31' & negocio_negocio_situacao_id == 4)
ng_ij_hist_ij_ven_ij_np_2017_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
  filter (historico_negocio_situacao_data < '2018-01-01' & historico_negocio_situacao_data > '2016-12-31' & negocio_negocio_situacao_id == 4)

#Aqui vou fazer a mesma soma dos valores dos três anos anteriores, dividindo por mês
fat_2019_mes <- ng_ij_hist_ij_ven_ij_np_2019_fat %>%
  mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
  group_by (ym) %>%
  summarize(ym_sum = sum(np_valor))

fat_2018_mes <- ng_ij_hist_ij_ven_ij_np_2018_fat %>%
  mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
  group_by (ym) %>%
  summarize(ym_sum = sum(np_valor))



###Forma 2 de fazer o gráfico de linhas com faturamento anual (substituindo com NA linhas faltantes)
#######################################################################
if (empresa == 16)
{
    
  n_linhas <- nrow(fat_2020_mes)
  fat_2020_mes_aux <- fat_2019_mes
  fat_2020_mes_aux$ym_sum <- NA
  fat_2020_mes_aux$ym_sum[1:n_linhas] <- fat_2020_mes$ym_sum
  
  fat_2020_2019_2018_mes <- fat_2020_mes_aux
  fat_2020_2019_2018_mes[, "ym_sum_2019"] <- fat_2019_mes$ym_sum
  fat_2020_2019_2018_mes[, "ym_sum_2018"] <- fat_2018_mes$ym_sum
  
  meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
  ## alterando pra números pra poder fazer da mesma forma
  fat_2020_2019_2018_mes$ym <- as.integer(fat_2020_2019_2018_mes$ym)
  fat_2020_2019_2018_mes$ym <- with(fat_2020_2019_2018_mes, cut(ym, breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
                                                                labels = meses))
  
  
  #######################################################################
  
  ##Começando o gráfico
  ###Assim estarei mostrando 2020 e uma média dos anos anteriores
  a <- list(
    title = '',
    showticklabels = T
  )
  ay <- list(
    overlaying = 'y',
    side = 'right',
    title = ''
  )
  
  ###Assim estarei mostrando 2020, 2019 e 2018
  
  n12 <- plot_ly(fat_2020_2019_2018_mes)
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers',x = ~ym, y =~ym_sum,
              name = 'Faturamento de 2020',
              text = ~paste(func_fmt_din_mi(ym_sum),'milhões'),
              hoverinfo = "text",
              color = I("blue")
    )
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers',x = ~ym, y = ~ym_sum_2019, yaxis = ay,
              name = 'Faturamento de 2019',
              text = ~paste(func_fmt_din_mi(ym_sum_2019),'milhões'),
              hoverinfo = "text",
              color = I("red"))
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers',x = ~ym, y = ~ym_sum_2018, yaxis = ay,
              name = 'Faturamento de 2018',
              text = ~paste(func_fmt_din_mi(ym_sum_2018),'milhões'),
              hoverinfo = "text",
              color = I("green"))
  
  n12 <- n12 %>%
    layout(xaxis = a, yaxis = a,
           #aqui eu ajusto onde quero que apareça a legenda
           legend = list(x=0.8, y=0.9)#)
    )
  n12
  
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = 'y',
    side = 'right',
    title = ''
  )
  
  
  
  if (teste == F){
    #tabelas
    rm(ng_ij_hist_ij_ven_ij_np_2020, fat_2020_mes, ng_ij_hist_ij_ven_total, negocio_ij_historico_ij_vendedor_total,
       fat_2019_mes, fat_2018_mes)
    #variáveis
    rm()
  }

}

##############################################

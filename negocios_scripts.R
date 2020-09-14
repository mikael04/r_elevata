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
#Lib pra usar paletas de cores
#library(RColorBrewer)
#Lib usada pros treemaps
#library(treemap)
#Lib usada para porcentagem
library(scales)



####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = 0

##Variável "Global"
empresa = 16 #Super

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "nhmobile_agriculture", 
                      Trusted_Connection = "True")

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
negocio <- tbl(con, "negocio") %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_vendedor_id) %>%
  collect()

##coleta todos os vendedores
vendedor_todos <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id,vendedor_ativo) %>%
  collect()

##negocio_produto para pegar os valores de cada negócio
negocio_produto <- tbl(con, "negocio_produto") %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  collect()

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()


##join de negocios e vendedores
negocio_lj_vendedor <- inner_join(negocio, vendedor_todos, by=c("negocio_vendedor_id" = "vendedor_id"))

#### Vou alterar o agrupamento de número de negócios para faturamento, portanto precisarei da tabela negocio_produto
neg_lj_ven_in_np <- inner_join(negocio_lj_vendedor, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##Filtrando empresa, vendedores ativos e negocios de 2020
corte_1 <- neg_lj_ven_in_np %>%
  filter(vendedor_empresa_id == empresa, vendedor_ativo == TRUE, negocio_data_cadastro >= as.Date("2020-01-01"), negocio_negocio_situacao_id != 0)

##removendo elementos não mais usados
if (teste == 0) {
  rm(negocio, vendedor_todos) #vou precisar dele pro funil mais pra baixo negocio_rj_historico_lj_vendedor_2020)
}

##agrupamento de negocios por vendedor, contando faturamento por status e por vendedor
##Aqui eu poderia fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_lj_vn_in_np_2020_fat <- corte_1 %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, vendedor_nome, negocio_vendedor_id, np_valor) %>%
  group_by(negocio_negocio_situacao_id, negocio_vendedor_id) %>%
  mutate(total_fat = sum(np_valor)) %>%
  distinct (negocio_vendedor_id, .keep_all = TRUE) %>%
  collect ()

##Forma antiga, agrupando por numero de egocios
######################################################################################
##corte inicial, selecionando apenas uma empresa, id 16 = Super, vendedor_ativo se o vendedor está ativo (true ou false), data
##Aqui vejo só negócios cadastrados em 2020, preciso ver negócios faturados em 2020


##agrupamento de negocios por vendedor, contando número de negócios por status e por vendedor
##Aqui eu poderia fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios) e depois o join, sem o select, ou então usar o mutate como foi feito
#ng_lj_vn_2020_n <- corte_1 %>%
#  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, vendedor_nome, negocio_vendedor_id) %>%
#  group_by(negocio_negocio_situacao_id, negocio_vendedor_id) %>%
#  mutate(num_negocios = n()) %>%
#  distinct (negocio_vendedor_id, .keep_all = TRUE) %>%
#  collect ()

##removendo elementos não mais usados
#rm(corte_1)


### XXXXX   Caso volte a trabalhar com número de pedidos, terei que alterar de ng_lj_vn_in_np_2020_fat para ng_lj_vn_2020_n

##aqui eu estou alterando o joão paulo, que havia problemas com codificação
ng_lj_vn_in_np_2020_fat$vendedor_nome[ng_lj_vn_in_np_2020_fat$negocio_vendedor_id == 45] <- "JOÃO PAULO"

###remover status "desconsiderar (erro cadastro)"
ng_lj_vn_in_np_2020_fat <- ng_lj_vn_in_np_2020_fat[!(ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id==9),]

##aqui ele transforma os inteiros em chars pra fazermos a alteração
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id <- as.character(ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id)


status = c("1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "4 - financiamento aprovado", "5 - faturado", "6 - financiamento nao aprovado", "7 - desistencia do cliente", "8 - perdemos para concorrencia", "0 - intencao ou prospeccao")
##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 1] <- status[1]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 2] <- status[2]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 3] <- status[3]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 4] <- status[5]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 5] <- status[6]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 6] <- status[7]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 7] <- status[8]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 8] <- status[4]
ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id[ng_lj_vn_in_np_2020_fat$negocio_negocio_situacao_id == 10] <- status[9]

##factor no "fill" cria os léveis da variável

###Selecionar cores desejadas
##########################################
#if (teste == 0) {
#  display.brewer.pal(n = 6, name = 'YlOrRd')
#  brewer.pal(n = 6, name = "YlOrRd")
#}


#################################


##Gráfico do faturamento por vendedor
##############################################
##############################################
n0 <- ggplot(ng_lj_vn_in_np_2020_fat, aes(vendedor_nome, total_fat, fill=factor(negocio_negocio_situacao_id))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  ylab("Número de clientes") +
  #ggtitle("Volume de negócios cadastrados por vendedor, ano 2020") +
  theme (axis.text.x = element_text(hjust = 1), axis.title = element_blank())+
  scale_fill_manual(values = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF", "#32CD32", "yellow", "orange", "#DE0D26"))+
  scale_y_continuous(labels = scales::label_number())+
  coord_flip(expand = F)

ggplotly(n0, tooltip=c("Nome", "total_fat")) %>%
  layout(legend = list(orientation = "h", x = -0.1, y = -0.15))
##############################################


##vou manter pra fazer o join mais pro final
if (teste == 0) {
  rm(ng_lj_vn_in_np_2020_fat)
}

### Aqui começa o segundo gráfico que preciso das datas de atualização (última atualização, por isso o join com historico)
########################################

##filtra os vendedores ativos
##vendedor <- filter(q_vendedor_todos, q_vendedor_todos$vendedor_ativo == 1)

historico_negocio_situacao  <- tbl(con, "historico_negocio_situacao") %>%
  select(historico_negocio_situacao_data, historico_negocio_situacao_negocio_id, historico_negocio_situacao_situacao_id) %>%
  collect()

historico_negocio_situacao_2020 <- historico_negocio_situacao %>%
  filter(historico_negocio_situacao_data >= as.Date("2020-01-01 00:00:00"))

##Right join de negocios, vendedores com histórico pra ter apenas a última
negocio_rj_historico_lj_vendedor_2020 <- inner_join(negocio_lj_vendedor, historico_negocio_situacao_2020, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))
negocio_rj_historico_lj_vendedor_total <- inner_join(negocio_lj_vendedor, historico_negocio_situacao, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))

##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
negocio_rj_historico_lj_vendedor_2020 <- negocio_rj_historico_lj_vendedor_2020[order(-negocio_rj_historico_lj_vendedor_2020$historico_negocio_situacao_situacao_id, negocio_rj_historico_lj_vendedor_2020$negocio_id),]
ng_ij_hist_ij_ven <- negocio_rj_historico_lj_vendedor_2020[!duplicated(negocio_rj_historico_lj_vendedor_2020$negocio_id),]


##tabela vem dessa: ng_ij_hist_ij_ven, já foi filtrada para histórico 2020, filtrando empresa_id, vendedor_ativo
ng_ij_hist_ij_ven <- ng_ij_hist_ij_ven %>%
  filter(vendedor_empresa_id == empresa, vendedor_ativo == TRUE) %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_vendedor_id, vendedor_ativo, vendedor_nome, negocio_data_cadastro, historico_negocio_situacao_data)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven <- ng_ij_hist_ij_ven %>%
  filter(negocio_negocio_situacao_id != 0)


####Aqui é pra testar um top10 de uma empresa específica
##ng_rj_hist_lj_ven_top10 <-ng_ij_hist_ij_ven

#aqui eu estou alterando o joão paulo (da super empresa_id 16), que havia problemas com codificação
ng_ij_hist_ij_ven$vendedor_nome[ng_ij_hist_ij_ven$negocio_vendedor_id == 45] <- "JOÃO PAULO"

ng_ij_hist_ij_ven_ij_np_2020 <- inner_join(ng_ij_hist_ij_ven, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

#rm(ng_cn_d_vn, ng_ij_hist_ij_ven)
#########################################################

###Começar a junção dos gráficos
##aqui caso o filtro seja mais de um, usamos o %in% ff
###Cuidar que status 4 e 5 estão trocados
## 5 - faturado, status[4], 7 - desistencia do cliente, status[6], 8 - perdemos para a concorrencia, status[8]
ff <- c(4, 6, 7)
#ggplotly(p1)

ng_ij_hist_ij_ven_ij_np_fec <-  ng_ij_hist_ij_ven_ij_np_2020%>%
  filter(negocio_negocio_situacao_id %in% ff)

ng_ij_hist_ij_ven_ij_np_fec <- ng_ij_hist_ij_ven_ij_np_fec %>%
  mutate(Negocio_Status = negocio_negocio_situacao_id)


status_4_6_7 = c("5 - faturado", "7 - desistencia do cliente", "8 - perdemos para concorrencia")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
ng_ij_hist_ij_ven_ij_np_fec$Negocio_Status <- with(ng_ij_hist_ij_ven_ij_np_fec, cut(negocio_negocio_situacao_id, breaks = c(0,4,6,7),
                                                                            labels = status_4_6_7))


#ng_ij_hist_ij_ven_ij_np_fec$negocio_negocio_situacao_id[ng_ij_hist_ij_ven_ij_np_fec$negocio_negocio_situacao_id == 4] <- status[5]
#ng_ij_hist_ij_ven_ij_np_fec$negocio_negocio_situacao_id[ng_ij_hist_ij_ven_ij_np_fec$negocio_negocio_situacao_id == 6] <- status[7]
#ng_ij_hist_ij_ven_ij_np_fec$negocio_negocio_situacao_id[ng_ij_hist_ij_ven_ij_np_fec$negocio_negocio_situacao_id == 7] <- status[8]

##agrupamento de negocios por vendedor, contando número de negócios por status e por vendedor
##Aqui eu poderia fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_lj_vn_in_np_2020_fech_fat <- ng_ij_hist_ij_ven_ij_np_fec %>%
  select(negocio_id, negocio_vendedor_id, Negocio_Status, vendedor_nome, negocio_vendedor_id, np_valor) %>%
  group_by(Negocio_Status, negocio_vendedor_id) %>%
  mutate(total_fat = sum(np_valor)) %>%
  distinct (negocio_vendedor_id, .keep_all = TRUE) %>%
  collect ()

### Faturamento de negócios fechados em 2020
n3 <- ggplot(ng_lj_vn_in_np_2020_fech_fat, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), total_fat, fill=factor(Negocio_Status), label = total_fat)) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.title = element_blank(), axis.text.x = element_blank())+
  scale_fill_manual(values = c("#32CD32", "orange", "#DE0D26"),)+
  #scale_y_continuous(labels = scales::label_number())+
  coord_flip(expand = F)

ggplotly(n3) %>%
  layout(legend = list(orientation = "h", x = -0.2, y = -0.05))


if (teste == 0) {
  rm(ng_ij_hist_ij_ven_ij_np_fec, historico_negocio_situacao_2020, negocio_lj_vendedor)
}

#########################################################################################

### Negócios por categoria
#########################################################################################
##negocio num de itens (trator e colheitadeira)

negocio_produto <- tbl(con, "negocio_produto") %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  collect()

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()

## Inner join pra não pegar os np_produto_id = 0
ngp_lj_pd <- inner_join(negocio_produto, produto, by=c("np_produto_id"="produto_id"))%>%
  filter (np_ativo == TRUE) %>%
  select (np_negocio_id, np_produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id, np_quantidade, np_valor) 

if (teste == 0) {
  rm(negocio_produto, produto)
}


##tabela usada pra termos negócios, data de atualização (hist), nome do vendedor (vend) e produtos (negocio produto e produto)
##Aqui temos já com alguns cortes (np_ativo)
ng_rj_hist_lj_ven_lj_ngp_lj_pd <- left_join(ng_ij_hist_ij_ven, ngp_lj_pd, by = c("negocio_id" = "np_negocio_id"))
## Removendo os inativos (np_ativo == 0)


##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- tbl(con, "categoria") %>%
  select(categoria_id, categoria_nome) %>%
  collect()


## Primeira vez pra verificar quais são os top10
ng_top10 <- ng_rj_hist_lj_ven_lj_ngp_lj_pd %>%
  select(produto_categoria_id, np_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(faturamento = sum(np_valor)) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(faturamento)) %>%
  collect ()

##pega só os top10
top10 <- head.matrix(ng_top10, n=10)

top10_lj <- left_join(top10, categoria, by = c("produto_categoria_id"="categoria_id"))
#Removendo a contagem que ele fez de faturamento (usado pra gerar o top10)
top10_lj <- top10_lj[, -2:-3]
top10_lj <- as.data.frame(top10_lj)
##Alterando por causa do caracter especial ########## AQUI VOU TER Q ALTERAR POR EMPRESA
top10_lj$categoria_nome[top10_lj$produto_categoria_id == 	120181114100824] <- "CAMINHÃO"

## Transformando tudo em "OUTRA" -1
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat <- left_join(ng_rj_hist_lj_ven_lj_ngp_lj_pd, top10_lj, by=c("produto_categoria_id" = "produto_categoria_id"))
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$produto_categoria_id[is.na(ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$categoria_nome)] <- "-1"
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$categoria_nome[is.na(ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$categoria_nome)] <- "OUTRA"

## Removendo os NA (três negócios cadastrados errados)
ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat[!is.na(ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat$np_produto_id),]


## Agora pra agrupar todos (já trocado os anteriores pra categoria "OUTRA")
ng_top10_ag <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat %>%
  select(produto_categoria_id, np_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(faturamento = sum(np_valor)) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(faturamento)) %>%
  collect ()

##pega só os top10
top10 <- head.matrix(ng_top10_ag, n=10)


top10_lj <- left_join(top10, categoria, by = c("produto_categoria_id"="categoria_id"))
#Removendo a contagem que ele fez de faturamento (usado pra gerar o top10)
top10_lj <- top10_lj[, -2]
top10_lj <- top10_lj[, -2]
top10_lj <- as.data.frame(top10_lj)
##Alterando por causa do caracter especial
top10_lj$categoria_nome[top10_lj$produto_categoria_id == 	120181114100824] <- "CAMINHÃO"

ng_top_ag <- left_join(ng_top10_ag, top10_lj, by=c("produto_categoria_id" = "produto_categoria_id"))

chart_ng_top_ag <- ng_top_ag

### Chart 1
##################################################################
##Aqui são mostradas todas as máquinas negociadas em 2020
##Aqui são mostradas todas as máquinas negociadas em 2020
hc_n4 <- chart_ng_top_ag %>%
  hchart (
    "treemap",
    hcaes(x=categoria_nome, value=faturamento, color = faturamento)) %>%
  hc_colorAxis(minColor = "#ADD8E6", maxColor = "#3182FF")

hc_n4

ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat_fat <-  ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat %>%
  filter(negocio_negocio_situacao_id == 4)

## Agora pra agrupar todos (já trocado os anteriores pra categoria "OUTRA")
ng_top10_ag_fat <- ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat_fat %>%
  select(produto_categoria_id, np_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(faturamento = sum(np_valor)) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  arrange(desc(faturamento)) %>%
  collect ()


ng_top_ag_fat <- left_join(ng_top10_ag_fat, top10_lj, by=c("produto_categoria_id" = "produto_categoria_id"))
ng_top_ag_fat <- ng_top_ag_fat %>%
  select (produto_categoria_id, categoria_nome, faturamento)

chart_ng_top_ag_fat <- ng_top_ag_fat

#Removendo tabelas não mais usadas
if (teste == 0) {
  rm(categoria, ng_top10, ng_top10_ag, top10, top10_lj, ng_rj_hist_lj_ven_lj_ngp_lj_pd, ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat)
}

### Chart 2, Aqui são mostradas todasas máquinas faturadas em 2020
##################################################################

hc_n5 <- chart_ng_top_ag_fat %>%
  hchart (
    "treemap",
    hcaes(x=categoria_nome, value=faturamento, color = faturamento)) %>%
  hc_colorAxis(minColor = "#90EE90", maxColor = "#32CD32")

hc_n5

if (teste == 0) {
  rm(ng_top_ag, ng_top_ag_fat, ng_rj_hist_lj_ven_lj_ngp_lj_pd_lj_cat_fat, chart_ng_top_ag, chart_ng_top_ag_fat, ng_top10_ag_fat, ng_ij_hist_ij_ven)
}
##################################################################
### Gráfico de produtos mais vendidos por negócios e seu faturamento (FAZER DEPOIS)
##################################################################
#vou pegar a tabela de negócios historico e vendedor com negócio produto 
#ng_prod <- ng_rj_hist_lj_ven_lj_ngp_lj_pd %>%
#  select(negocio_id, negocio_negocio_situacao_id, negocio_data_cadastro, np_produto_id, produto_nome, np_quantidade, np_valor)

##Join com produto pra saber qual produto 
#ng_rj_hist_lj_prod <- left_join(ng_ij_hist_ij_ven, ngp_lj_pd, by=c("negocio_id" = "np_negocio_id"))

### Idade dos negócios
##################################################################
##Filtrar vendedores ativos e empresa
ng_ij_hist_ij_ven <- negocio_rj_historico_lj_vendedor_total %>%
  filter (vendedor_ativo == TRUE, vendedor_empresa_id == empresa)

## Vou ordenar por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais novas, ficar só com a primeira atualização após mudança de status
ng_ij_hist_ij_ven <- ng_ij_hist_ij_ven[order(ng_ij_hist_ij_ven$historico_negocio_situacao_situacao_id, ng_ij_hist_ij_ven$negocio_id), ]

##Aqui estou removendo duplicatas (de outra forma), que removem apenas quando o histórico_situacao e negocio_id se repetem
ng_ij_hist_ij_ven <- subset(ng_ij_hist_ij_ven, !duplicated(subset(ng_ij_hist_ij_ven, select=c(historico_negocio_situacao_situacao_id, negocio_id))))

##filtrando apenas negócios não fechados negocio_situacao_id != 4,5,6,7,9
ff = c(4,5,6,7,9)

ng_rj_hist_lj_ven_ab <- ng_ij_hist_ij_ven %>%
  filter(!negocio_negocio_situacao_id %in% ff)

status = c("1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "4 - financiamento aprovado", "5 - faturado", "6 - financiamento nao aprovado", "7 - desistencia do cliente", "8 - perdemos para concorrencia", "0 - intencao ou prospeccao")
##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id == 1] <- status[1]
ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id == 2] <- status[2]
ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id == 3] <- status[3]
ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id == 8] <- status[4]
ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_ab$negocio_negocio_situacao_id == 10] <- status[9]


##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_rj_hist_lj_ven_ab <- ng_rj_hist_lj_ven_ab %>%
  filter(negocio_negocio_situacao_id != 0)

idades = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses")

ng_rj_hist_lj_ven_idd <- ng_rj_hist_lj_ven_ab %>%
  select (negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, vendedor_nome, historico_negocio_situacao_data) %>%
  mutate(idade = (as.integer(as.POSIXct(Sys.time()) - as.POSIXct(negocio_data_cadastro)))) %>%
  mutate(idade_cat = 0) %>%
  arrange(idade)


#ng_ij_hist_ij_ven$idade <- as.numeric(ng_ij_hist_ij_ven$idade)
ng_rj_hist_lj_ven_idd <- ng_rj_hist_lj_ven_idd %>%
  select (negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, vendedor_nome, historico_negocio_situacao_data, idade) %>%
  mutate(idade = as.integer(idade)) %>%
  arrange(idade)

#typeof(ng_rj_hist_lj_ven_idd$idade)


ng_rj_hist_lj_ven_idd$idade_cat[ng_rj_hist_lj_ven_idd$idade < 10000]<- idades[5]
ng_rj_hist_lj_ven_idd$idade_cat[ng_rj_hist_lj_ven_idd$idade < 735] <- idades[4]
ng_rj_hist_lj_ven_idd$idade_cat[ng_rj_hist_lj_ven_idd$idade < 366] <- idades[3]
ng_rj_hist_lj_ven_idd$idade_cat[ng_rj_hist_lj_ven_idd$idade < 181] <- idades[2]
ng_rj_hist_lj_ven_idd$idade_cat[ng_rj_hist_lj_ven_idd$idade < 61] <- idades[1]

          
##Aqui eu posso fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios c/status) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_rj_hist_lj_ven_num <- ng_rj_hist_lj_ven_idd %>%
  select(negocio_id, negocio_vendedor_id, vendedor_nome, idade, idade_cat) %>%
  group_by(idade_cat, negocio_vendedor_id) %>%
  mutate(num_negocios_idades = n()) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  collect ()

ng_rj_hist_lj_ven_num$idade_cat = factor(ng_rj_hist_lj_ven_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))
#aqui eu estou alterando o joão paulo, que havia problemas com codificação
ng_rj_hist_lj_ven_num$vendedor_nome[ng_rj_hist_lj_ven_num$negocio_vendedor_id == 45] <- "JOÃO PAULO"

##Gráfico de idade dos negócios abertos, por vendedor, por idade do negocio
##############################################
n6 <- ggplot(ng_rj_hist_lj_ven_num, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), num_negocios_idades, fill=idade_cat, label = num_negocios_idades)) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank())+
  scale_fill_manual(values = c("#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26"))+
  coord_flip(expand = F)
ggplotly(n6) %>%
  layout(legend = list(orientation = "h", x = -0.3, y = -0.08))
############################################################################

###Gráfico de idades geral da empresa antes pizza, alterado pra waffle por categoria de idade
############################################################################

ng_rj_hist_lj_emp_num <- ng_rj_hist_lj_ven_idd %>%
  select(idade, idade_cat) %>%
  group_by(idade_cat) %>%
  mutate(num_negocios_idades = n()) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  collect ()

total_negocios = sum(ng_rj_hist_lj_emp_num$num_negocios_idades)

ng_rj_hist_lj_emp_num <- ng_rj_hist_lj_emp_num %>%
  select(idade, idade_cat, num_negocios_idades) %>%
  group_by(idade_cat) %>%
  mutate(porcentagem = num_negocios_idades/total_negocios) %>%
  mutate(porcent = scales::percent(porcentagem)) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  arrange(idade) %>%
  collect ()

ng_rj_hist_lj_emp_num$idade_cat = factor(ng_rj_hist_lj_emp_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))

####Gráfico de pizza

colors_pie <- c("#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26")
n7 <- plot_ly(ng_rj_hist_lj_emp_num, labels = ~idade_cat, values = ~num_negocios_idades, type = 'pie', sort = F,
              marker = list(colors = colors_pie, showlegend = F))
n7 <- n7 %>%
  layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
n7

###Pizza ggplot (com porcentagem)
#
#n7 <- ggplot(ng_rj_hist_lj_emp_num, aes(x = "", y = porcentagem,  fill = idade_cat))+
#  geom_bar(width = 1, stat="identity") +
#  ggtitle("Negócios de toda empresa em aberto") +
#  coord_polar("y", start=0)+
#  scale_fill_manual(values = c("#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26"),)+
#  theme_void() +
#  geom_text(aes(x = 1, y = cumsum(porcentagem) - porcentagem/2, label = porcent), size=5)
 # 
#ggplotly(n7)

## Gráfico de waffle, ainda flata arrumar nome dos agrupamentos
#vetor_auxiliar <- `ng_rj_hist_lj_emp_num$num_negocios_idades`= ng_rj_hist_lj_emp_num$num_negocios_idades
#
#ng_rj_hist_lj_emp_num <- ng_rj_hist_lj_emp_num %>%
#  mutate(categorias = (`ng_rj_hist_lj_emp_num$num_negocios_idades` + ng_rj_hist_lj_emp_num$num_negocios_idades))
#
#p5 <- plot_ly()
#p5 <- waffle(ng_rj_hist_lj_emp_num$num_negocios_idades/30,
#             size=0.5,
#             title = "Negócios de toda a empresa em aberto",
#             xlab=" 1 quadrado = 30 negócios")
#p5
#######################################################################################################


### Funil de vendas (vendas abertas + fechadas em 2020)
########################################################################

##ng_ij_hist_ij_ven já removeu status 9 (erro no cadastro)
ff = c(4,5,6,7)
##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##antes de 2020
#remover os que estão com status fechado E status < 2020
ng_rj_hist_lj_ven_funil <- ng_ij_hist_ij_ven[!(ng_ij_hist_ij_ven$negocio_negocio_situacao_id %in% ff & ng_ij_hist_ij_ven$historico_negocio_situacao_data <= as.Date("2020-01-01")),]

ng_rj_hist_lj_ven_funil <- ng_rj_hist_lj_ven_funil %>%
  filter(negocio_negocio_situacao_id != 0)
  
status = c("1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "4 - financiamento aprovado", "5 - faturado", "6 - financiamento nao aprovado", "7 - desistencia do cliente", "8 - perdemos para concorrencia", "0 - intencao ou prospeccao")

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string


ng_rj_hist_lj_ven_funil <- ng_rj_hist_lj_ven_funil %>%
  mutate(Negocio_Status = negocio_negocio_situacao_id)
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
ng_rj_hist_lj_ven_funil$Negocio_Status <- with(ng_rj_hist_lj_ven_funil, cut(negocio_negocio_situacao_id, breaks = c(0,1,2,3,4,5,6,7,8,10),
                                                                                         labels = status))


#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 1] <- status[1]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 2] <- status[2]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 3] <- status[3]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 4] <- status[5]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 5] <- status[6]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 6] <- status[7]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 7] <- status[8]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 8] <- status[4]
#ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id[ng_rj_hist_lj_ven_funil$negocio_negocio_situacao_id == 10] <- status[9]


## Aqui tenho a contagem de negócios por status (todos os abertos + fechados em 2020)
ng_rj_hist_lj_ven_funil_cnt <- ng_rj_hist_lj_ven_funil %>%
  select (Negocio_Status) %>%
  group_by(Negocio_Status) %>%
  mutate(contagem = n()) %>%
  arrange(Negocio_Status) %>%
  distinct(Negocio_Status, .keep_all = TRUE) %>%
  collect()


##se precisar fatorar pra ordenar
#ng_rj_hist_lj_emp_num$idade_cat = factor(ng_rj_hist_lj_emp_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))

##Funil de vendas (simples)

n8 <- plot_ly()
n8 <- n8 %>%
  add_trace(
    type="funnelarea",
    values = ng_rj_hist_lj_ven_funil_cnt$contagem,
    text = c("Intenção ou prospecção", "Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado", "Faturado", "Financiamento não aprovado", "Desistência do cliente", "Perdemos para a concorrência"),
    marker = list(colors = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF", "#32CD32", "yellow", "orange", "#DE0D26"))
    #marker = list(color = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF", "#32CD32", "yellow", "orange", "#DE0D26"))
    )
#p6 <- p6 %>%
#  layout(yaxis = list(categoryarray = c("Intenção ou prospecção", "Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado", "Faturado", "Financiamento não aprovado", "Desistência do cliente", "Perdemos para a concorrência")))
n8
########################################################################################

### Funil de vendas (vendas abertas)
########################################################################
##Status aberto
st_a = c(1,2,3,8,10)
##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##Tempo: Total
##status abertos apenas
ng_rj_hist_lj_ven_funil_ab <- neg_lj_ven_in_np %>%
  filter(negocio_negocio_situacao_id %in% st_a)

##filtrar apenas por empresa
ng_rj_hist_lj_ven_funil_ab <- ng_rj_hist_lj_ven_funil_ab %>%
  filter(vendedor_empresa_id == empresa, vendedor_ativo == T)

status = c("1 - em negociacao", "2 - montagem de cadastro", "3 - aguardando aprovacao", "4 - financiamento aprovado", "0 - intencao ou prospeccao")
#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_rj_hist_lj_ven_funil_ab <- ng_rj_hist_lj_ven_funil_ab %>%
  mutate(Negocio_Status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_rj_hist_lj_ven_funil_ab$Negocio_Status <- with(ng_rj_hist_lj_ven_funil_ab, cut(negocio_negocio_situacao_id, breaks = c(0,1,2,3,8,10),
                                                                                         labels = status))


## Aqui tenho a contagem de negócios por status (todos os abertos)
#ng_rj_hist_lj_ven_funil_ab_cnt <- ng_rj_hist_lj_ven_funil_ab %>%
#  select (negocio_negocio_situacao_id) %>%
#  group_by(negocio_negocio_situacao_id) %>%
#  mutate(contagem = n()) %>%
#  arrange(negocio_negocio_situacao_id) %>%
#  distinct(negocio_negocio_situacao_id, .keep_all = TRUE) %>%
#  collect()

##Agora será agrupado por pedidos em aberto e faturamento total
ng_rj_hist_lj_ven_funil_fat <- ng_rj_hist_lj_ven_funil_ab %>%
  select (Negocio_Status, np_valor) %>%
  group_by(Negocio_Status) %>%
  mutate(Total_Faturado = sum(np_valor)) %>%
  arrange(Negocio_Status) %>%
  distinct(Negocio_Status, .keep_all = TRUE) %>%
  collect()

##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_rj_hist_lj_ven_funil_fat <- ng_rj_hist_lj_ven_funil_fat %>%
  select (Negocio_Status, Total_Faturado) %>%
  mutate(tot_fat_m = as.integer(Total_Faturado/1000000)) %>%
  collect()


##se precisar fatorar pra ordenar
#ng_rj_hist_lj_emp_num$idade_cat = factor(ng_rj_hist_lj_emp_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))

##Funil de vendas (simples), negócios abertos em porcentagem 
########################################################################################
#n9 <- plot_ly()
#n9 <- n9 %>%
#  add_trace(
#    type="funnelarea",
#    values = ng_rj_hist_lj_ven_funil_fat$Total_Faturado,
#    text = c("Intenção ou prospecção", "Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado"),
#  #hoverinfo = 'text',
#    marker = list(colors = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF")),
#    showlegend = FALSE
#  )
#
#n9
########################################################################################

##Funil de vendas (simples), negócios abertos em contagem 
########################################################################################
n9 <- plot_ly (ng_rj_hist_lj_ven_funil_fat) %>%
  add_trace(
    type ="funnelarea",
    values = ng_rj_hist_lj_ven_funil_fat$tot_fat_m,
    #text = ng_rj_hist_lj_ven_funil_fat$negocio_negocio_situacao_id,
    text = c("Intenção ou prospecção", "Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado"),
    textinfo = "text+value",
    hovertemplate = paste ("%{value:,.0f}Mi",
                           "equivalente a %{percent}",
                           "<extra></extra>"),
    marker = list(colors = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF")),
    showlegend = FALSE
  )

n9

##Fechados do mês
##Status fechado
st_f = c(4,5,6,7)
status_f = c("5 - faturado", "6 - financiamento nao aprovado", "7 - desistencia do cliente", "8 - perdemos para concorrencia")

##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##Tempo: Total
##status abertos 
#ng_rj já havia feito o corte apenas em fechados após 2020
ng_rj_hist_lj_ven_fec_2020 <- ng_ij_hist_ij_ven_ij_np_2020 %>%
  filter(negocio_negocio_situacao_id %in% st_f)

##Funil já filtrou (na verdade ng_ij_hist_ij_ven já filtrou) vendedor ativo e empresa = 16

#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_rj_hist_lj_ven_fec_2020 <- ng_rj_hist_lj_ven_fec_2020 %>%
  mutate(Negocio_Status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_rj_hist_lj_ven_fec_2020$Negocio_Status <- with(ng_rj_hist_lj_ven_fec_2020, cut(negocio_negocio_situacao_id, breaks = c(0,4,5,6,7),
                                                                                  labels = status_f))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_rj_hist_lj_ven_funil_fat_fec_2020 <- ng_rj_hist_lj_ven_fec_2020 %>%
  select (Negocio_Status, np_valor) %>%
  group_by(Negocio_Status) %>%
  mutate(Total_Faturado = sum(np_valor)) %>%
  arrange(Negocio_Status) %>%
  distinct(Negocio_Status, .keep_all = TRUE) %>%
  collect()

##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_rj_hist_lj_ven_funil_fat_fec_2020  <- ng_rj_hist_lj_ven_funil_fat_fec_2020  %>%
  select (Negocio_Status, Total_Faturado) %>%
  mutate(tot_fat_m = as.integer(Total_Faturado/1000000)) %>%
  collect()

##Gráfico de pizza fechados do ano

colors_pie <- c("#32CD32", "yellow" , "orange" , "#DE0D26")
n10 <- plot_ly(ng_rj_hist_lj_ven_funil_fat_fec, labels = ~Negocio_Status, values = ~Total_Faturado, type = 'pie', sort = F,
              marker = list(colors = colors_pie, showlegend = F))
n10 <- n10 %>%
  layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
n10

#Criando nova tabela com dados apenas do mês
ng_rj_hist_lj_ven_funil_fat_fec_2020_mes <- ng_rj_hist_lj_ven_fec_2020 %>%
  filter(historico_negocio_situacao_data >= '2020-08-01') %>%
  mutate(Negocio_Status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_rj_hist_lj_ven_funil_fat_fec_2020_mes$Negocio_Status <- with(ng_rj_hist_lj_ven_funil_fat_fec_2020_mes, cut(negocio_negocio_situacao_id, breaks = c(0,4,5,6,7),
                                                                                  labels = status_f))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_rj_hist_lj_ven_funil_fat_fec_2020_mes <- ng_rj_hist_lj_ven_funil_fat_fec_2020_mes %>%
  select (Negocio_Status, np_valor) %>%
  group_by(Negocio_Status) %>%
  mutate(Total_Faturado = sum(np_valor)) %>%
  arrange(Negocio_Status) %>%
  distinct(Negocio_Status, .keep_all = TRUE) %>%
  collect()

##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_rj_hist_lj_ven_funil_fat_fec_2020_mes  <- ng_rj_hist_lj_ven_funil_fat_fec_2020_mes  %>%
  select (Negocio_Status, Total_Faturado) %>%
  mutate(tot_fat_m = as.integer(Total_Faturado/1000000)) %>%
  collect()

##Gráfico de pizza fechados do mês

colors_pie <- c("#32CD32", "yellow" , "orange" , "#DE0D26")
n11 <- plot_ly(ng_rj_hist_lj_ven_funil_fat_fec_2020_mes, labels = ~Negocio_Status, values = ~Total_Faturado, type = 'pie', sort = F,
               marker = list(colors = colors_pie, showlegend = F))
n11 <- n11 %>%
  layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
n11

##Remover tudo utilizado
if (teste == 0) {
  #rm(list=ls())
}


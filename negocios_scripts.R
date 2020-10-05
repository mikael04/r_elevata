rm(list = ls())
#Lib q será futuramente usada pros painéis interativos
#library(shiny)
#Lib pra conexão com o banco
library(odbc)
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
#Lib para lidar com o tempo
library(lubridate)
#Lib usada para emojis/fonts/box de valores
library(ggplot2)
library(emojifont)



####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = F
####Variável usada para não plotar os gráficos na dash
dash = F
####Variavel global c/ ano atual (para comparação)
ano_atual = '2020-01-01'

##Variável "Global"
emp_am = 42 # Amazonia
emp_ar = 77 # Araguaia
emp_ko = 78 # Komatsu
emp_ms = 35 # Ms
emp_su = 16 # Super
emp_si = 59 # Simex 
emp_ta = 60 # Taisa

##Empresa utilizada
empresa <- emp_ko

#Alterar o valor de inteiro para reais
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
vendedor <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_id, vendedor_empresa_id,vendedor_ativo) %>%
  filter (vendedor_empresa_id == empresa, vendedor_ativo == 1) %>%
  collect()

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

##negocio_produto para pegar os valores de cada negócio
negocio_produto <- tbl(con, "negocio_produto") %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  collect()

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()

##join de negocios e vendedores
negocio_ij_vendedor <- inner_join(negocio, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

########## Para o funil e para agrupar por faturamento
#### Vou alterar o agrupamento de número de negócios para faturamento, portanto precisarei da tabela negocio_produto
neg_ij_ven_ij_np <- inner_join(negocio_ij_vendedor, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##Filtrando empresa, vendedores ativos e negocios de 2020
corte_1 <- neg_ij_ven_ij_np %>%
  filter(negocio_data_cadastro >= as.Date("2020-01-01"), negocio_negocio_situacao_id != 0)

##removendo elementos não mais usados
if (teste == F) {
  rm() #vou precisar dele pro funil mais pra baixo negocio_ij_historico_ij_vendedor_2020)
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

###Gráfico n0 - Faturamento dos negócios por vendedor (2020, status atual)
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

### Gráfico n0 - Número de clientes por vendedor
if(dash == F){
  n0
}

##vou manter ng_ij_vn_ij_np_fat pra fazer o join mais pro final
if (teste == F) {
  rm(ng_ij_vn_ij_np_fat, corte_1, status)
}
##############################################
##Faturamento de negócios fechados em 2020

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

ff <- c(4, 6, 7)

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

### Gráfico n3 - Faturamento de negócios fechados em 2020
#########################################################
n3 <- ggplot(ng_ij_vn_ij_np_fech_fat, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), total_fat, fill=factor(negocio_status),
                                          text = paste('Valor neste status:', total_fat_t))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.title = element_blank(), axis.text.x = element_blank())+
  scale_fill_manual(values = c("#32CD32", "orange", "#DE0D26"),)+
  #scale_y_continuous(labels = scales::label_number())+
  coord_flip(expand = F)

n3 <- ggplotly(n3, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.2, y = -0.05))

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

### Gráfico hc_n4 - Valor financeiro de negócios em 2020  (por categoria)
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

### Gráfico hc_n5 - Máquinas faturadas em 2020 (por categoria)
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
ng_ij_hist_ij_ven_emp <- negocio_ij_historico_ij_vendedor_total %>%
  filter (vendedor_ativo == TRUE, vendedor_empresa_id == empresa)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_emp <- ng_ij_hist_ij_ven_emp %>%
  filter(negocio_negocio_situacao_id != 0)

## Vou ordenar por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais novas, ficar só com a primeira atualização após mudança de status
ng_ij_hist_ij_ven_emp_idd <- ng_ij_hist_ij_ven_emp[order(ng_ij_hist_ij_ven_emp$historico_negocio_situacao_situacao_id, ng_ij_hist_ij_ven_emp$negocio_id), ]

##Aqui estou removendo duplicatas (de outra forma), que removem apenas quando o histórico_situacao e negocio_id se repetem
ng_ij_hist_ij_ven_emp_idd <- subset(ng_ij_hist_ij_ven_emp_idd, !duplicated(subset(ng_ij_hist_ij_ven_emp_idd, select=c(negocio_id))))

##filtrando apenas negócios não fechados negocio_situacao_id != 4,5,6,7,9
ff = c(4,5,6,7,9)

ng_ij_hist_ij_ven_ab <- ng_ij_hist_ij_ven_emp_idd %>%
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
  mutate(idade = (as.integer(today() - as_date(negocio_data_cadastro)))) %>%
  mutate(idade_cat = NA) %>%
  arrange(idade)

ng_ij_hist_ij_ven_idd$idade_cat <- with(ng_ij_hist_ij_ven_idd, cut(idade, breaks = c(0,60, 180, 365, 730, 10000),
                                                                   label = idades))

##Aqui eu posso fazer um group_by + summarise pra ter apenas coluna id_vendedor + count(negocios c/status) e depois o join, sem o select, ou então usar o mutate como foi feito
ng_ij_hist_ij_ven_num <- ng_ij_hist_ij_ven_idd %>%
  select(negocio_id, negocio_vendedor_id, vendedor_nome, idade, idade_cat) %>%
  group_by(idade_cat, negocio_vendedor_id) %>%
  mutate(num_negocios_idades = n()) %>%
  distinct (idade_cat, .keep_all = TRUE) %>%
  collect ()

##Usado pra ordenar o gráfico
ng_ij_hist_ij_ven_num$idade_cat = factor(ng_ij_hist_ij_ven_num$idade_cat, levels = c("Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses"))

n6 <- plot_ly(ng_ij_hist_ij_ven_num, type = 'bar', orientation = 'h', x=~num_negocios_idades , y=~vendedor_nome,
              color = ~idade_cat,
              colors = c("#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26"))
n6 <- n6 %>%
  layout(barmode = 'stack',
         xaxis = list(title = ''),
         yaxis = list(title = ''))
n6

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

### Gráfico n7 - Negócios abertos da empresa, pizza

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

### Gráfico n9 - Funil agrupado por faturamento
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

### Gráfico n10 - Pizza fechados do ano
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
##Criando nova tabela com dados apenas do mês anterior
##Auxiliares para meses
this_month <- today()
day(this_month) <- 1
last_month <- this_month-months(1)
ng_ij_hist_ij_ven_funil_fat_fec_2020_mes <- ng_ij_hist_ij_ven_fec_2020 %>%
  filter(last_month <= historico_negocio_situacao_data,  historico_negocio_situacao_data < this_month) %>%
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

### Gráfico n11 - Pizza fechados no último mês
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
  rm(colors_pie, st_f, status_f, this_month, last_month)
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

anos_ant = year(today()) - min(year(ng_ij_hist_ij_ven_ij_np_total$historico_negocio_situacao_data))

##primeiro vou selecionar por ano, e depois apenas status faturado
if(anos_ant > 0) {
  ng_ij_hist_ij_ven_ij_np_2019_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
    filter (historico_negocio_situacao_data < '2020-01-01' & historico_negocio_situacao_data > '2018-12-31' & negocio_negocio_situacao_id == 4)
}
if(anos_ant > 1) {
  ng_ij_hist_ij_ven_ij_np_2018_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
    filter (historico_negocio_situacao_data < '2019-01-01' & historico_negocio_situacao_data > '2017-12-31' & negocio_negocio_situacao_id == 4)
}

##Usando só os dois anos anteriores
#ng_ij_hist_ij_ven_ij_np_2017_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
#  filter (historico_negocio_situacao_data < '2018-01-01' & historico_negocio_situacao_data > '2016-12-31' & negocio_negocio_situacao_id == 4)

#Aqui vou fazer a mesma soma dos valores dos três anos anteriores, dividindo por mês
if(anos_ant > 0) {
  fat_2019_mes <- ng_ij_hist_ij_ven_ij_np_2019_fat %>%
    mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
    group_by (ym) %>%
    summarize(ym_sum_2019 = sum(np_valor)) %>%
    ungroup ()
}
if(anos_ant > 1) {
  fat_2018_mes <- ng_ij_hist_ij_ven_ij_np_2018_fat %>%
    mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
    group_by (ym) %>%
    summarize(ym_sum_2018 = sum(np_valor))%>%
    ungroup ()
}


###Fazer o gráfico de linhas com faturamento anual (substituindo com NA linhas faltantes)
###Na super temos 2018, na komatsu tem q verificar se possui 2018 e 2019
#######################################################################
n_linhas <- nrow(fat_2020_mes)
ym <- c("01","02","03","04","05","06","07","08","09","10","11","12")
fat_2020_mes_aux <- data.frame(ym)
fat_2020_mes_aux$ym_sum <- NA
fat_2020_mes_aux$ym_sum[1:n_linhas] <- fat_2020_mes$ym_sum

fat_2020_2019_2018_mes <- fat_2020_mes_aux
if(anos_ant > 0) {
  fat_2020_2019_2018_mes <- left_join(fat_2020_2019_2018_mes, fat_2019_mes, by = c("ym"))
}
if(anos_ant > 1) {
  fat_2020_2019_2018_mes <- left_join(fat_2020_2019_2018_mes, fat_2018_mes, by = c("ym"))
}

meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
## alterando pra números pra poder fazer da mesma forma
fat_2020_2019_2018_mes$ym <- as.integer(fat_2020_2019_2018_mes$ym)
##Vou usar dessa forma, ao invés de transformar, irei apenas criar uma nova coluna (poderia ser feito com um join também)
fat_2020_2019_2018_mes <- fat_2020_2019_2018_mes %>%
  mutate(mes = cut(ym,breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
                   labels = meses))
##Forma antiga, agora a nova mantém a coluna "identificadora"
#fat_2020_2019_2018_mes$ym <- with(fat_2020_2019_2018_mes, cut(ym, breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
#                                                              labels = meses))


#######################################################################

##Começando o gráfico
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
### Gráfico n12 - Faturamento anual (ano atual + dois anteriores)
n12 <- plot_ly(fat_2020_2019_2018_mes)
n12 <- n12 %>%
  add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y =~ym_sum,
            name = 'Faturamento de 2020',
            text = ~paste(func_fmt_din_mi(ym_sum),'milhões'),
            hoverinfo = "text",
            color = I("green")
  )
if(anos_ant > 0) {
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~ym_sum_2019, yaxis = ay,
              name = 'Faturamento de 2019',
              text = ~paste(func_fmt_din_mi(ym_sum_2019),'milhões'),
              hoverinfo = "text",
              color = I("#1E90FF"))
}
if(anos_ant > 1) {
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~ym_sum_2018, yaxis = ay,
              name = 'Faturamento de 2018',
              text = ~paste(func_fmt_din_mi(ym_sum_2018),'milhões'),
              hoverinfo = "text",
              color = I("#4682B4"))
}

n12 <- n12 %>%
  layout(xaxis = list(title = ''), yaxis = list(title = ''),
         #aqui eu ajusto onde quero que apareça a legenda
         legend = list(x=0.8, y=0.9)#)
  )
if (dash == F){
  n12
}


if (teste == F){
  #tabelas
  rm(ng_ij_hist_ij_ven_ij_np_2020, fat_2020_mes, ng_ij_hist_ij_ven_total, negocio_ij_historico_ij_vendedor_total,
     fat_2019_mes, fat_2018_mes, fat_2020_2019_2018_mes, fat_2020_mes_aux, ay, a, negocio_produto,
     ng_ij_hist_ij_ven_2020_fat, ng_ij_hist_ij_ven_ij_np_2019_fat, ng_ij_hist_ij_ven_ij_np_2018_fat,
     ng_ij_hist_ij_ven_ij_np_total)
  #variáveis
  rm(meses, n_linhas, ym, anos_ant)
}

##############################################
###Tempo médio de vida de um negócio

ng_cad_fin <- ng_ij_hist_ij_ven_emp %>%
  select (negocio_id, negocio_data_cadastro, negocio_negocio_situacao_id, historico_negocio_situacao_situacao_id, historico_negocio_situacao_data) %>%
  arrange (desc(historico_negocio_situacao_data), negocio_id)

##Aqui estou removendo duplicatas (de outra forma), que removem apenas quando negocio_id se repete
ng_cad_fin <- subset(ng_cad_fin, !duplicated(subset(ng_cad_fin, select=c(negocio_id))))

##Já estou filtrando apenas status fechados (faturado, financiamento não aprovado, desistência do cliente, perdemos para a concorrência) 
ng_cad_fin <- ng_cad_fin %>%
  filter(negocio_negocio_situacao_id == 4 | negocio_negocio_situacao_id == 5 | negocio_negocio_situacao_id == 6 | negocio_negocio_situacao_id == 7) %>%
  mutate(temp_d = date(historico_negocio_situacao_data) - date(negocio_data_cadastro))

##Calculando tempo médio
ng_cad_fin <- ng_cad_fin %>%
  group_by(negocio_negocio_situacao_id) %>%
  mutate(media_d = round(mean(temp_d)), 2) %>%
  distinct(negocio_negocio_situacao_id, .keep_all = T) %>%
  ungroup() %>%
  select (negocio_negocio_situacao_id, media_d)


##Só pra ver os nomes
ng_cad_fin <- ng_cad_fin %>%
  mutate(Status = case_when(negocio_negocio_situacao_id == 4 ~ "Faturado",
                            negocio_negocio_situacao_id == 5 ~ "Financ. não aprovado",
                            negocio_negocio_situacao_id == 6 ~ "Desist. do cliente",
                            negocio_negocio_situacao_id == 7 ~ "Perdemos para conc."))
#ng_cad_fin <- ng_cad_fin %>%
#  mutate(Status = as.factor(Status))

ng_cad_fin <- ng_cad_fin %>%
  mutate (media_d = paste(as.character(media_d),"dias")) %>%
  arrange(negocio_negocio_situacao_id)
valuebox <- data.frame(
  x = rep(seq(2, 9, 6.5), 2),
  y = c(rep(6.5, 2), rep(2,2)),
  h = rep(4.25, 4),
  w = rep(6.25, 4),
  value = ng_cad_fin$media_d,
  info = ng_cad_fin$Status,
  shape = c(fontawesome('fa-calendar'), fontawesome('fa-calendar'), fontawesome('fa-calendar'), fontawesome('fa-calendar')),
  font_family = c(rep("fontawesome-webfont", 4)),
  color = factor(1:4))
### Gráfico n13 - Tempo de vida médio de um negócio faturado (status = faturado)
colors <- c("#32CD32", "#FFD700" , "orange" , "#DE0D26")
n13 <- ggplot(valuebox, aes(x=x, y=y, height = h, width = w, label = info)) +
  geom_tile(aes(fill = color)) +
  geom_text(color = "white", fontface = "bold", size = 18,
            aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
  geom_text(color = "white", fontface = "bold", size = 12,
            aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
  coord_fixed() +
  scale_fill_manual(values = colors) +
  geom_text(size = 30, aes(label = shape, family = font_family,
                           x = x + 1.5, y = y + 0.5), alpha = 0.25) +
  theme_void() +
  guides(fill = FALSE)

if (dash == F){
  n13
}

### Gráfico n14 - Tempo de vida médio de um negócio perdido (status = desistência do cliente ou perdemos para concorrência)

if (dash == F){
  #n14
}

if (teste == F){
  #tabelas
  rm(ng_ij_hist_ij_ven_emp, valuebox, ng_cad_fin, ng_ij_hist_ij_ven_emp)
  #variáveis
  rm()
}


################################################################################################################################
#############################################################################################################################
###Contar clientes/visitas/negocios cadastrados por mês -> vem do script visita_clientes
####################################

cliente <- tbl(con,'cliente') %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_cidade, cliente_ultima_visita) %>%
  filter(cliente_empresa_id == empresa) %>%
  collect()

##Collect cria o df resultado da query, nesse caso, visitas_cliente, já filtrando apenas ano atual
visita_cliente <- tbl(con,'visita_cliente') %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro) %>%
  filter (vc_data_cadastro >= ano_atual) %>%
  collect ()

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

##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id'))

##Já filtrado apenas empresa (variavel global)
clientes_mes <- cliente %>%
  filter (cliente_data_cadastro >= ano_atual) %>%
  mutate (ym = format(cliente_data_cadastro, '%m')) %>%
  group_by (ym) %>%
  summarize(n_cli = n()) %>%
  ungroup ()

##Visita precisa juntar com visita_status ou _resultado () pra obter empresa_id ##poderia ser vendedor, mas a tabela vis_st_emp (status c/ status_empresa) já está pronta
##vis_st_emp já vem filtrada pela empresa (var global)
vc_ij_emp <- inner_join(visita_cliente, vis_st_emp, by = c("vc_status_id" = "vs_id")) %>%
  #select(vc_id, vc_status_id, vc_data_cadastro) ##Sem empresa_id
  select(vc_id, vc_status_id, vc_data_cadastro, vse_empresa_id)

visitas_mes <- vc_ij_emp %>%
  filter (vc_data_cadastro >= ano_atual) %>%
  mutate (ym = format(vc_data_cadastro, '%m')) %>%
  group_by (ym) %>%
  summarize(n_vis = n()) %>%
  ungroup ()

##Negocio precisa juntar com vendedor pra obter empresa_id ##Poderia ser cliente também, mas a tabela vendedor é menor
##vendedor já vem filtrado pela empresa (var global)
ng_ij_emp <- inner_join(negocio, vendedor, by = c("negocio_vendedor_id" = "vendedor_id")) %>%
  #select(negocio_id, negocio_vendedor_id, negocio_data_cadastro)%>% ##Sem empresa_id
  select(negocio_id, negocio_vendedor_id, negocio_data_cadastro, vendedor_empresa_id)

negocios_mes <- ng_ij_emp %>%
  filter (negocio_data_cadastro >= ano_atual) %>%
  mutate (ym = format(negocio_data_cadastro, '%m')) %>%
  group_by (ym) %>%
  summarize(n_neg = n()) %>%
  ungroup ()

##Juntando tudo em um só dataframe
cli_ij_vc_mes <- inner_join(clientes_mes, visitas_mes, by = c("ym"))
cli_ij_vc_ij_ng_mes <- inner_join(cli_ij_vc_mes, negocios_mes, by = c("ym"))


##Criando nomes de colunas
meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
## alterando pra números pra poder fazer da mesma forma
cli_ij_vc_ij_ng_mes$ym <- as.integer(cli_ij_vc_ij_ng_mes$ym)

##Vou usar dessa forma, ao invés de transformar, irei apenas criar uma nova coluna (poderia ser feito com um join também)
cli_ij_vc_ij_ng_mes <- cli_ij_vc_ij_ng_mes %>%
  mutate(mes = cut(ym,breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
                   labels = meses))
###Não farei dessa forma pois quero manter a coluna
#cli_ij_vc_ij_ng_mes$ym <- with(cli_ij_vc_ij_ng_mes, cut(ym, breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
#                                                        labels = meses))

### Gráfico c3 - Cadastro de clientes, visitas e negócios no ano de 2020
c3 <- plot_ly(cli_ij_vc_ij_ng_mes, type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~n_cli,
              name = 'Clientes',
              text = ~paste(func_fmt_numbr(n_cli), 'clientes'),
              hoverinfo = "text",
              color = I('#7B68EE'))
c3 <- c3 %>%
  add_trace (type = 'scatter', mode = 'lines+markers', y = ~n_vis,
             name = 'Visitas',
             text = ~paste(func_fmt_numbr(n_vis), 'visitas'),
             hoverinfo = "text",
             color = I('#DAA520'))
c3 <- c3 %>%
  add_trace (type = 'scatter', mode = 'lines+markers', y = ~n_neg,
             name = 'Negócios',
             text = ~paste(func_fmt_numbr(n_neg), 'negócios'),
             hoverinfo = "text",
             color = I('green'))

c3 <- c3 %>%
  layout(xaxis = list(title = '', range = c(min(0), max(12))), ##Dessa forma pego os 12 meses do ano
         yaxis = list(title = ''))
if(dash == F){
  c3
}

if(teste == F){
  #tabelas
  rm(cliente, visita_cliente, negocio, vendedor, vis_st_emp, cli_ij_vc_mes, cli_ij_vc_ij_ng_mes,
     clientes_mes, negocios_mes, ng_ij_emp, visitas_mes, vc_ij_emp, visita_status, visita_status_empresa);
  #variáveis
  rm(meses);
}
#subplot(n12, c3, nrows = 2, shareX = T)
################################################################################################################################

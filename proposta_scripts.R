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
teste = F
dash = F

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS01", 
                      Database = "nhmobile_agriculture", 
                      Trusted_Connection = "True")

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
##coleta todas as propostas
proposta <- tbl(con, "proposta") %>%
  select(proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status) %>%
  collect()

##coleta todos os negócios
negocio <- tbl(con, "negocio") %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_vendedor_id, negocio_usado) %>%
  collect()

##coleta todos os vendedores
vendedor <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_id, vendedor_empresa_id, vendedor_ativo) %>%
  collect()

##Aqui vou filtrar os ativos e já vou filtrar a empresa
vendedor <- vendedor %>%
  filter(vendedor_ativo == TRUE)

##junção de proposta com negócio
prop_ij_neg <- inner_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado)

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_ij_neg <- prop_ij_neg[!is.na(prop_ij_neg$negocio_vendedor_id),]

if (teste){
  rm(proposta, negocio)
}

prop_ij_neg_2020 <- prop_ij_neg %>%
  filter(proposta_data_cadastro >= '2020-01-01')

##Aqui tenho a contagem de status da empresa
prop_ij_neg_cont <- prop_ij_neg_2020 %>%
  select (proposta_status) %>%
  group_by(proposta_status) %>%
  mutate(cont_status = n()) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  collect()

status = c("0 - Pendente", "1 - Aceito", "2 - Recusado", "3 - Cancelado", "4 - Finalizado")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_cont$proposta_status <- with(prop_ij_neg_cont, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                               labels = status))

##Juntando com vendedor pra obter o nome do vendedor
prop_ij_neg_ij_vend <- inner_join(prop_ij_neg, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

##Filtrando a empresa
prop_ij_neg_ij_vend <- prop_ij_neg_ij_vend %>%
  filter(vendedor_empresa_id == 16, proposta_data_cadastro >= '2020-01-01')

#aqui eu estou alterando o joão paulo, que havia problemas com codificação
prop_ij_neg_ij_vend$vendedor_nome[prop_ij_neg_ij_vend$negocio_vendedor_id == 45] <- "JOÃO PAULO"

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_ij_neg_ij_vend <- prop_ij_neg_ij_vend[!is.na(prop_ij_neg_ij_vend$negocio_vendedor_id),]

##Aqui tenho a contagem de status por vendedor
prop_ij_neg_cont_vend <- prop_ij_neg_ij_vend %>%
  select (negocio_vendedor_id, negocio_vendedor_id, vendedor_nome, proposta_status) %>%
  group_by(proposta_status, negocio_vendedor_id) %>%
  mutate(cont_status = n()) %>%
  distinct(negocio_vendedor_id, proposta_status, .keep_all = TRUE) %>%
  collect()

##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_cont_vend$proposta_status <- with(prop_ij_neg_cont_vend, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                         labels = status))

##Gráfico 6 - Número propostas, por tipo, por vendedor (total)
p0 <- ggplot(prop_ij_neg_cont_vend, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), cont_status, fill=factor(proposta_status), label = cont_status, 
                                        text = paste('Número de pedidos nesta categoria:', cont_status))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))+
  coord_flip(expand = F)

p0 <- ggplotly(p0, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.3, y = -0.15))

if(dash == F){
  p0
}

###Gráfico 7 - Número propostas, por tipo em 2020 (total)
p1 <- ggplot(prop_ij_neg_cont, aes(x = proposta_status, y = cont_status, fill=as.factor(proposta_status), 
                                   text = paste('Número de pedidos nesta categoria:', cont_status))) +
  geom_col(position = "identity") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))
p1 <- ggplotly(p1, tooltip = 'text') %>%
  layout(showlegend = FALSE)
if(dash == F){
  p1
}
if(teste == F){
  #tabelas
  rm(negocio, proposta, vendedor, prop_ij_neg_2020, prop_ij_neg_cont, prop_ij_neg_cont_vend, prop_ij_neg_ij_vend)
  #variáveis
  rm(status)
}
##############################################

##Propostas por categoria (usado)
##############################################
##Aqui tenho a contagem de usados em propostasda empresa
prop_ij_neg_cont_us <- prop_ij_neg %>%
  select (negocio_usado) %>%
  group_by(negocio_usado) %>%
  mutate(usado = n()) %>%
  distinct(negocio_usado, .keep_all = TRUE) %>%
  collect()

total = sum(prop_ij_neg_cont_us$usado)

prop_ij_neg_cont_us <- prop_ij_neg_cont_us %>%
  mutate(total = total)


prop_ij_neg_cont_us$negocio_usado[prop_ij_neg_cont_us$negocio_usado == TRUE] <- "Proposta com usado"
prop_ij_neg_cont_us$negocio_usado[prop_ij_neg_cont_us$negocio_usado == FALSE] <- "Proposta sem usado"

###Gráfico 8 - Proporção de usados (pizza)
p2 <- plot_ly(prop_ij_neg_cont_us, labels = ~negocio_usado, values = ~usado, type = 'pie', sort = F,
              texttemplate = "%{value} (%{percent})",
              hovertemplate = paste ("%{label} <br>",
                                     "%{value} <br>",
                                     "Equivalente a %{percent} do total",
                                     "<extra></extra>"))

if(dash == F){
  p2
}

if(teste == F){
  #tabelas
  rm(prop_ij_neg, prop_ij_neg_cont_us)
  #variáveis
  rm(total)
}

##############################################
### Ticket médio por proposta

### Uma proposta tem n proposta_pagamento (cuidar as ativas, pp_ativo = 1)
##coleta todos proposta_pagamenmto
proposta_pagamento <- tbl(con, "proposta_pagamento") %>%
  select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
  collect()

pr_ij_pp <- inner_join(proposta, proposta_pagamento, by = c("proposta_id = pp_proposta_id"))

if(teste == F){
  #tabelas
  rm(proposta, proposta_pagamento, pr_ij_pp)
  #variáveis
  rm(status)
}
##############################################

if (teste == 0) {
  #rm(list=ls())
}


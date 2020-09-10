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

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
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
prop_lj_neg <- left_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado)

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_lj_neg <- prop_lj_neg[!is.na(prop_lj_neg$negocio_vendedor_id),]

rm(proposta, negocio)

prop_lj_neg_2020 <- prop_lj_neg %>%
  filter(proposta_data_cadastro >= '2020-01-01')

##Aqui tenho a contagem de status da empresa
prop_lj_neg_cont <- prop_lj_neg_2020 %>%
  select (proposta_status) %>%
  group_by(proposta_status) %>%
  mutate(cont_status = n()) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  collect()

prop_lj_neg_cont$proposta_status[prop_lj_neg_cont$proposta_status == 0] <- "0 - Pendente"
prop_lj_neg_cont$proposta_status[prop_lj_neg_cont$proposta_status == 1] <- "1 - Aceito"
prop_lj_neg_cont$proposta_status[prop_lj_neg_cont$proposta_status == 2] <- "2 - Recusado"
prop_lj_neg_cont$proposta_status[prop_lj_neg_cont$proposta_status == 3] <- "3 - Cancelado"
prop_lj_neg_cont$proposta_status[prop_lj_neg_cont$proposta_status == 4] <- "4 - Finalizado"


##Juntando com vendedor pra obter o nome do vendedor
prop_lj_neg_lj_vend <- left_join(prop_lj_neg, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

##Filtrando a empresa
prop_lj_neg_lj_vend <- prop_lj_neg_lj_vend %>%
  filter(vendedor_empresa_id == 16, proposta_data_cadastro >= '2020-01-01')

#aqui eu estou alterando o joão paulo, que havia problemas com codificação
prop_lj_neg_lj_vend$vendedor_nome[prop_lj_neg_lj_vend$negocio_vendedor_id == 45] <- "JOÃO PAULO"

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_lj_neg_lj_vend <- prop_lj_neg_lj_vend[!is.na(prop_lj_neg_lj_vend$negocio_vendedor_id),]

##Aqui tenho a contagem de status por vendedor
prop_lj_neg_cont_vend <- prop_lj_neg_lj_vend %>%
  select (negocio_vendedor_id, negocio_vendedor_id, vendedor_nome, proposta_status) %>%
  group_by(proposta_status, negocio_vendedor_id) %>%
  mutate(cont_status = n()) %>%
  distinct(negocio_vendedor_id, proposta_status, .keep_all = TRUE) %>%
  collect()

prop_lj_neg_cont_vend$proposta_status[prop_lj_neg_cont_vend$proposta_status == 0] <- "0 - Pendente"
prop_lj_neg_cont_vend$proposta_status[prop_lj_neg_cont_vend$proposta_status == 1] <- "1 - Aceito"
prop_lj_neg_cont_vend$proposta_status[prop_lj_neg_cont_vend$proposta_status == 2] <- "2 - Recusado"
prop_lj_neg_cont_vend$proposta_status[prop_lj_neg_cont_vend$proposta_status == 3] <- "3 - Cancelado"
prop_lj_neg_cont_vend$proposta_status[prop_lj_neg_cont_vend$proposta_status == 4] <- "4 - Finalizado"

##Gráfico do número propostas, por tipo, por vendedor (total)
##############################################
p0 <- ggplot(prop_lj_neg_cont_vend, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), cont_status, fill=factor(proposta_status), label = cont_status)) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))+
  coord_flip(expand = F)
#geom_text(position = position_stack(vjust = +0.5))

##Se usar ggplot vai pro lugar, se usar o plotly tem q mover como fiz abaixo
ggplotly(p0) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.25))
##############################################

##Gráfico do número propostas, por tipo (total)
##############################################
p1 <- ggplot(prop_lj_neg_cont, aes(x = proposta_status, y = cont_status, fill=as.factor(proposta_status))) +
  geom_col(position = "dodge") +
  theme (axis.text.x = element_text(angle = 20, hjust = 1), axis.title = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))

ggplotly(p1) %>%
  layout(showlegend = FALSE) #remove a legenda (factor)

#gp1 <- ggplotly(p1)
#gp1 <- style(gp1, line = list(color = 'gold'), hoverinfo = "y", traces = 1)

#gp1

##############################################

##Propostas por categoria (usado)
##############################################
##Aqui tenho a contagem de usados em propostasda empresa
prop_lj_neg_cont_us <- prop_lj_neg %>%
  select (negocio_usado) %>%
  group_by(negocio_usado) %>%
  mutate(usado = n()) %>%
  distinct(negocio_usado, .keep_all = TRUE) %>%
  collect()

total = sum(prop_lj_neg_cont_us$usado)

prop_lj_neg_cont_us <- prop_lj_neg_cont_us %>%
  mutate(total = total)

prop_lj_neg_cont_us$negocio_usado[prop_lj_neg_cont_us$negocio_usado == TRUE] <- "Proposta com usado"
prop_lj_neg_cont_us$negocio_usado[prop_lj_neg_cont_us$negocio_usado == FALSE] <- "Proposta sem usado"

###Gráfico de pizza
##############################################
p2 <- plot_ly(prop_lj_neg_cont_us, labels = ~negocio_usado, values = ~usado, type = 'pie')
p2 <- p2 %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p2
  
##############################################  

if (teste == 0) {
  #rm(list=ls())
}


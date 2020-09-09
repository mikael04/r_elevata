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

prop_lj_negocio <- left_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado)

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_lj_negocio <- prop_lj_negocio[!is.na(prop_lj_negocio$negocio_vendedor_id),]

rm(proposta, negocio)

##Aqui tenho a contagem de status da empresa
prop_lj_neg_cont <- prop_lj_negocio %>%
  select (proposta_status) %>%
  group_by(proposta_status) %>%
  mutate(Cont_status = n()) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  collect()

##Juntando com vendedor pra obter o nome do vendedor
prop_lj_neg_lj_vend <- left_join(prop_lj_negocio, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

##Filtrando a empresa
prop_lj_neg_lj_vend <- prop_lj_neg_lj_vend %>%
  filter(vendedor_empresa_id == 16)

#aqui eu estou alterando o joão paulo, que havia problemas com codificação
prop_lj_neg_lj_vend$vendedor_nome[prop_lj_neg_lj_vend$negocio_vendedor_id == 45] <- "JOÃO PAULO"

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_lj_neg_lj_vend <- prop_lj_neg_lj_vend[!is.na(prop_lj_neg_lj_vend$negocio_vendedor_id),]

##Aqui tenho a contagem de status por vendedor
prop_lj_neg_cont_vend <- prop_lj_neg_lj_vend %>%
  select (negocio_vendedor_id, negocio_vendedor_id, vendedor_nome, proposta_status) %>%
  group_by(proposta_status, negocio_vendedor_id) %>%
  mutate(Cont_status = n()) %>%
  distinct(negocio_vendedor_id, proposta_status, .keep_all = TRUE) %>%
  collect()


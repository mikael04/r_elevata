rm(list = ls())

library(data.table)
library(dplyr)
source("Geradores_tabelas_html/gera_tabelas_html_avaliacoes.R")
source("Geradores_tabelas_html/avaliacoes/script_modifica_tabela_avaliacoes.R")

## setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
debug = T
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas_mapas

# Propostas
# Avaliacoes
# Visitas

empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
  select(empresa_id)

params_list_i <- empresas_ativas$empresa_id
empresas_ativ <- as.list(params_list_i)
## 0 -> indica que estamos rodando para o dia atual
num_dias_list <- as.list(0)

## Gerando as tabelas (lendo tabelas base e fazendo manipulação
## Após manipulação, gera o html intermediário (que será alterado, para que a cópia dele seja adicionado a pasta html final)
## arquivos salvos em Geradores_tabelas_html/propostas/empresas/
vendedores_empresa <- fct_gera_tabelas_avaliacoes(debug)

## Arquivos salvos em Geradores_tabelas_html/propostas/html_intermed, html que será alterado
## Geradores_tabelas_html/propostas/htmls_final (, "final") -> com arquivo copiado final

## Manipula todas as tabelas, adicionando itens de html/css necessários para formatação,
## arquivos permanecem em html_intermed e são copiados para html_final
fct_alt_todas_html('avaliacoes', vendedores_empresa, empresas_ativas, debug)
#############################################################

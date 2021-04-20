rm(list = ls())

library(data.table)
library(dplyr)
source("Geradores_tabelas_html/propostas/script_modifica_tabela_propostas.R")

## setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
teste = T
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

for(i in 1:length(empresas_ativ)){
  fct_cria_tabelas_html(empresas_ativ[[i]], 'propostas', teste)
}

fct_alt_todas_html('propostas', teste)
#############################################################

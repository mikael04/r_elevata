library(purrr)
library(data.table)
library(dplyr)

setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
teste = F
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas_mapas

# Propostas
# Avaliacoes
# Visitas

empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
  select(empresa_id)

params_list_i <- empresas_ativas$empresa_id
params_list <- as.list(params_list_i)
## 0 -> indica que estamos rodando para o dia atual
num_dias_list <- as.list(0)
x <- length(params_list)

empresas_ativas[1]

#############################################################

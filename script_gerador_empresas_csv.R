rm(list = ls())
library(odbc)
library(dplyr)
###################################

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = 'SQL Server',
                      Server = 'localhost\\SQLEXPRESS01',
                      Database = 'nhmobile_agriculture',
                      Trusted_Connection = 'True')


##Pegando a tabela de empresas
empresa <- tbl(con,'empresa') %>%
  select (empresa_id, empresa_nome, empresa_ativo) %>%
  filter (empresa_ativo == '1' & empresa_id > '0') %>%
  select (-empresa_ativo) %>%
  collect ()
#Arrumando encoding
Encoding(empresa$empresa_nome) <- 'latin1'

data.table::fwrite(empresa, file="empresas_ativas_id.csv", sep = ";")


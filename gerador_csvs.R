library(odbc)
library(data.table)
library(dplyr)

##Conectando com o SQL
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = 'SQL Server',
                      Server = 'localhost\\SQLEXPRESS01',
                      Database = 'nhmobile_agriculture',
                      Trusted_Connection = 'True')

## Coletando os csvs
#categoria, empresa, forma_faturamento, forma_faturamento_empresa, historico_negocio_situacao, inst_financeira,
#inst_financeira_empresa, marca, marca_categoria, marca_empresa, negocio, negocio_produto, negocio_situacao,
#negocio_usado, negocio_vendedor, parque_maquina, produto, proposta, proposta_modo_forma, proposta_pagamento,
#proposta_pagamento_usado, proposta_produto, regiao, vendedor, visita_cliente, visita_resultado,
#visita_resultado_empresa, visita_status, visita_status_empresa



##Collect cria o df resultado da query, nesse caso, 
categoria <- tbl(con,'categoria') %>%
  collect ()

data.table::fwrite(visita_cliente, file="categoria.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
cliente <- tbl(con,'cliente') %>%
  collect ()

data.table::fwrite(visita_cliente, file="cliente.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
empresa <- tbl(con,'empresa') %>%
  collect ()

data.table::fwrite(visita_cliente, file="empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
forma_faturamento <- tbl(con,'forma_faturamento') %>%
  collect ()

data.table::fwrite(visita_cliente, file="forma_faturamento.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
forma_faturamento_empresa <- tbl(con,'forma_faturamento_empresa') %>%
  collect ()

data.table::fwrite(visita_cliente, file="forma_faturamento_empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
historico_negocio_situacao <- tbl(con,'historico_negocio_situacao') %>%
  collect ()

data.table::fwrite(visita_cliente, file="historico_negocio_situacao.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
inst_financeira <- tbl(con,'inst_financeira') %>%
  collect ()

data.table::fwrite(visita_cliente, file="inst_financeira.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
inst_financeira_empresa <- tbl(con,'inst_financeira_empresa') %>%
  collect ()

data.table::fwrite(visita_cliente, file="inst_financeira_empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
marca <- tbl(con,'marca') %>%
  collect ()

data.table::fwrite(visita_cliente, file="marca.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
 <- tbl(con,'') %>%
  collect ()

data.table::fwrite(visita_cliente, file=".csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, visitas_cliente
visita_cliente <- tbl(con,'visita_cliente') %>%
  collect ()

data.table::fwrite(visita_cliente, file="visita_cliente.csv", sep = ";")

#vc <- fread(file =  ".csv", sep = ";", header = T)

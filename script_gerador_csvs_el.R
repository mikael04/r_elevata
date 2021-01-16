library(odbc)
library(data.table)
library(dplyr)


setwd("C:\\Users\\sysadmin\\Documents\\R\\Scripts")

##Conectando com o SQL
con <- DBI::dbConnect(odbc::odbc(), "SQL",
                      UID = "backup_auto", PWD='Elev@t@1406', database = 'nhmobile_agriculture_backup')


## Coletando os csvs
#categoria, empresa, forma_faturamento, forma_faturamento_empresa, historico_negocio_situacao, inst_financeira,
#inst_financeira_empresa, marca, marca_categoria, marca_empresa, negocio, negocio_produto, negocio_situacao,
#negocio_usado, negocio_vendedor, parque_maquina, produto, proposta, proposta_modo_forma, proposta_pagamento,
#proposta_pagamento_usado, proposta_produto, regiao, vendedor, visita_cliente, visita_resultado,
#visita_resultado_empresa, visita_status, visita_status_empresa



##Collect cria o df resultado da query, nesse caso, 
categoria <- tbl(con,'categoria') %>%
  collect ()

data.table::fwrite(categoria, file="Tabelas/categoria.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
cliente <- tbl(con,'cliente') %>%
  select (cliente_id, cliente_nome, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, 
          cliente_latitude, cliente_longitude, cliente_ultima_visita, cliente_ultimo_negocio) %>%
  collect ()

data.table::fwrite(cliente, file="Tabelas/cliente.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
empresa <- tbl(con,'empresa') %>%
  select (empresa_id, empresa_nome, empresa_data_cadastro, empresa_ativo) %>%
  collect ()

##Pegando a tabela de empresas
empresa_a <- empresa %>%
  select (empresa_id, empresa_nome, empresa_ativo) %>%
  filter (empresa_ativo == T & empresa_id > '0') %>%
  select (-empresa_ativo) %>%
  collect ()

#Arrumando encoding
Encoding(empresa_a$empresa_nome) <- 'latin1'
#escrevendo empresas ativas
data.table::fwrite(empresa_a, file="Tabelas/empresas_ativas_id.csv", sep = ";")
#escrevendo tabela empresas
data.table::fwrite(empresa, file="Tabelas/empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
forma_faturamento <- tbl(con,'forma_faturamento') %>%
  collect ()

data.table::fwrite(forma_faturamento, file="Tabelas/forma_faturamento.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
forma_faturamento_empresa <- tbl(con,'forma_faturamento_empresa') %>%
  collect ()

data.table::fwrite(forma_faturamento_empresa, file="Tabelas/forma_faturamento_empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
historico_negocio_situacao <- tbl(con,'historico_negocio_situacao') %>%
  collect ()

data.table::fwrite(historico_negocio_situacao, file="Tabelas/historico_negocio_situacao.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
inst_financeira <- tbl(con,'inst_financeira') %>%
  select (instfinanceira_id, instfinanceira_nome, instfinanceira_ativo) %>%
  collect ()

data.table::fwrite(inst_financeira, file="Tabelas/inst_financeira.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
inst_financeira_empresa <- tbl(con,'inst_financeira_empresa') %>%
  select (ife_instfinanceira_id, ife_empresa_id, ife_ativo) %>%
  collect ()

data.table::fwrite(inst_financeira_empresa, file="Tabelas/inst_financeira_empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
marca <- tbl(con,'marca') %>%
  collect ()

data.table::fwrite(marca, file="Tabelas/marca.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
marca_categoria <- tbl(con,'marca_categoria') %>%
  collect ()

data.table::fwrite(marca_categoria, file="Tabelas/marca_categoria.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
marca_empresa <- tbl(con,'marca_empresa') %>%
  collect ()

data.table::fwrite(marca_empresa, file="Tabelas/marca_empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
negocio <- tbl(con,'negocio') %>%
  select (negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_produto_id,
          negocio_data_cadastro, negocio_usado, negocio_cliente_id, negocio_tipo_negocio,
          negocio_cliente_vendedor_excluido, negocio_forma_faturamento_id,
          negocio_inst_financeira_id, negocio_probabilidade) %>%
  collect ()

data.table::fwrite(negocio, file="Tabelas/negocio.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
negocio_produto <- tbl(con,'negocio_produto') %>%
  select (np_id, np_negocio_id, np_produto_id, np_quantidade, np_ativo, np_valor) %>%
  collect ()

data.table::fwrite(negocio_produto, file="Tabelas/negocio_produto.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
negocio_situacao <- tbl(con,'negocio_situacao') %>%
  collect ()

data.table::fwrite(negocio_situacao, file="Tabelas/negocio_situacao.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
negocio_usado <- tbl(con,'negocio_usado') %>%
  select (nu_id, nu_negocio_id, nu_produto_id, nu_ano, nu_valor, nu_estado, nu_vendedor_nome, nu_dt_cadastro,
          nu_ativo, nu_quantidade, nu_excluido, nu_empresa_id) %>%
  collect ()

data.table::fwrite(negocio_usado, file="Tabelas/negocio_usado.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
negocio_vendedor <- tbl(con,'negocio_vendedor') %>%
  collect ()

data.table::fwrite(negocio_vendedor, file="Tabelas/negocio_vendedor.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
parque_maquina <- tbl(con,'parque_maquina') %>%
  select (pm_id, pm_cliente_id, pm_produto_id, pm_ano_modelo, pm_data_cadastro, pm_quantidade,
          pm_ativo, pm_comprado_novo, pm_ano_comprado, pm_potencia, pm_qtd_horas) %>%
  collect ()

data.table::fwrite(parque_maquina, file="Tabelas/parque_maquina.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
produto <- tbl(con,'produto') %>%
  select (produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_ativo,
          produto_empresa_id) %>%
  collect ()

data.table::fwrite(produto, file="Tabelas/produto.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
proposta <- tbl(con,'proposta') %>%
  select (proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status) %>%
  collect ()

data.table::fwrite(proposta, file="Tabelas/proposta.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
proposta_modo_forma <- tbl(con,'proposta_modo_forma') %>%
  collect ()

data.table::fwrite(proposta_modo_forma, file="Tabelas/proposta_modo_forma.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
proposta_pagamento <- tbl(con,'proposta_pagamento') %>%
  collect ()

data.table::fwrite(proposta_pagamento, file="Tabelas/proposta_pagamento.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
proposta_pagamento_usado <- tbl(con,'proposta_pagamento_usado') %>%
  collect ()

data.table::fwrite(proposta_pagamento_usado, file="Tabelas/proposta_pagamento_usado.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
proposta_produto <- tbl(con,'proposta_produto') %>%
  select (pp_id, pp_proposta_id, pp_versao, pp_produto_id, pp_valor, pp_ativo, pp_quantidade,
          pp_negocio_produto_id) %>%
  collect ()

data.table::fwrite(proposta_produto, file="Tabelas/proposta_produto.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
regiao <- tbl(con,'regiao') %>%
  collect ()

data.table::fwrite(regiao, file="Tabelas/regiao.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
vendedor <- tbl(con,'vendedor') %>%
  select (vendedor_id, vendedor_nome, vendedor_ativo, vendedor_empresa_id, vendedor_data_cadastro) %>%
  collect ()

data.table::fwrite(vendedor, file="Tabelas/vendedor.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
visita_cliente <- tbl(con,'visita_cliente') %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id,
          vc_data_cadastro, vc_latitude, vc_longitude) %>%
  collect ()

data.table::fwrite(visita_cliente, file="Tabelas/visita_cliente.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
visita_resultado <- tbl(con,'visita_resultado') %>%
  select (vr_id, vr_nome, vr_ativo) %>%
  collect ()

data.table::fwrite(visita_resultado, file="Tabelas/visita_resultado.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
visita_resultado_empresa <- tbl(con,'visita_resultado_empresa') %>%
  select (vre_resultado_id, vre_empresa_id, vre_ativo) %>%
  collect ()

data.table::fwrite(visita_resultado_empresa, file="Tabelas/visita_resultado_empresa.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
visita_status <- tbl(con,'visita_status') %>%
  select (vs_id, vs_nome, vs_ativo) %>%
  collect ()

data.table::fwrite(visita_status, file="Tabelas/visita_status.csv", sep = ";")

##Collect cria o df resultado da query, nesse caso, 
visita_status_empresa <- tbl(con,'visita_status_empresa') %>%
  select (vse_status_id, vse_empresa_id, vse_ativo) %>%
  collect ()

data.table::fwrite(visita_status_empresa, file="Tabelas/visita_status_empresa.csv", sep = ";")
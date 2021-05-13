## Esse script irá executar os demais scripts, portanto será necessário rodar apenas ele

source("Geradores_tabelas_html/script_tabelas_avaliacoes_html.R")
source("Geradores_tabelas_html/script_tabelas_negocios_html.R")
source("Geradores_tabelas_html/script_tabelas_propostas_html.R")
source("Geradores_tabelas_html/script_tabelas_visitas_html.R")

## Gera tabelas de avaliações
func_tabela_avaliacoes(debug = T)

## Gera tabelas de negocios
func_tabela_negocios(debug = T)

## Gera tabelas de propostas
func_tabela_propostas(debug = T)

## Gera tabelas de visitas
func_tabela_visitas(debug = T)

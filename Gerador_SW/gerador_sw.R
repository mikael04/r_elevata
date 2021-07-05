rm(list = ls())

library(dplyr)
## Arquivo de saída, sw.js
sw_emp <- file("Gerador_SW/sw_emp.js")
## Arquivo de saída, sw.js
sw <- file("Gerador_SW/sw.js")

## Pega o tempo do sistema e formata
time <- format(Sys.time(), "%d%m%Y_%H%M%S")

## Se quiser alterar a tabela manualmente por aqui
# Tipo <- c('css', 'js', 'htmls', 'dashs', 'listas', 'aux', "cache", 'updateTime')
# Versao <- c(0, 0, 0, time, time, 0, 0, time)
# versions <- data.frame(Tipo, Versao)
# data.table::fwrite(versions, "Gerador_SW/tabela_versoes.csv")

## Tabela com código de versões
versions <- data.table::fread("Gerador_SW/arquivos_auxiliares/tabela_versoes.csv")

## Lendo versões para criação de novo SW
aux <- versions[Tipo == 'aux']$Versao
cache <- versions[Tipo == 'cache']$Versao
css <- versions[Tipo == 'css']$Versao
dashs <- time # <- time (pegarão a data que rodar o arquivo)
htmls <- versions[Tipo == 'htmls']$Versao
js <- versions[Tipo == 'js']$Versao
listas <- time # <- time (pegarão a data que rodar o arquivo)
updateTime <- time # <- time (pegarão a data que rodar o arquivo)
debug <- "true"

## Criando linhas a serem adicionadas no sw.js
auxVersion <- paste0("const auxVersion = '", aux, "'")
cacheVersion <- paste0("const cacheVersion = '", cache, "'")
cssVersion <- paste0("const cssVersion = '", css, "'")
dashsVersion <- paste0("const dashsVersion = '", dashs, "'")
htmlsVersion <- paste0("const htmlsVersion = '", htmls, "'")
jsVersion <- paste0("const jsVersion = '", js, "'")
listasVersion <- paste0("const listasVersion = '", listas, "'")
updateTimeVersion <- paste0("const updateTimeVersion = '", updateTime, "'")

debugVersion <- paste0("\nconst debug = ", debug)

## Lê corpo padrão do sw (completo, dashs e listas)
read_sw_aux <- file("Gerador_SW/arquivos_auxiliares/sw_aux.txt")
## Cola as linhas, separando por \n
content <- paste(readLines(read_sw_aux), collapse = "\n")
## Fecha arquivo
close(read_sw_aux)
## Copia as linhas mais corpo do texto (sw.js)
writeLines(c(cssVersion, jsVersion, htmlsVersion, dashsVersion, listasVersion,
             auxVersion, cacheVersion, updateTimeVersion, debugVersion, content), sw)
close(sw)

## Lê corpo padrão do sw_emp (apenas página inicial)
read_sw_emp_aux <- file("Gerador_SW/arquivos_auxiliares/sw_emp_aux.txt")
## Cola as linhas, separando por \n
content_emp <- paste(readLines(read_sw_emp_aux), collapse = "\n")
## Copia as linhas mais corpo do texto (sw_emp.js)
writeLines(c(cssVersion, jsVersion, htmlsVersion,
             auxVersion, cacheVersion, debugVersion, content_emp), sw_emp)
close(sw_emp)
rm(list = ls())

rm(list = ls())

## Le a tabela salva (já organizada)
prop_ate_1ano_ant <- data.table::fread("Testes/propostas_pendentes.csv")

library(tableHTML)
## imprime em HTML (para facilitar modificação)
tableHTML::write_tableHTML(tableHTML::tableHTML(prop_ate_1ano_ant), file = 'Testes/example_table.html')

# Now create an ugly but minimalish HTML output from the Rmd ------------------
knitr::knit2html(
  input = "Testes/example_table.html",
  output = "Testes/outputs/manipulacao/exemplo_minimalista.html",
  ##Adicionando um css em branco para não pegar o default
  stylesheet = "Testes/style_blank.css",
  header = "Testes/header_lista_proposta.html",
)

##########################################################
## Funcao para substituir
library(xfun)

##########################################################
## Substituindo a tabela por tabela comum css irá mexer nessa classe

f_table <- c('<table style="border-collapse:collapse;" class=table_\\d\\d\\d\\d border=1>', '</table>')
r_table <- c('<table class="table">', '</table>')
for (i in 1:length(f_table)){ 
  gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_table[i], replacement = r_table[i])
}
##########################################################

##########################################################
## Substituindo th pela classe do css e separando por tabelas individualmente
f_tab <- c('<tr>', '</tr>')
r_tab <- c('<tr class="column">', '</tr>')
for (i in 1:length(f_tab)){
  gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_tab[i], replacement = r_tab[i])
}
##########################################################

##########################################################
## Substituindo headers (table header, para não aparecer) e id por classe

f_th <- c("id=",
          'th class="tableHTML_header_1">',
          "tableHTML_header_2", "tableHTML_header_3", "tableHTML_header_4", "tableHTML_header_5", "tableHTML_header_6")
r_th <- c("class=",
          "hide", "hide", "hide", "hide", "hide", "hide")

## Replace id and header
for (i in 1:length(f_th)){
  gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_th[i], replacement = r_th[i])
}
##########################################################

##########################################################
## Adicionando classe ao tbody (para busca)
gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = '<tbody>', replacement = '<tbody id=myTable>')
##########################################################

##########################################################
## Substituindo nomes das colunas pelos nomes corretos (css lida com a formatação)

f_tb <- c('class="tableHTML_rownames"',
          'class="tableHTML_column_1"', 
          'class="tableHTML_column_2"',
          'class="tableHTML_column_3"',
          'class="tableHTML_column_4"',
          'class="tableHTML_column_5">')
r_tb <- c('class="hide"> <span class="id"',
          'class="table-content"> <span class="clientes"', 
          'class="table-content"> <span class="vendedores"',
          'class="table-content"> <span class="produtos"',
          'class="table-content"> <span class="datas"',
          'class="table-content"> <span class="links" ')
## Replace id and header
for (i in 1:length(f_tb)){
  gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_tb[i], replacement = r_tb[i])
}
gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = '</td>', replacement = "</span></td>")

##########################################################

##########################################################
## Substituindo os caractéres especiais do link
f_link <- c('&#60;',
            '&ldquo;&quot;',
            '&rdquo;&ldquo;',
            '&lsquo;&#62;',
            '&lsquo;',
            '&rsquo;',
            '&#62;')
r_link <- c('<',
            '"',
            '"',
            '">',
            "'",
            "'",
            '>')
## Replace id and header
for (i in 1:length(f_link)){
  gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_link[i], replacement = r_link[i])
}
##########################################################

##########################################################
## Adicionando nova linha as propostas com mais de um produto
gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = '&mdash;', replacement = '<br>')
##########################################################

##########################################################
## Copiando o arquivo para lugar que está o css (estou fazendo isso pois as substituições precisam ser feitas numa pasta vazia) 
file.copy(from="Testes/outputs/manipulacao/exemplo_minimalista.html", to="Testes/outputs/lista_proposta.html", overwrite = T, recursive = F, copy.mode = T)


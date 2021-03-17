rm(list = ls())

## Le a tabela salva (já organizada)
prop_ate_1ano_ant <- data.table::fread("Testes/propostas_pendentes.csv")

## Função para resolver acentuaçȧo e caractéres especiais
fix_encoding <- function(x) {
  Encoding(x) <- "latin1"
  return(x)
}
# prop_ate_1ano_ant <- prop_ate_1ano_ant %>% 
#   mutate_if(is.character,fix_encoding)

vendedores <- prop_ate_1ano_ant %>%
  dplyr::select(Vendedor) %>%
  dplyr::distinct(Vendedor) %>%
  dplyr::arrange(Vendedor)

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
## Substituindo o meta cagado

f_meta <- '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>'
r_meta <- ''
gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_meta, replacement = r_meta)
##########################################################


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
## Vai depois de substituir todos os "ids" por classes, para não substituir o id do filter_Div
## Substituindo o <body> para criar título, filtros e busca

f_body <- ('<body>')
r_body_top <- '
    <body>
      <div class="top" id="filter-Div">
        <h1 class="title"> Filtro de Propostas<br><br> </h1>
        <p class="table-select">Primeiramente, selecione um nome de vendedor</p> <br>
        <select class="mySel" id="1">
          <option value="NENHUM">Selecione um vendedor</option>'
nrow(vendedores)
r_body_vendedores <- "\n\t\t\t\t\t"
for(i in 1:nrow(vendedores)){
  r_body_vendedores <- paste0(r_body_vendedores, "<option value='", vendedores[i], "'>", vendedores[i], "</option> \n\t\t\t\t\t")
}
gsub("'", '"', r_body_vendedores)
r_body_bottom <- 
          '
          <option value="TODOS">TODOS</option>
        </select>
        <br><br>
        <p class="table-search-description">Agora, caso queira fazer uma busca de um produto ou cliente deste vendedor,
          digite um trecho do item buscado (pelo menos 3 letras)</p><br>
        <input type="text" class="myInput" id="0" placeholder="Procurando..."/>
      </div>'

r_body <- paste0(r_body_top, r_body_vendedores, r_body_bottom)

gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_body, replacement = r_body)
##########################################################

##########################################################
## Adicionando classe ao tbody (para busca)
gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = '<tbody>', replacement = '<tbody id="myTable">')
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
## Substituindo os caractéres especiais dos nomes
f_spec <- c('<c0>', '<c1>', '<c2>', '<c3>',
            '<c8>', '<c9>', '<ca>', 
            '<cc>', '<cd>', '<ce>',
            '<c9>', '<cd>', '<d5>',
            '<c7>',
            '<a0>', '<aa>')
r_spec <- c('Ã', 'Á', 'Â', 'Ã',
            'È', 'É', 'Ê',
            'Ì', 'Í', 'Î',
            'Õ', 'Ó', 'Ô',
            'Ũ', 'Ú', 'Û',
            'Ç',
            ' ', 'ª')
## Replace id and header
for (i in 1:length(f_link)){
  #gsub_dir(dir = "Testes/outputs/manipulacao/", pattern = f_link[i], replacement = r_link[i])
}
##########################################################


##########################################################
## Copiando o arquivo para lugar que está o css (estou fazendo isso pois as substituições precisam ser feitas numa pasta vazia) 
file.copy(from="Testes/outputs/manipulacao/exemplo_minimalista.html", to="Testes/outputs/lista_proposta.html", overwrite = T, recursive = F, copy.mode = T)


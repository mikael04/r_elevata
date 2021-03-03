rm(list = ls())

prop_ate_1ano_ant <- data.table::fread("Testes/propostas_pendentes.csv")
library(tableHTML)
tableHTML::write_tableHTML(tableHTML::tableHTML(prop_ate_1ano_ant), file = 'Testes/example_table.html')

# Now create an ugly but minimalish HTML output from the Rmd ------------------
knitr::knit2html(
  input = "Testes/example_table.html",
  output = "Testes/outputs/exemplo_minimalista.html",
  ##Adicionando um css em branco para não pegar o default
  stylesheet = "Testes/style_blank.css",
  header = "Testes/header.txt",
)

##########################################################
## Funcao para substituir
library(xfun)

##########################################################
## Substituindo a tabela por tabela comum css irá mexer nessa classe

f_table <- ('<table style="border-collapse:collapse;" class=table_\\d\\d\\d\\d border=1>')
r_table <- ('<table class="table">')

gsub_dir(dir = "Testes/outputs/", pattern = f_table, replacement = r_table)
##########################################################

##########################################################
## Substituindo headers da tabela (thead) (assim ele transforma em hide e não mostra o header)
th_head <- ('<thead>')
gsub_dir(dir = "Testes/outputs/", pattern = th_head, replacement = '<thead class="hide">')
##########################################################

##########################################################
## Substituindo headers (table header, para não aparecer) e id por classe

f_th <- c("id=", "tableHTML_header_1", "tableHTML_header_2",
          "tableHTML_header_3", "tableHTML_header_4", "tableHTML_header_5", "tableHTML_header_6")
r_th <- c("class=", "hide", "hide",
          "hide", "hide", "hide", "hide")

## Replace id and header
for (i in 1:length(f_th)){
  gsub_dir(dir = "Testes/outputs/", pattern = f_th[i], replacement = r_th[i])
}
##########################################################

##########################################################
## Substituindo nomes das colunas pelos nomes corretos (css lida com a formatação)

f_tb <- c('class="tableHTML_rownames"', 'class="tableHTML_column_1"', 'class="tableHTML_column_2"', 'class="tableHTML_column_3"',
          'class="tableHTML_column_4"', 'class="tableHTML_column_5"')
r_tb <- c('class="hide" data-heading="ID"', 'class="table_content" data-heading="Vendedor"', 'class="table_content" data-heading="Cliente"', 'class="table_content" data-heading="Produto - Valor"', 'class="table_content_hide" data-heading="Status"', 'class="table_content_hide" data-heading="Data"')
## Replace id and header
for (i in 1:length(f_tb)){
  gsub_dir(dir = "Testes/outputs/", pattern = f_tb[i], replacement = r_tb[i])
}
##########################################################/mnt/data/Mikael/Projetos/Scripts_R/r_elevata/Testes/outputs/exemplo_minimalista.html

##########################################################
## Substituindo th pela classe do css e separando por tabelas individualmente
f_tab <- c('<tr>', '</tr>')
r_tab <- c('<table class="table">\n<tr class="table__row">', '</tr> </table>')
for (i in 1:length(f_tab)){
  gsub_dir(dir = "Testes/outputs/", pattern = f_tab[i], replacement = r_tab[i])
}
##########################################################

##########################################################
## Adicionando linha que terá botão para mais informações
gsub_dir(dir = "Testes/outputs/", pattern = '<td class="table_content_hide" data-heading="Status"',
         replacement = '</tr>
        <tr class="header">
          <th class="mais_infos">Mais informações <span>+</span></th>
        </tr>
        <tr class="hide">
          <td class="table_content_hide" data-heading="Status"')

##########################################################

##########################################################
## SAdicionando script jquery para clicar no botão
script_end <- ("<script>
      $('tr.header').click(function(){
        $(this).find('span').text(function(_, value){return value=='-'?'+':'-'});
        $(this).nextUntil('tr.table__row').css('display', function(i,v){
            return this.style.display === 'table-row' ? 'none' : 'table-row';
        });
      });
    </script>
  </body>")
  
gsub_dir(dir = "Testes/outputs/", pattern = '</body>', replacement = script_end)
##########################################################


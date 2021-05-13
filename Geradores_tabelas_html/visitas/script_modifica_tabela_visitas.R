####################################################################################################################
## Função para criação de htmls, lê csvs, salva como html (htmls_intermed), salva o nome dos vendedores (usado posteriormente), cria (com knit2html)
## novo html que será manipulado (htmls_final)
# if(debug)
# empresa = 16
# tabela_categoria = 'visitas'
# debug = T
####################################################################################################################
fct_alt_todas_html <- function(tabela_categoria, vendedores_empresa, empresas_ativas, debug){
  debug_interno = T
  # if(debug_interno){
  #   tabela_categoria = 'visitas'
  #   debug = T
  # }
  if(debug){
    print("Começando função de alterar tabelas")
    print("tabela_categoria = ")
    print(tabela_categoria)
  }

  ## Funcao para substituir
  library(xfun)

  ##########################################################
  ## Substituindo a tabela por tabela comum css irá mexer nessa classe

  f_table <- c('<table style="border-collapse:collapse;" class=table_\\d\\d\\d\\d border=1>', '</table>')
  # r_table <- c('<table class="table">', '\t\t</table>')
  header <- '<!DOCTYPE html>
<html>
\t<head>
    <title> Lista de visitas </title>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
    <meta name="viewport" content="width=device-width, height=device-height, initial-scale=1">

    <!-- Fonte utilizada -->
    <link href="https://fonts.googleapis.com/css2?family=Open+Sans&family=Roboto&display=swap" rel="stylesheet">
    <!-- CSS para lista -->
    <link rel="stylesheet" href="../../../../css/listas_iframe.css">
    <!-- lib para js jquery -->
    <script src="../../../../js/jquery-3.6.0.min.js"></script>
    <!-- lib para date picker -->
    <script src="../../../../js/jquery-ui.min.js"></script>
    <script src="../../../../js/jquery-ui.datepicker-pt-BR.js"></script>
    <link href="../../../../css/jquery-ui.min.css" rel="stylesheet">
    <!-- lib para manipular data-->
    <script src="../../../../js/luxon.min.js"></script>
  </head>
  <body>
    <table class="table">'
  r_table <- c(header, '\t\t</table>\n
		<!-- script de lista (filtro e busca) -->
    <script src="../../../../js/lista_visitas.js"></script>\n\t</body>\n</html>')
  for (i in 1:length(f_table)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_table[i], replacement = r_table[i])
  }
  ##########################################################

  ##########################################################
  ## Substituindo th pela classe do css e separando por tabelas individualmente
  f_tab <- c('<tr>', '</tr>')
  r_tab <- c('<tr class="column">', '</tr>')
  for (i in 1:length(f_tab)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_tab[i], replacement = r_tab[i])
  }
  ##########################################################

  ##########################################################
  ## Substituindo headers (table header, para não aparecer) e id por classe

  f_th <- c("id=",
            '"tableHTML_header_1">',
            "tableHTML_header_2", "tableHTML_header_3", "tableHTML_header_4", "tableHTML_header_5", "tableHTML_header_6",  "tableHTML_header_7",  "tableHTML_header_8")
  r_th <- c("class=",
            "'hide'>ID",
            "hide", "hide", "hide", "hide", "hide", "hide", "hide")

  ## Replace id and header
  for (i in 1:length(f_th)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_th[i], replacement = r_th[i])
  }
  ##########################################################

  ##########################################################
  ## Adicionando classe ao tbody (para busca)
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = '<tbody>', replacement = '<tbody id="myTable">')
  ##########################################################

  ##########################################################
  ## Substituindo nomes das colunas pelos nomes corretos (css lida com a formatação)

  f_tb <- c('class="tableHTML_rownames"',
            'class="tableHTML_column_1"',
            'class="tableHTML_column_2"',
            'class="tableHTML_column_3"',
            'class="tableHTML_column_4"',
            'class="tableHTML_column_5"',
            'class="tableHTML_column_6"',
            'class="tableHTML_column_7"')
  r_tb <- c('class="hide"> <span class="id"',
            'class="table-content"> <span class="clientes"',
            'class="table-content"> <span class="vendedores"',
            'class="table-content"> <span class="resultado"',
            'class="table-content"> <span class="motivo"',
            'class="table-content"> <span class="observacao"',
            'class="table-content"> <span class="datas"')
  ## Replace id and header
  for (i in 1:length(f_tb)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_tb[i], replacement = r_tb[i])
  }
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = '</td>', replacement = "</span></td>")
  ##########################################################

  ##########################################################
  ## Substituindo os caractéres especiais dos nomes
  # f_spec <- c('<c0>', '<c1>', '<c2>', '<c3>',
  #             '<c 8>', '<c9>', '<ca>',
  #             '<cc>', '<cd>', '<ce>',
  #             '<c3>', '<d4>', '<d5>',
  #             '<d9>', '<da>', '<db>',
  #             '<c7>',
  #             '<a0>', '<aa>', '<b0>', '<ba>',
  #             '""')
  # r_spec <- c('&#192;', '&#193;', '&#194;', '&#195;',
  #             '&#200;', '&#201;', '&#202;',
  #             '&#204;', '&#205;', '&#206;',
  #             '&#211;', '&#212;', '&#213;',
  #             '&#218;', '&#219;', '&#220;',
  #             '&#199;',
  #             '&#160;', '&#170;', '&#176;', '&#186;',
  #             '"')

  # c('A c/ crase', 'A c/ acento agudo', 'A c/ acento circunflexo', 'A c/ til',
  #   'E c/ crase', 'E c/ acento agudo', 'E c/ acento circunflexo',
  #   'I c/ crase', 'I c/ acento agudo', 'I c/ acento circunflexo',
  #   'O c/ acento agudo', 'O c/ acento circunflexo', 'O c/ til',
  #   'U c/ crase', 'U c/ acento agudo', 'U c/ acento circunflexo',
  #   'C cedilha',
  #   'espaço', ' a ordinal', 'o grau', 'o ordinal'
  #   '""' gerado no início da linha, por uma aspas simples, não funcionou substituir por &#034;)

  # ## Replace id and header
  # for (i in 1:length(f_spec)){
  #   gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_spec[i], replacement = r_spec[i])
  # }
  ##########################################################

  ##########################################################
  ## Adicionando filtro topo

  # Primeiro substituir a parte superior e adicionar palavras para alterar parte de baixo (vendedores e final)
  f_top_filter_top <- c('<body>')
  r_top_filter_top = "<body>
      <div class='top' id='filter-Div'>
        <h1 class='title'> Filtro de Visitas<br><br> </h1>
        <p class='table-select'>Primeiramente, selecione um nome de vendedor</p> <br>
        <select class='mySel' id='1'>
          <option value='NENHUM'>Selecione um vendedor</option>
          outros_vendedores
          final_filter_top"

  # Segunda substituição (parte de baixo)
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_top_filter_top, replacement = r_top_filter_top)
  f_top_filter_bottom <- c('final_filter_top')
  r_top_filter_bottom <-'<option value="TODOS">TODOS</option>
        </select>
        <br><br>
        <div class="data-filtro contentcenter">
          <div class="col-md-2 contentleft">
            <h4>Data Inicial</h4><br>
            <input id="dataInicial" type="text" name="timepicker" class="form-control"
                    placeholder="" data-orientation="top">
          </div>
          <div class="col-md-3 contentright">
            <h4>Data Final</h4><br>
            <input id="dataFinal" type="text" name="timepicker" class="form-control"
                    placeholder="" data-orientation="top">
          </div>
        </div>
        <br>
        <p class="table-search-description">Agora, caso queira fazer uma busca de cliente, resultado ou motivo das visitas deste vendedor,
          digite um trecho do item buscado (pelo menos 3 letras)</p><br>
        <input type="text" class="myInput" id="0" placeholder="Procurando..."/>
      </div>'
  # Segunda substituição (parte de baixo)
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/'), pattern = f_top_filter_bottom, replacement = r_top_filter_bottom)

  # Terceira substituição, do meio (adicionando vendedores)
  for(i in 1:(length(vendedores_empresa))){
    if(!is.null(vendedores_empresa[[i]])){
      if(debug){
        print("Subsituindo vendedores, for i")
        print(i)
        print(vendedores_empresa[[i]])
      }
      text_options= ''
      #vendedores_empresa[[1]][1]
      for(j in (1:length(vendedores_empresa[[i]]))){
        if(debug){
          print("Substituindo vendedores, for j")
          sprintf("i = %i", i)
          sprintf("j = %i", j)
          print(vendedores_empresa[[i]][j])
        }
        text_options <- paste0(text_options, "<option value='", vendedores_empresa[[i]][j],"'>", vendedores_empresa[[i]][j], '</option>\n\t\t\t\t\t')
      }
      if(debug){
        print(text_options)
      }
      f_top_filter_med <- c('outros_vendedores')
      r_top_filter_med <- text_options
      ## text_options vai ser uma string com todas as opções adicionadas, depois será feita a substituição, e isso vai rodar para cada arquivo
      ## i = empresa (as ativas)
      ## j = cada vendedor dentro da empresa
      gsub_file(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[i], '.html'), f_top_filter_med, r_top_filter_med)
    }
  }
  ##########################################################

  ##########################################################
  ## Copiando o arquivo após terminar de alterar para lugar final
  for(i in 1:(length(vendedores_empresa))){
    if(!is.null(vendedores_empresa[[i]])){
      if(debug){
        print("Copiando arquivos para pasta final")
        print(paste0("Iteração = ", i))
        print(paste0("Empresa = ", empresas_ativas[i]))
        print(paste0("Vendedores da empresa: ", vendedores_empresa[[i]]))
      }
      file.copy(from=paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresas_ativas[i], '.html'), to=paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/', tabela_categoria, '_', empresas_ativas[i], '.html'), overwrite = T, recursive = F, copy.mode = T)
    }
  }
}

fct_alt_vend_individual <- function(vendedor, empresa){

}

####################################################################################################################
## Função para criação de htmls, lê csvs, salva como html (htmls_intermed), salva o nome dos vendedores (usado posteriormente), cria (com knit2html)
## novo html que será manipulado (htmls_final)
# if(debug)
# empresa = 1
# tabela_categoria = 'propostas'
fct_cria_tabelas_html <- function(empresa, tabela_categoria, debug){
  #options((encoding="native"))
  # debug_interno = F
  # if(debug_interno){
  #   tabela_categoria = 'propostas'
  #   empresa = 67
  #   debug = T
  # }
  ## rm(list = ls())
  library(dplyr)
  ## Le a tabela salva (já organizada)
  if(file.exists(paste0("Geradores_tabelas_html/", tabela_categoria, "/empresas/",tabela_categoria, "_", empresa ,".csv"))){
    print("Arquivo Existe, será gerado o html:");
    print(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresa , '.html'))
    tabela_csv <- data.table::fread(paste0("Geradores_tabelas_html/", tabela_categoria, "/empresas/",tabela_categoria, "_", empresa ,".csv"))

    # tabela_csv <- tabela_csv %>%
    #   mutate_if(is.character,fix_encoding)
    if(nrow(tabela_csv) > 0){
      vendedores <- tabela_csv %>%
        dplyr::select(Vendedor) %>%
        dplyr::distinct(Vendedor) %>%
        dplyr::arrange(Vendedor) %>%
        dplyr::mutate(Vendedor = `Encoding<-`(Vendedor, 'latin1'))
    }else{
      vendedores <- NULL
    }

    library(tableHTML)
    ## imprime em HTML (para facilitar modificação)
    #tableHTML::write_tableHTML(tableHTML::tableHTML(tabela_csv), file = 'Geradores_tabelas_html/propostas/example_table.html')
    tableHTML::write_tableHTML(tableHTML::tableHTML(tabela_csv), file = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresa , '.html'))

    ## Gerando HTML com header pronto
    input_ = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresa , '.html')
    output_ = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/', tabela_categoria, '_', empresa , '.html')
    knitr::knit2html(
      input = input_,
      output = output_,
      ##Adicionando um css em branco para não pegar o default
      stylesheet = "Geradores_tabelas_html/style_blank.css",
      header = "Geradores_tabelas_html/propostas/header_lista_proposta.html"
    )
    if (file.exists(paste0(tabela_categoria, '_', empresa, '.txt'))) {
      #Delete file if it exists
      file.remove(paste0(tabela_categoria, '_', empresa, '.txt'))
    }
    #paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/', tabela_categoria, '_', empresa , '.html')
  }else{
    if(debug){
      print("Arquivo não existe, possivelmente não usa essa funcionalidade ou algum outro erro");
      print(paste0("Geradores_tabelas_html/", tabela_categoria, "/empresas/", tabela_categoria, "_", empresa,".csv"))
    }
    return(NULL)
  }

  ##########################################################
  return(vendedores)
}
####################################################################################################################

####################################################################################################################
fct_alt_todas_html <- function(tabela_categoria, debug, vendedores_empresa, empresas_ativas){
  # debug_interno = T
  # if(debug_interno){
  #   tabela_categoria = 'propostas'
  #   debug = T
  # }
  if(debug){
    print("tabela_categoria = ")
    print(tabela_categoria)
  }

  ## Funcao para substituir
  library(xfun)

  ##########################################################
  ## Substituindo o meta cagado

  f_meta <- '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>'
  r_meta <- ''
  if(debug){
    print(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'))
  }
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_meta, replacement = r_meta)
  ##########################################################


  ##########################################################
  ## Substituindo a tabela por tabela comum css irá mexer nessa classe

  f_table <- c('<table style="border-collapse:collapse;" class=table_\\d\\d\\d\\d border=1>', '</table>')
  r_table <- c('<table class="table">', '</table>')
  for (i in 1:length(f_table)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_table[i], replacement = r_table[i])
  }
  ##########################################################

  ##########################################################
  ## Substituindo th pela classe do css e separando por tabelas individualmente
  f_tab <- c('<tr>', '</tr>')
  r_tab <- c('<tr class="column">', '</tr>')
  for (i in 1:length(f_tab)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_tab[i], replacement = r_tab[i])
  }
  ##########################################################

  ##########################################################
  ## Substituindo headers (table header, para não aparecer) e id por classe

  f_th <- c("id=",
            '"tableHTML_header_1">',
            "tableHTML_header_2", "tableHTML_header_3", "tableHTML_header_4", "tableHTML_header_5", "tableHTML_header_6")
  r_th <- c("class=",
            "'hide'>ID",
            "hide", "hide", "hide", "hide", "hide")

  ## Replace id and header
  for (i in 1:length(f_th)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_th[i], replacement = r_th[i])
  }
  ##########################################################

  ##########################################################
  ## Adicionando classe ao tbody (para busca)
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = '<tbody>', replacement = '<tbody id="myTable">')
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
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_tb[i], replacement = r_tb[i])
  }
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = '</td>', replacement = "</span></td>")

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
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_link[i], replacement = r_link[i])
  }
  ##########################################################

  ##########################################################
  ## Adicionando nova linha as propostas com mais de um produto
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = '&mdash;', replacement = '<br>')
  ##########################################################

  ##########################################################
  ## Substituindo os caractéres especiais dos nomes
  f_spec <- c('<c0>', '<c1>', '<c2>', '<c3>',
              '<c 8>', '<c9>', '<ca>',
              '<cc>', '<cd>', '<ce>',
              '<c3>', '<d4>', '<d5>',
              '<d9>', '<da>', '<db>',
              '<c7>',
              '<a0>', '<aa>', '<b0>', '<ba>')
  r_spec <- c('&#192;', '&#193;', '&#194;', '&#195;',
              '&#200;', '&#201;', '&#202;',
              '&#204;', '&#205;', '&#206;',
              '&#211;', '&#212;', '&#213;',
              '&#218;', '&#219;', '&#220;',
              '&#199;',
              '&#160;', '&#170;', '&#176;', '&#186;')

  # c('A c/ crase', 'A c/ acento agudo', 'A c/ acento circunflexo', 'A c/ til',
  #   'E c/ crase', 'E c/ acento agudo', 'E c/ acento circunflexo',
  #   'I c/ crase', 'I c/ acento agudo', 'I c/ acento circunflexo',
  #   'O c/ acento agudo', 'O c/ acento circunflexo', 'O c/ til',
  #   'U c/ crase', 'U c/ acento agudo', 'U c/ acento circunflexo',
  #   'C cedilha',
  #   'espaço', ' a ordinal', 'o grau', 'o ordinal')

  ## Replace id and header
  for (i in 1:length(f_spec)){
    gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_spec[i], replacement = r_spec[i])
  }
  ##########################################################

  ##########################################################
  ## Adicionando filtro topo

  # Primeiro substituir a parte superior e adicionar palavras para alterar parte de baixo (vendedores e final)
  f_top_filter_top <- c('<body>')
  r_top_filter_top = "<body>
      <div class='top' id='filter-Div'>
        <h1 class='title'> Filtro de Propostas<br><br> </h1>
        <p class='table-select'>Primeiramente, selecione um nome de vendedor</p> <br>
        <select class='mySel' id='1'>
          <option value='NENHUM'>Selecione um vendedor</option>
          outros_vendedores
          final_filter_top"

  # Primeira substituição (parte de cima e gerar palavras para substituir com outros)
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_top_filter_top, replacement = r_top_filter_top)
  f_top_filter_bottom <- c('final_filter_top')
  r_top_filter_bottom <-"<option value='TODOS'>TODOS</option>
        </select>
        <br><br>
        <p class='table-search-description'>Agora, caso queira fazer uma busca de um produto ou cliente deste vendedor,
          digite um trecho do item buscado (pelo menos 3 letras)</p><br>
        <input type='text' class='myInput' id='0' placeholder='Procurando...'/>
      </div>'"
  # Segunda substituição (parte de baixo)
  gsub_dir(dir = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/'), pattern = f_top_filter_bottom, replacement = r_top_filter_bottom)

  # Terceira substituição, do meio (adicionando vendedores)
  for(i in 1:(length(vendedores_empresa))){
    if(!is.null(vendedores_empresa[[i]])){
      if(debug){
        print(i)
        print(vendedores_empresa[[i]])
      }
      text_options= ''
      #vendedores_empresa[[1]][1]
      for(j in (1:length(vendedores_empresa[[i]]))){
        if(debug){
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
      gsub_file(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_final/', tabela_categoria, '_', empresas_ativas[i], '.html'), f_top_filter_med, r_top_filter_med)
    }
  }
  ##########################################################

  ##########################################################
  ## Copiando o arquivo para lugar que está o css (estou fazendo isso pois as substituições precisam ser feitas numa pasta vazia)
  ##file.copy(from="Gerador_tabelas_html/outputs/htmls_final/exemplo_minimalista.html", to="Gerador_tabelas_html/lista_proposta.html", overwrite = T, recursive = F, copy.mode = T)
}

fct_alt_vend_individual <- function(vendedor, empresa){

}

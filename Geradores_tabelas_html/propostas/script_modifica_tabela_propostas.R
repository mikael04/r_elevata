# if(teste)
# empresa = 1
# tabela_categoria = 'propostas'
fct_cria_tabelas_html <- function(empresa, tabela_categoria, teste){
  #options((encoding="native"))
  # = T
  if(teste_interno){
    tabela_categoria = 'propostas'
    empresa = 16
    teste = T
  }
  ## rm(list = ls())
  library(dplyr)
  ## Le a tabela salva (já organizada)
  if(file.exists(paste0("Geradores_tabelas_html/", tabela_categoria, "/empresas/",tabela_categoria, "_", empresa ,".csv"))){
    print("Arquivo Existe, será gerado o html:");
    print(paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresa , '.html'))
    tabela_csv <- data.table::fread(paste0("Geradores_tabelas_html/", tabela_categoria, "/empresas/",tabela_categoria, "_", empresa ,".csv"), encoding = 'Latin-1')
    ## Função para resolver acentuaçȧo e caractéres especiais
    # fix_encoding <- function(x) {
    #   Encoding(x) <- "latin1"
    #   return(x)
    # }
    # prop_ate_1ano_ant <- prop_ate_1ano_ant %>%
    #   mutate_if(is.character,fix_encoding)

    vendedores <- tabela_csv %>%
      dplyr::select(Vendedor) %>%
      dplyr::distinct(Vendedor) %>%
      dplyr::arrange(Vendedor)

    library(tableHTML)
    ## imprime em HTML (para facilitar modificação)
    #tableHTML::write_tableHTML(tableHTML::tableHTML(tabela_csv), file = 'Geradores_tabelas_html/propostas/example_table.html')
    tableHTML::write_tableHTML(tableHTML::tableHTML(tabela_csv), file = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresa , '.html'))

    ## Gerando HTML com header pronto
    input_ = paste0('Geradores_tabelas_html/', tabela_categoria, '/htmls_intermed/', tabela_categoria, '_', empresa , '.html')
    output_ = paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/', tabela_categoria, '_', empresa , '.html')
    knitr::knit2html(
      input = input_,
      output = output_,
      ##Adicionando um css em branco para não pegar o default
      stylesheet = "Gerador_tabelas_html/propostas/style_blank.css",
      header = "Gerador_tabelas_html/propostas/header_lista_proposta.html"
    )
    paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/', tabela_categoria, '_', empresa , '.html')
  }else{
    if(teste){
      print("Arquivo não existe, possivelmente não usa essa funcionalidade ou algum outro erro");
      print(paste0("Geradores_tabelas_html/", tabela_categoria, "/empresas/", tabela_categoria, "_", empresa,".csv"))
    }
  }

  ##########################################################
  # return vendedores_empresa
}

fct_alt_todas_html <- function(tabela_categoria, teste){
  teste_interno = T
  if(teste_interno){
    tabela_categoria = 'proposta'
    teste = T
  }
  if(teste){
    print("tabela_categoria = ")
    print(tabela_categoria)
  }

  ## Funcao para substituir
  library(xfun)

  ##########################################################
  ## Substituindo o meta cagado

  f_meta <- '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>'
  r_meta <- ''
  if(teste){
    print(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/1'))
  }
  gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/1/'), pattern = f_meta, replacement = r_meta)
  ##########################################################


  ##########################################################
  ## Substituindo a tabela por tabela comum css irá mexer nessa classe

  f_table <- c('<table style="border-collapse:collapse;" class=table_\\d\\d\\d\\d border=1>', '</table>')
  r_table <- c('<table class="table">', '</table>')
  for (i in 1:length(f_table)){
    gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/1/'), pattern = f_table[i], replacement = r_table[i])
  }
  ##########################################################

  ##########################################################
  ## Substituindo th pela classe do css e separando por tabelas individualmente
  f_tab <- c('<tr>', '</tr>')
  r_tab <- c('<tr class="column">', '</tr>')
  for (i in 1:length(f_tab)){
    gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = f_tab[i], replacement = r_tab[i])
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
    gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = f_th[i], replacement = r_th[i])
  }
  ##########################################################

  ##########################################################
  ## Adicionando classe ao tbody (para busca)
  gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = '<tbody>', replacement = '<tbody id="myTable">')
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
    gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = f_tb[i], replacement = r_tb[i])
  }
  gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = '</td>', replacement = "</span></td>")

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
    gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = f_link[i], replacement = r_link[i])
  }
  ##########################################################

  ##########################################################
  ## Adicionando nova linha as propostas com mais de um produto
  gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = '&mdash;', replacement = '<br>')
  ##########################################################

  ##########################################################
  ## Substituindo os caractéres especiais dos nomes
  f_spec <- c('<c0>', '<c1>', '<c2>', '<c3>',
              '<c8>', '<c9>', '<ca>',
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
    gsub_dir(paste0('Geradores_tabelas_html/', tabela_categoria, '/manipulacao/'), pattern = f_spec[i], replacement = r_spec[i])
  }
  ##########################################################


  ##########################################################
  ## Copiando o arquivo para lugar que está o css (estou fazendo isso pois as substituições precisam ser feitas numa pasta vazia)
  file.copy(from="Gerador_tabelas_html/outputs/manipulacao/exemplo_minimalista.html", to="Gerador_tabelas_html/lista_proposta.html", overwrite = T, recursive = F, copy.mode = T)
}

fct_alt_vend_individual <- function(vendedor, empresa){

}

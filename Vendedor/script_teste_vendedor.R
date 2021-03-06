rm(list = ls())
#Lib q ser? futuramente usada pros pain?is interativos
#library(shiny)
#Lib pra conex?o com o banco
#library(odbc)
#Lib para ler (mais rapidamente) os csvs
library(data.table)
#lib com uma cacetada de outras libs para manipular dados
#library(tidyverse)
#Libs pra trabalhar com a base (cortes e fun??es similares ao SQL)
library(dplyr) #Contido no tidyverse
#lib pros gr?ficos mais "interativos"
library(plotly)
#library(htmlwidgets)
#Lib pra usar paletas de cores
library(RColorBrewer)
#library(viridis)
#Lib usada pros treemaps
#library(treemap)
#Lib usada pros waffles
#library(waffle)
#usada para converter n?meros em moeda
#library(scales)
#Lib para lidar com o tempo
library(lubridate)
source("fct_tempo.R")
source("fct_fmt_din.R")
source("fct_fmt_nome.R")


###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = T
####Variável usada para não plotar os gráficos na dash
dash = F
####Variável para testar dias anteriores
num_dias <- 0
if(!teste){
  ##Teste se estou gerando via rstudio (knit)
  if(as.integer(num_dias) == 0) {
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual = fct_ano_atual()
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = fct_mes_atual()
    ## Apenas ano, para gerar títulos
    ano <- year(ano_atual)
    ##Execução normal, recebendo data do gerador de dashs
  }else{
    data <- (lubridate::today()-lubridate::days(num_dias))
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
    ## Apenas ano, para gerar títulos
    ano <- lubridate::year(ano_atual)
  }
}else{
  ##Teste setando dia
  data <- lubridate::ymd("2020-12-31")
  ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
  ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
  ####Variavel global c/ mês atual (para comparação)
  mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
  ## Apenas ano, para gerar títulos
  ano <- lubridate::year(ano_atual)
}

##Teste, senão tiver parâmetro, estou fazendo o teste e entra no if, senão vai pro else
####Vari?vel global para ver se tem usados Ainda n?o usada
#usados = T

##plotando texto sem informações #usado para gráficos que não tiverem nenhuma informação no período
#caminho para imagem de sem dados
s_dados_path <- "s_dados.png"

#empresa = params$variable1
#teste
empresa = 78
vend_id = 1058

########################################################################################################
###Começando scripts proposta_scripts
########################################################################################################

##-> Collect cria o dataframe resultado da query, proposta será a tabela na qual estou lendo (FROM cliente)
##coleta todas as propostas
proposta <- fread("Tabelas/proposta.csv", colClasses = c(proposta_id = "character", proposta_negocio_id = "character")) %>%
  select(proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status)

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id)


##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter (vendedor_empresa_id == empresa) %>%
  dplyr::filter(vendedor_id == vend_id)

vendedor_a <- vendedor %>%
  filter (vendedor_ativo == T)

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- sapply(vendedor$vendedor_nome, func_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

##coleta proposta_pagamento
proposta_pagamento <- fread("Tabelas/proposta_pagamento.csv", colClasses = c(pp_id = "character", pp_proposta_id = "character")) %>%
  select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
  filter(pp_ativo == 1) %>%
  select(-pp_ativo)

###################################

##junção de proposta com negócio
prop_ij_neg <- inner_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado, negocio_produto_id)

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)/Acho que era erro na junção (inner parece ter resolvido)
#prop_ij_neg <- prop_ij_neg[!is.na(prop_ij_neg$negocio_vendedor_id),]

if (teste == F){
  rm()
}

##Juntando com vendedor pra obter o nome do vendedor
prop_ij_neg_ij_vend <- inner_join(prop_ij_neg, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

##Filtrando a empresa
prop_ij_neg_ij_vend_anat <- prop_ij_neg_ij_vend %>%
  filter(vendedor_empresa_id == empresa, proposta_data_cadastro >= ano_atual, proposta_data_cadastro < (ano_atual+years(1)))

prop_ij_neg_ij_vend_ij_propag_anat <- inner_join(prop_ij_neg_ij_vend_anat, proposta_pagamento, by=c("proposta_id" = "pp_proposta_id")) %>%
  select (-pp_modo_id, -pp_forma_id, -pp_usado_id)


status = c("Pendente", "Aceito", "Recusado", "Cancelado", "Finalizado")

##Aqui tenho a contagem de status por vendedor
prop_ij_neg_ij_vend_ij_propag_anat <- prop_ij_neg_ij_vend_ij_propag_anat %>%
  select (negocio_vendedor_id, negocio_vendedor_id, vendedor_nome, proposta_status, vendedor_ativo, pp_valor) %>%
  group_by(proposta_status, negocio_vendedor_id) %>%
  mutate(cont_status = n()) %>%
  mutate(sum_valor = sum(pp_valor)) %>%
  distinct(negocio_vendedor_id, proposta_status, .keep_all = TRUE) %>%
  ungroup ()


##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_ij_vend_ij_propag_anat$proposta_status <- with(prop_ij_neg_ij_vend_ij_propag_anat, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                                                   labels = status))
prop_ij_neg_cont_vend_a_ij_propag_anat <- prop_ij_neg_ij_vend_ij_propag_anat %>%
  filter(vendedor_ativo == T)

### Gráfico p0 - Número propostas, por tipo, por vendedor em anat(total)
if(nrow(prop_ij_neg_cont_vend_a_ij_propag_anat) > 0){
  p0 <- plot_ly(prop_ij_neg_cont_vend_a_ij_propag_anat, type = 'bar', orientation = 'h', x = ~cont_status , y = ~reorder(vendedor_nome, desc(vendedor_nome)),
                color = ~proposta_status,
                colors = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"),
                name = ~proposta_status,
                showlegend = TRUE
  )
  p0 <- p0 %>%
    layout(barmode = 'grouped',
           xaxis = list(title = ''),
           yaxis = list(title = ''),
           legend = list(traceorder = 'normal', orientation = 'h'))

}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  #p0 <- s_dados
  p0 <- include_graphics(s_dados_path)
}
if(dash == F){
  p0
}

prop_ij_neg_ij_propag_cont_anat <- prop_ij_neg_ij_vend_ij_propag_anat %>%
  group_by(proposta_status) %>%
  mutate(cont_status_v = sum(cont_status)) %>%
  mutate(sum_valor_v = sum(sum_valor)) %>%
  distinct(proposta_status, .keep_all = T) %>%
  select (-negocio_vendedor_id, -vendedor_nome, -vendedor_ativo, -cont_status, -sum_valor) %>%
  ungroup ()


prop_ij_neg_ij_propag_cont_anat <- prop_ij_neg_ij_propag_cont_anat %>% dplyr::rowwise() %>%
  dplyr::mutate(sum_valor_v_t = func_fmt_din(sum_valor_v))
Encoding(prop_ij_neg_ij_propag_cont_anat$sum_valor_v_t) <- "UTF-8"

### Gráfico p1 - Número propostas, por tipo em anat (total)
if(nrow(prop_ij_neg_ij_propag_cont_anat) > 0){
  p1 <- plot_ly(prop_ij_neg_ij_propag_cont_anat, type = 'bar', x = ~proposta_status , y = ~sum_valor_v,
                color = ~proposta_status,
                colors = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"),
                text = ~paste0(proposta_status,
                               '<br>',
                               'Valor financeiro: ', sum_valor_v_t,
                               '<br>',
                               "Nº de propostas: ", cont_status_v),
                hoverinfo = "text",
                # hovertemplate = ~paste0("%{label}: ",
                #                         "<br>",
                #                         "Valor financeiro: %{value}",
                #                         "<br>",
                #                         "Nº de propostas: ", count_modo,
                #                         "<br>"),
                showlegend = F
  )
  p1 <- p1 %>%
    layout(barmode = 'grouped',
           xaxis = list(title = ''),
           yaxis = list(title = ''),
           legend = list(traceorder = 'normal'))

}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  #p1 <- s_dados
  p1 <- include_graphics(s_dados_path)
}

if(dash == F){
  p1
}

if(teste == F){
  #tabelas
  if(dash == F){
    rm()
  }
  rm(prop_ij_neg_anat, prop_ij_neg_ij_vend_ij_propag_anat, prop_ij_neg_ij_vend_anat, prop_ij_neg_ij_propag_cont_anat);
  #variáveis
  rm(status);
}

##############################################

##Propostas por categoria (usado)
##############################################
##Aqui tenho a contagem de usados em propostasda empresa
prop_ij_neg_ij_vend_cont_us <- prop_ij_neg_ij_vend %>%
  select (negocio_usado) %>%
  group_by(negocio_usado) %>%
  mutate(usado = n()) %>%
  distinct(negocio_usado, .keep_all = TRUE) %>%
  ungroup ()

total = sum(prop_ij_neg_ij_vend_cont_us$usado)

prop_ij_neg_ij_vend_cont_us <- prop_ij_neg_ij_vend_cont_us %>% ###########################################################################################################
mutate(total = total)

prop_ij_neg_ij_vend_cont_us$negocio_usado[prop_ij_neg_ij_vend_cont_us$negocio_usado == TRUE] <- "Proposta com usado"
prop_ij_neg_ij_vend_cont_us$negocio_usado[prop_ij_neg_ij_vend_cont_us$negocio_usado == FALSE] <- "Proposta sem usado"

### Gráfico p2 - Proporção de usados (pizza)
if(nrow(prop_ij_neg_ij_vend_cont_us) > 0){
  p2 <- plot_ly(prop_ij_neg_ij_vend_cont_us, labels = ~negocio_usado, values = ~usado, type = 'pie', sort = F,
                texttemplate = "%{value} (%{percent})",
                hovertemplate = paste ("%{label} <br>",
                                       "%{value} <br>",
                                       "Equivalente a %{percent} do total",
                                       "<extra></extra>"))
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  #p2 <- s_dados
  p2 <- include_graphics(s_dados_path)
}
if(dash == F){
  p2
}
if(teste == F){
  #tabelas
  rm(prop_ij_neg_ij_vend_cont_us, prop_ij_neg);
  #variáveis
  rm(total);
}

##############################################
###Ticket médio por proposta (apenas novos)

#################################################
##negocio_produto para pegar os valores de cada negócio
negocio_produto <- fread("Tabelas/negocio_produto.csv", colClasses = c(np_id = "character", np_negocio_id = "character", np_produto_id = "character")) %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade, np_ativo, np_valor)


##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character", produto_marca_id = "character", produto_categoria_id = "character")) %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

proposta_produto <- fread("Tabelas/proposta_produto.csv", colClasses = c(pp_id = "character", pp_proposta_id = "character", pp_produto_id = "character")) %>%
  select(pp_id, pp_proposta_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo) %>%
  filter (pp_ativo == 1) %>% #Filtrando pp_ativo = true
  select(-pp_ativo) %>%
  mutate (pp_valor_tot = pp_valor*pp_quantidade)



p_ij_n_ij_v_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_produto, by = c("proposta_id" = "pp_proposta_id"))

##Vou puxar negócio novamente pra pegar a coluna de se é usado ou não (acho que é mais eficiente do que usar a tabela completa desde o início)
negocio_aux <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character")) %>%
  select(negocio_id, negocio_tipo_negocio)


p_ij_n_ij_v_ij_pp_n <- inner_join(p_ij_n_ij_v_ij_pp, negocio_aux, by = c("proposta_negocio_id" = "negocio_id"))

##filtro da empresa e também propostas finalizadas, também filtrando só negócios novos e com valores acima de 10
p_ij_n_ij_pp_empresa <- p_ij_n_ij_v_ij_pp_n %>%
  filter(vendedor_empresa_id == empresa, proposta_status == 4, negocio_tipo_negocio == 'N', pp_valor_tot > 10)

p_ij_n_ij_pp_empresa <- p_ij_n_ij_pp_empresa %>%
  group_by(proposta_id) %>%
  mutate (valor_proposta = sum(pp_valor_tot)) %>%
  ungroup ()

##media geral da empresa
total_empresa <- sum(p_ij_n_ij_pp_empresa$pp_valor_tot)
n_empresa <- nrow(p_ij_n_ij_pp_empresa)
media_empresa <- round((total_empresa/n_empresa), 2)

##flag usada para mostrar gráfico p3 (caso não tenha usados, flag_ticket_usados = F) ou p4 (caso tenha usados, flag_ticket_usados = T)
flag_ticket_usados = F
##Calculando ticket médio por categoria

p_ij_n_ij_pp_ij_prod <- inner_join (p_ij_n_ij_pp_empresa, produto, by= c('pp_produto_id' = 'produto_id')) %>%
  select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_produto_id, pp_valor_tot, produto_categoria_id, vendedor_empresa_id) %>%
  filter(proposta_status == 4, vendedor_empresa_id == empresa) #Proposta finalizada


## Primeira vez pra verificar quais são os top5
pr_top5_fat <- p_ij_n_ij_pp_ij_prod %>%
  select(produto_categoria_id, pp_valor_tot) %>%
  group_by(produto_categoria_id) %>%
  mutate(fat = sum(pp_valor_tot)) %>%
  mutate(n = n()) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  ungroup ()

##Isso aqui tudo é pra pegar o top 5 (antes top10, mas aparecia uma categoria que não queríamos), substituir os que não estão por -Outros e depois refazer a média
top5_fat <- head.matrix(pr_top5_fat, n=5)

##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- fread("Tabelas/categoria.csv", colClasses = c(categoria_id = "character")) %>%
  select(categoria_id, categoria_nome)


##Super apenas (para diminuir o tamanho do nome)
if (empresa == 16)
{
  categoria$categoria_nome[categoria$categoria_id == '120160830102326'] <- 'SEMEADORA'
}
#Arrumando encoding
Encoding(categoria$categoria_nome) <- 'latin1'

top5_fat_ij_cat <- inner_join(top5_fat, categoria, by = c("produto_categoria_id"="categoria_id"))
top5_fat_ij_cat <- top5_fat_ij_cat[, -2:-4]
top5_fat_ij_cat <- as.data.frame(top5_fat_ij_cat)

##Aqui estou juntando para agrupar primeiro através do na
pr_top5_fat_aux <- left_join(pr_top5_fat, top5_fat_ij_cat, by=c("produto_categoria_id" = "produto_categoria_id"))
pr_top5_fat_aux$produto_categoria_id[is.na(pr_top5_fat_aux$categoria_nome)] <- "-1"
pr_top5_fat_aux$categoria_nome[is.na(pr_top5_fat_aux$categoria_nome)] <- "OUTRAS *"


##Depois de setar as linhas q tem nome de coluna = na (não estão no top5) e ter setado todas pra id = -1, faço a soma dos n e faturamentos
n_out <- sum(pr_top5_fat_aux$n[which(pr_top5_fat_aux$produto_categoria_id=="-1")])
fat_out <- sum(pr_top5_fat_aux$fat[which(pr_top5_fat_aux$produto_categoria_id=="-1")])

##E então, repasso o valor de volta para a coluna "outros", de faturamento total (de todas as categorias menos as q estão no top5) e número de vezes que aparecem
pr_top5_fat_aux$n[pr_top5_fat_aux$produto_categoria_id=='-1'] <- n_out
pr_top5_fat_aux$fat[pr_top5_fat_aux$produto_categoria_id=='-1'] <- fat_out

##E aqui removo as demais linhas, que já foram adicionadas a "Outros"
pr_top5_fat_aux <- pr_top5_fat_aux[!is.na(pr_top5_fat_aux$categoria_nome),]

##Aqui só renomeio pra ver que a categoria outros tem um * representando a soma de todas as outras categorias também
#pr_top5_fat_aux$categoria_nome[(pr_top5_fat_aux$categoria_nome == "OUTRA")] <- "OUTRA *" #Não mais necessário pq já tneho que adicionar a categoria outra então já faço com nome que quero


## Aqui já estõu fazendo a média das categorias
pr_top5_fat_med <- pr_top5_fat_aux %>%
  select(categoria_nome, produto_categoria_id, fat, n) %>%
  group_by(produto_categoria_id) %>%
  mutate(fat_med = fat/n) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  ungroup ()

fat_tot_categorias <- pr_top5_fat_med

##Adicionando a matriz pra poder exibir no gráfico ###########################################################################################################
fat_tot_categorias <- fat_tot_categorias %>%
  dplyr::mutate(med_emp = media_empresa) %>% dplyr::rowwise() %>%
  mutate(med_emp_t = func_fmt_din(med_emp))
## Se windows, elevata, precisa recodificar para UTF-8 na conversão
Encoding(fat_tot_categorias$med_emp_t) <- "UTF-8"

##Adicionando coluna com os valores já em texto para plot
if(nrow(fat_tot_categorias) > 0){
  fat_tot_categorias <- fat_tot_categorias %>% rowwise() %>%
    mutate(fat_med_t = func_fmt_din(fat_med))
  ## Se windows, elevata, precisa recodificar para UTF-8 na conversão
  Encoding(fat_tot_categorias$fat_med_t) <- "UTF-8"
}

### Gráfico p3 - Ticket médio novos
if(nrow(fat_tot_categorias) > 0){
  ax <- list(
    autotick = TRUE,
    title = "",
    showticklabels = TRUE)

  p3 <- plot_ly(fat_tot_categorias, x = ~categoria_nome)
  p3 <- p3 %>%
    add_trace(type = "bar", y = ~fat_med,
              name = 'Categorias novos',
              marker = list(color = 'lightblue'),
              text = ~paste0("Categoria novos",
                             '<br>' ,
                             categoria_nome,
                             '<br>' ,
                             fat_med_t),
              hoverinfo = "text")

  p3 <- p3 %>%
    add_trace(type = "scatter+lines", mode = "markers+line", y = ~med_emp,
              name = 'Novos (média)',
              line = list(color = '#21618c'),
              text = ~paste0('Ticket médio novos<br>' , med_emp_t),
              hoverinfo = "text",
              marker = list(color = 'red'))
  p3 <- p3  %>%
    layout(
      autosize = T,
      xaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE, title = ''),
      #range nos dois eixos iguais pra ficar na mesma proporção
      yaxis = list(title = '', showgrid = TRUE, zeroline = T),
      ##aqui eu ajusto onde quero que apareça a legenda
      legend = list(traceorder = 'normal', x = 0.7, y = 0.8))
}else {
  ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
  #p3 <- s_dados
  p3 <- include_graphics(s_dados_path)
}
if(dash == F){
  p3
}

###Ticket médio por proposta (apenas usados)

p_ij_n_ij_v_ij_pp_n <- inner_join(p_ij_n_ij_v_ij_pp, negocio_aux, by = c("proposta_negocio_id" = "negocio_id"))

##filtro da empresa e também propostas finalizadas, também filtrando só negócios novos e com valores acima de 10
p_ij_n_ij_pp_empresa_us <- p_ij_n_ij_v_ij_pp_n %>%
  filter(vendedor_empresa_id == empresa, proposta_status == 4, negocio_tipo_negocio == 'U', pp_valor_tot > 10)

if(nrow(p_ij_n_ij_pp_empresa_us) > 0)
{
  flag_ticket_usados = T
  p_ij_n_ij_pp_empresa_us <- p_ij_n_ij_pp_empresa_us %>%
    group_by(proposta_id) %>%
    mutate (valor_proposta = sum(pp_valor_tot)) %>%
    ungroup ()

  ##media geral da empresa
  total_empresa_us <- sum(p_ij_n_ij_pp_empresa_us$pp_valor_tot)
  n_empresa_us <- nrow(p_ij_n_ij_pp_empresa_us)
  media_empresa_us <- round((total_empresa_us/n_empresa_us), 2)


  ##Calculando ticket médio por categoria

  p_ij_n_ij_pp_ij_prod_us <- inner_join (p_ij_n_ij_pp_empresa_us, produto, by= c('pp_produto_id' = 'produto_id')) %>%
    select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_produto_id, pp_quantidade, pp_valor_tot, produto_categoria_id, vendedor_empresa_id, negocio_tipo_negocio) %>%
    filter(proposta_status == 4, vendedor_empresa_id == empresa) #Proposta finalizada

  ##Adicionar categoria pra ver se é isso mesmo
  p_ij_n_ij_pp_ij_prod_us <- inner_join(p_ij_n_ij_pp_ij_prod_us, categoria, by = c("produto_categoria_id" = "categoria_id"))

  ## Primeira vez pra verificar quais são os top5
  pr_top5_fat_us <- p_ij_n_ij_pp_ij_prod_us %>%
    select(produto_categoria_id, pp_valor_tot) %>%
    group_by(produto_categoria_id) %>%
    mutate(fat = sum(pp_valor_tot)) %>%
    mutate(n = n()) %>%
    distinct (produto_categoria_id, .keep_all = TRUE) %>%
    ungroup ()


  #Arrumando encoding
  Encoding(categoria$categoria_nome) <- 'latin1'

  ##Aqui estou juntando para agrupar primeiro através do na
  pr_top5_fat_aux <- left_join(pr_top5_fat_us, top5_fat_ij_cat, by=c("produto_categoria_id" = "produto_categoria_id"))
  pr_top5_fat_aux$produto_categoria_id[is.na(pr_top5_fat_aux$categoria_nome)] <- "-1"
  pr_top5_fat_aux$categoria_nome[is.na(pr_top5_fat_aux$categoria_nome)] <- "OUTRAS *"


  ##Depois de setar as linhas q tem nome de coluna = na (não estão no top5) e ter setado todas pra id = -1, faço a soma dos n e faturamentos
  n_out <- sum(pr_top5_fat_aux$n[which(pr_top5_fat_aux$produto_categoria_id=="-1")])
  fat_out <- sum(pr_top5_fat_aux$fat[which(pr_top5_fat_aux$produto_categoria_id=="-1")])

  ##E então, repasso o valor de volta para a coluna "outros", de faturamento total (de todas as categorias menos as q estão no top5) e número de vezes que aparecem
  pr_top5_fat_aux$n[pr_top5_fat_aux$produto_categoria_id=='-1'] <- n_out
  pr_top5_fat_aux$fat[pr_top5_fat_aux$produto_categoria_id=='-1'] <- fat_out

  ##E aqui removo as demais linhas, que já foram adicionadas a "Outros"
  pr_top5_fat_aux <- pr_top5_fat_aux[!is.na(pr_top5_fat_aux$categoria_nome),]

  ##Aqui só renomeio pra ver que a categoria outros tem um * representando a soma de todas as outras categorias também
  #pr_top5_fat_aux$categoria_nome[(pr_top5_fat_aux$categoria_nome == "OUTRA")] <- "OUTRA *" #Não mais necessário pq já tneho que adicionar a categoria outra então já faço com nome que quero


  ## Aqui já estõu fazendo a média das categorias
  pr_top5_fat_med_us <- pr_top5_fat_aux %>%
    select(categoria_nome, produto_categoria_id, fat, n) %>%
    group_by(produto_categoria_id) %>%
    mutate(fat_med = fat/n) %>%
    distinct (produto_categoria_id, .keep_all = TRUE) %>%
    ungroup ()


  fat_tot_categorias_us <- pr_top5_fat_med_us


  ##Adicionando a matriz pra poder exibir no gráfico ###########################################################################################################
  fat_tot_categorias_us <- fat_tot_categorias_us %>%
    dplyr::mutate(med_emp_us = media_empresa_us) %>%
    dplyr::mutate(med_emp_us_t = func_fmt_din(media_empresa_us)) %>%
    dplyr::rename(produto_categoria_id_us = produto_categoria_id, fat_us = fat, n_us = n, fat_med_us = fat_med)
  ## Se windows, elevata, precisa recodificar para UTF-8 na conversão
  Encoding(fat_tot_categorias_us$med_emp_us_t) <- "UTF-8"
  # rename(fat_us = fat) %>%
  # rename(fat_med_us = fat_med) %>%
  # rename(n_us = n)

  ##Adicionando coluna com os valores já em texto para plot
  if(nrow(fat_tot_categorias_us) > 0){
    fat_tot_categorias_us <- fat_tot_categorias_us %>% rowwise() %>%
      mutate(fat_med_us_t = func_fmt_din(fat_med_us))

    ## Se windows, elevata, precisa recodificar para UTF-8 na conversão
    Encoding(fat_tot_categorias_us$fat_med_us_t) <- "UTF-8"
  }

  fat_tot_categorias_n_us <- full_join(fat_tot_categorias, fat_tot_categorias_us, by=("categoria_nome")) %>%
    select(-produto_categoria_id_us)
  ### Gráfico p4 - Ticket médio de novos e usados (se for apenas novos, vai usar p3)
  if(nrow(fat_tot_categorias_us) > 0){
    ax <- list(
      autotick = TRUE,
      title = "",
      showticklabels = TRUE)
    p4 <- plot_ly(fat_tot_categorias_n_us, x = ~categoria_nome)
    p4 <- p4 %>%
      add_trace(type = "bar", y = ~fat_med,
                name = 'Categorias novos',
                marker = list(color = 'lightblue'),
                text = ~paste0("Categoria novos",
                               '<br>' ,
                               categoria_nome,
                               '<br>' ,
                               fat_med_t),
                hoverinfo = "text")

    p4 <- p4 %>%
      add_trace(type = "bar", y = ~fat_med_us,
                name = 'Categorias usados',
                marker = list(color = '#DAA520'),
                text = ~paste0("Categoria usados",
                               '<br>' ,
                               categoria_nome,
                               '<br>' ,
                               fat_med_t_us),
                hoverinfo = "text")


    p4 <- p4 %>%
      add_trace(type = "scatter+lines", mode = "markers+line", y = ~med_emp,
                name = 'Novos (média)',
                line = list(color = '#21618c'),
                text = ~paste0('Ticket médio novos<br>' , med_emp_t),
                hoverinfo = "text",
                marker = list(color = 'red'))

    p4 <- p4 %>%
      add_trace(type = "scatter+lines", mode = "markers+line", y = ~med_emp_us,
                name = 'Usados (média)',
                line = list(color = '#6e2c00'),
                text = ~paste0('Ticket médio usados<br>' , med_emp_us_t),
                hoverinfo = "text",
                marker = list(color = 'red'))

    p4 <- p4 %>%
      layout(
        autosize = T,
        xaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE, title = ''),
        #range nos dois eixos iguais pra ficar na mesma proporção
        yaxis = list(title = '', showgrid = TRUE, zeroline = T),
        ##aqui eu ajusto onde quero que apareça a legenda
        legend = list(orientation = "h", x = 0.5,
                      xanchor = "center",
                      traceorder = 'normal'))
  }else {
    ##Caso não haja informações do período, plotar gráfico s_dados (texto informando que não há informações p/ o período)
    #p4 <- s_dados
    p4 <- include_graphics(s_dados_path)
  }
} else{
  p4 <- NULL
}
if(dash == F){
  p4
}

if (dash == F){
  subplot(p3, p4, shareX = T, shareY = T)
}

if(teste == F){
  #tabelas
  rm(ax, categoria, fat_tot_categorias, p_ij_n_ij_pp_empresa, p_ij_n_ij_pp_empresa_us, pr_top5_fat, pr_top5_fat_aux,
     pr_top5_fat_med, proposta_produto, top5_fat, top5_fat_ij_cat, p_ij_n_ij_pp_ij_prod,
     p_ij_n_ij_v_ij_pp, p_ij_n_ij_v_ij_pp_n, negocio_produto, negocio_aux, fat_tot_categorias_us, p_ij_n_ij_pp_ij_prod_us,
     pr_top5_fat_med_us, pr_top5_fat_us)
  #variáveis
  rm(fat_out, media_empresa, n_out, total_empresa)
}

##################################################################
### Faturamento de propostas em anat por status


##Junção pra termos valor da proposta (preciso do negócio pra filtrar empresa e vendedores se preciso)
p_ij_n_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_pagamento, by = c("proposta_id" = "pp_proposta_id")) %>%
  dplyr::arrange(pp_valor)
##Aqui tenho a soma de faturamento da empresa dividido em categorias
p_ij_n_ij_pp_sum <- p_ij_n_ij_pp %>%
  select (proposta_status, pp_valor) %>%
  group_by(proposta_status) %>%
  mutate (valor_proposta = sum(pp_valor)) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  select (-pp_valor) %>%
  ungroup ()

## Aqui agrupo as categorias
p_ij_n_ij_pp_sum_cat <- p_ij_n_ij_pp_sum %>%
  select (proposta_status, valor_proposta) %>%
  group_by(proposta_status) %>%
  mutate (valor_status = sum(valor_proposta)) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  dplyr::arrange(proposta_status) %>%
  ungroup ()

##Usar pra renomear os status das propostas
status = c("0 - Pendente", "1 - Aceito", "2 - Recusado", "3 - Cancelado", "4 - Finalizado")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
p_ij_n_ij_pp_sum_cat$proposta_status <- with(p_ij_n_ij_pp_sum_cat, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                       labels = status))

###Gráfico p5 - Faturamento de propostas por status (não usado)
if(nrow(p_ij_n_ij_pp_sum_cat) > 0){
  ax <- list(
    autotick = TRUE,
    title = "",
    showticklabels = TRUE)
  p5 <- plot_ly(p_ij_n_ij_pp_sum_cat, x = ~proposta_status, y = ~valor_status, type = 'bar',
                name = 'Faturamento por status',
                marker = list(color = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32")),
                text = ~paste(proposta_status,'<br>' , func_fmt_din(valor_status)),
                hoverinfo = "text")

  p5 <- p5 %>%
    layout(barmode = 'identity', xaxis = ax, yaxis = ax)
}else {
  #p5 <- s_dados
  p5 <- include_graphics(s_dados_path)
}


if(dash == F){
  p5
}
if(teste == F){
  #tabelas
  rm(ax, p_ij_n_ij_pp, p_ij_n_ij_pp_sum, p_ij_n_ij_pp_sum_cat, prop_ij_neg_ij_vend);
  #variáveis
  rm(status);
}
##############################################
##Começando a distribuição de propostas por tipo de pagamento
##Vou selecionar só o que for usar para contar tipos de pagamentos

##Já faço o filtro da empresa aqui na hora de puxar do banco
proposta_modo_forma <- fread("Tabelas/proposta_modo_forma.csv") %>%
  select (pmf_id, pmf_nome, pmf_ativo, PMF_EMPRESA_ID) %>%
  filter (pmf_ativo == 1 & PMF_EMPRESA_ID == empresa) %>%
  select (-pmf_ativo, -PMF_EMPRESA_ID)


#Arrumando encoding
Encoding(proposta_modo_forma$pmf_nome) <- 'latin1'

##Criada sem nome e pmf_empresa (não consegui fazer a junção duas vezes, provavelmente por já ter o tempo)
#proposta_modo_forma_aux <-proposta_modo_forma[,1:2]

p_ij_ppa <- inner_join(proposta, proposta_pagamento, by=c('proposta_id' = 'pp_proposta_id')) %>%
  select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_modo_id, pp_forma_id, pp_usado_id, pp_valor) %>%
  filter (pp_modo_id != 0, pp_forma_id != 0) #, proposta_data_cadastro > 'ano_atual) ##teste de ano atual

p_ij_ppa <- inner_join(p_ij_ppa, negocio, by=c('proposta_negocio_id' = 'negocio_id')) %>%
  dplyr::select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_modo_id, pp_forma_id, pp_usado_id, pp_valor, negocio_vendedor_id) %>%
  dplyr::filter(negocio_vendedor_id == vend_id)

if(teste == T){
  #####################
  ###Apenas para print
  # p_ij_ppa_ij_pmf <- inner_join (p_ij_ppa, proposta_modo_forma, by = c("pp_modo_id" = "pmf_id")) %>%
  #   select(proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_modo_id, pmf_nome) %>%
  #   rename(Modo_pagamento = pmf_nome) %>%
  #   dplyr::arrange(pp_modo_id)
  # write_excel_csv(p_ij_ppa_ij_pmf, "propostas_c_modo_pagamento.csv", delim = ";")
  #####################
}
if(teste == T){
  #####################
  ###Apenas para print
  # p_ij_ppa_ij_pmf <- inner_join (p_ij_ppa, proposta_modo_forma, by = c("pp_forma_id" = "pmf_id")) %>%
  #   select(proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_forma_id, pmf_nome) %>%
  #   rename(Forma_pagamento = pmf_nome) %>%
  #   dplyr::arrange(pp_forma_id)
  # write_excel_csv(p_ij_ppa_ij_pmf, "propostas_c_forma_pagamento.csv", delim = ";")
  #####################
}
##conta antes de substituir (provavelmente mais rápido lidar com int do que string) (1 contando modo) (conta todos, depois filtro através de proposta_modo_forma)
p_ij_ppa_sum_modo <- p_ij_ppa %>%
  select (pp_id, pp_modo_id, pp_valor) %>%
  group_by(pp_modo_id) %>%
  mutate(sum_modo_aux = sum(pp_valor)) %>%
  mutate(count_modo_aux = n()) %>%
  ungroup() %>%
  distinct(pp_modo_id, .keep_all = T)

p_ij_ppa_sum_modo_ij_pmf <- inner_join (p_ij_ppa_sum_modo, proposta_modo_forma, by = c("pp_modo_id" = "pmf_id")) %>%
  select(pp_modo_id, pmf_nome, sum_modo_aux, count_modo_aux) %>%
  rename(Modo = pmf_nome)

##Vou calcular 5% e agrupar esses que forem menores que 5% em outra categoria (outros)
n_total_modo <- sum(p_ij_ppa_sum_modo_ij_pmf$sum_modo_aux)
aux_3perc <- n_total_modo*0.05
if (empresa == 16){
  p_ij_ppa_sum_modo_ij_pmf$pp_modo_id[p_ij_ppa_sum_modo_ij_pmf$sum_modo_aux < aux_3perc] <- 26
  p_ij_ppa_sum_modo_ij_pmf$Modo[p_ij_ppa_sum_modo_ij_pmf$sum_modo_aux < aux_3perc] <- "OUTRAS"
}else if (empresa == 78){
  p_ij_ppa_sum_modo_ij_pmf$pp_modo_id[p_ij_ppa_sum_modo_ij_pmf$sum_modo_aux < aux_3perc] <- 52
  p_ij_ppa_sum_modo_ij_pmf$Modo[p_ij_ppa_sum_modo_ij_pmf$sum_modo_aux < aux_3perc] <- "OUTRA"
}

##Aqui eu estou refazendo a contagem pq já havia uma categoria "Outras", só estou adicionando os demais (com taxa <3%) nela
p_ij_ppa_sum_modo_ij_pmf <- p_ij_ppa_sum_modo_ij_pmf %>%
  select (pp_modo_id, Modo, sum_modo_aux, count_modo_aux) %>%
  group_by(pp_modo_id) %>%
  mutate(sum_modo = sum(sum_modo_aux)) %>%
  mutate(count_modo = sum(count_modo_aux)) %>%
  ungroup() %>%
  distinct(pp_modo_id, .keep_all = T)


#conta antes de substituir (provavelmente mais rápido lidar com int do que string) (2 contando forma) (conta todos, depois filtro através de proposta_modo_forma)
p_ij_ppa_sum_forma <- p_ij_ppa %>%
  select (pp_id, pp_forma_id, pp_valor) %>%
  group_by(pp_forma_id) %>%
  mutate(sum_forma_aux = sum(pp_valor)) %>%
  mutate(count_forma_aux = n()) %>%
  ungroup() %>%
  distinct(pp_forma_id, .keep_all = T)

p_ij_ppa_sum_forma_ij_pmf <- inner_join (p_ij_ppa_sum_forma, proposta_modo_forma, by = c("pp_forma_id" = "pmf_id")) %>%
  select(pp_forma_id, pmf_nome, sum_forma_aux, count_forma_aux) %>%
  rename(Forma = pmf_nome)

##Vou calcular 5% e agrupar esses que forem menores que 5% em outra categoria (outros)
n_total_forma <- sum(p_ij_ppa_sum_forma_ij_pmf$sum_forma_aux)
aux_3perc <- n_total_forma*0.05
if (empresa == 16){##Super não tem forma Outras (usando -1 como valor)
  p_ij_ppa_sum_forma_ij_pmf$pp_forma_id[p_ij_ppa_sum_forma_ij_pmf$sum_forma_aux < aux_3perc] <- -1
  p_ij_ppa_sum_forma_ij_pmf$Forma[p_ij_ppa_sum_forma_ij_pmf$sum_forma_aux < aux_3perc] <- "OUTRAS"
}else if (empresa == 78){
  p_ij_ppa_sum_forma_ij_pmf$pp_forma_id[p_ij_ppa_sum_forma_ij_pmf$sum_forma_aux < aux_3perc] <- 50
  p_ij_ppa_sum_forma_ij_pmf$Forma[p_ij_ppa_sum_forma_ij_pmf$sum_forma_aux < aux_3perc] <- "OUTROS"
}

##Aqui eu estou refazendo a contagem pq já havia uma categoria "Outras", só estou adicionando os demais (com taxa <3%) nela
p_ij_ppa_sum_forma_ij_pmf <- p_ij_ppa_sum_forma_ij_pmf %>%
  select (pp_forma_id, Forma, sum_forma_aux, count_forma_aux) %>%
  group_by(pp_forma_id) %>%
  mutate(sum_forma = sum(sum_forma_aux)) %>%
  mutate(count_forma = sum(count_forma_aux)) %>%
  ungroup() %>%
  distinct(pp_forma_id, .keep_all = T)

### Gráfico p6 - Modos de pagamento
if(nrow(p_ij_ppa_sum_modo_ij_pmf) > 0){
  p6 <- plot_ly()
  p6 <- p6 %>%
    add_pie(data = p_ij_ppa_sum_modo_ij_pmf, values = ~sum_modo, labels = ~Modo,
            hovertemplate = ~paste0("%{label}: ",
                                    "<br>",
                                    "Valor financeiro: ", func_fmt_din(sum_modo),
                                    "<br>",
                                    "Nº de propostas: ", count_modo,
                                    "<br>"),
            name = '') %>%
    layout(legend = list(orientation = 'h'))
}else {
  #p6 <- s_dados
  p6 <- include_graphics(s_dados_path)
}
if(dash == F){
  p6
}

### Gráfico p7 - Formas de pagamento
if(nrow(p_ij_ppa_sum_forma_ij_pmf) > 0){
  p7 <- plot_ly()
  p7 <- p7 %>%
    add_pie(data = p_ij_ppa_sum_forma_ij_pmf, values = ~sum_forma, labels = ~Forma,
            hovertemplate = ~paste0("%{label}: ",
                                    "<br>",
                                    "Valor financeiro: ", func_fmt_din(sum_forma),
                                    "<br>",
                                    "Nº de propostas: ", count_forma,
                                    "<br>"),
            name = '') %>%
    layout(legend = list(orientation = 'h'))
}else {
  #p7 <- s_dados
  p7 <- include_graphics(s_dados_path)
}

if(dash == F){
  p7
}

if(teste == F){
  #tabelas
  # rm(p_ij_ppa, proposta, proposta_modo_forma, p_ij_ppa_count_modo, p_ij_ppa_cont_modo_ij_pmf,
  #    p_ij_ppa_count_forma, p_ij_ppa_cont_forma_ij_pmf, proposta_pagamento, media_empresa_us,
  #    n_empresa, n_empresa_us, total_empresa_us);
  #variáveis
  # if(empresa == 16){rm(aux_3perc, n_outros_forma, n_total_modo)}
}
if (teste == 0) {
  #rm(list=ls())
}
######################################################################################

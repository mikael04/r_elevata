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
teste = F
####Variável usada para não plotar os gráficos na dash
dash = F
## Para o final de ano, só vou diminuir o número de dias até chegar no dia 31 (ex: hoje é dia 04, então vou remover 4d pra voltar pra 2020-12-31)
if(!teste){
  ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
  ano_atual = fct_ano_atual()
  ####Variavel global c/ mês atual (para comparação)
  mes_atual = fct_mes_atual()
  ## Apenas ano, para gerar títulos
  ano <- year(ano_atual)
}else{
  ##Teste setando dia
  data <- lubridate::ymd("2020-12-31")
  data <- as_date(data, lubridate::origin)
  ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
  ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1))
  ####Variavel global c/ mês atual (para comparação)
  mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
  ## Apenas ano, para gerar títulos
  ano <- lubridate::year(ano_atual)
}


##plotando texto sem informações #usado para gráficos que não tiverem nenhuma informação no período
#caminho para imagem de sem dados
s_dados_path <- "s_dados.png"

#empresa = params$variable1
#teste
empresa = 65

#################################################################################
##### Dash geral

#################################################################################
### Funil de vendas (vendas abertas)
########################################################################

negocio <- fread("scripts_final_ano/Tabelas_final_ano/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id)

##coleta todos os vendedores
vendedor <- fread("scripts_final_ano/Tabelas_final_ano/vendedor.csv") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter (vendedor_empresa_id == empresa)

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

vendedor_a <- vendedor %>%
  filter(vendedor_ativo == T)

##negocio_produto para pegar os valores de cada negócio
negocio_produto <- fread("scripts_final_ano/Tabelas_final_ano/negocio_produto.csv", colClasses = c(np_id = "character", np_negocio_id = "character", np_produto_id = "character")) %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade, np_ativo, np_valor) %>%
  filter (np_ativo == T)

##join de negocios e vendedores
negocio_ij_vendedor <- inner_join(negocio, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

########## Para o funil e para agrupar por faturamento
#### Vou alterar o agrupamento de número de negócios para faturamento, portanto precisarei da tabela negocio_produto
neg_ij_ven_ij_np <- inner_join(negocio_ij_vendedor, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##Status aberto
st_a = c(1,2,3,8,10)
##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##Tempo: Total
##status abertos apenas
ng_ij_hist_ij_ven_funil_ab <- neg_ij_ven_ij_np %>%
  filter(negocio_negocio_situacao_id %in% st_a)

##filtrar apenas por empresa
ng_ij_hist_ij_ven_funil_ab <- ng_ij_hist_ij_ven_funil_ab %>%
  filter(vendedor_empresa_id == empresa)

status = c("Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado", "Intenção ou prospecção")
#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_ij_hist_ij_ven_funil_ab <- ng_ij_hist_ij_ven_funil_ab %>%
  mutate(negocio_status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_hist_ij_ven_funil_ab$negocio_status <- with(ng_ij_hist_ij_ven_funil_ab, cut(negocio_negocio_situacao_id, breaks = c(0,1,2,3,8,10),
                                                                                  labels = status))
##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_ab %>%
  select (negocio_status, np_valor, negocio_negocio_situacao_id, np_quantidade) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor*np_quantidade)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  select (-np_valor, -np_quantidade) %>%
  collect()

if (teste == T){
  ###Testando e imprimindo tabela para envio
  #negocio_print <- ng_ij_hist_ij_ven_funil_ab %>%
  #  select (negocio_id, negocio_vendedor_id, vendedor_nome, negocio_status, negocio_data_cadastro, negocio_usado, negocio_produto_id, np_quantidade, np_valor)
  
  #write_excel_csv(negocio_print, "negocios_vendedores_negocio_produto.csv", delim = ";")
  
}

##conversão de faturamento para texto	
if (nrow(ng_ij_hist_ij_ven_funil_fat) > 0){		
  ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_fat %>%		
    mutate(tot_fat_t = func_fmt_din(total_faturado))		
  
  ##Começando a gambiarra (criar nova columa com nome da categoria + valor da categoria)		
  ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_fat %>%		
    mutate(nome_cat_valor_fat = paste(negocio_status, "\n", tot_fat_t))		
  
  ##Vou converter o id de situação pra 0, pra poder ordenar (intenção seria o status 0, é o primeiro)		
  ng_ij_hist_ij_ven_funil_fat$negocio_negocio_situacao_id[ng_ij_hist_ij_ven_funil_fat$negocio_negocio_situacao_id == 10] <- 0		
  ng_ij_hist_ij_ven_funil_fat <- ng_ij_hist_ij_ven_funil_fat %>%		
    arrange(negocio_negocio_situacao_id)		
}

### Gráfico n9 - Funil agrupado por faturamento
if (nrow(ng_ij_hist_ij_ven_funil_fat) > 0){
  n9 <- plot_ly (ng_ij_hist_ij_ven_funil_fat) %>%
    add_trace(
      type ="funnelarea",
      values = ng_ij_hist_ij_ven_funil_fat$total_faturado,
      text = ng_ij_hist_ij_ven_funil_fat$nome_cat_valor_fat,
      textinfo = "text",	
      hovertemplate = paste0 ("%{text} <br>",	
                              "Equivalente a %{percent}",
                              "<extra></extra>"),
      marker = list(colors = c("#ADD8E6", "#87CEEB" , "#87CEFA", "#00BFFF", "#3182FF")),
      showlegend = FALSE
    )
}else {
  n9 <- include_graphics(s_dados_path)
}
if(dash == F){
  n9
}



if (teste == F){
  #tabelas
  rm(neg_ij_ven_ij_np, ng_ij_hist_ij_ven_funil_ab, ng_ij_hist_ij_ven_funil_fat)
  #variáveis
  rm(st_a, status)
}
#############################################################

##Fechados do mês
##Status fechado
st_f = c(4,5,6,7)
status_f = c("Faturado", "Financiamento não aprovado", "Desistencia do cliente", "Perdemos para concorrência")

historico_negocio_situacao <- fread("scripts_final_ano/Tabelas_final_ano/historico_negocio_situacao.csv", colClasses = c(historico_negocio_situacao_situacao_id = "character")) %>%
  select(historico_negocio_situacao_data, historico_negocio_situacao_negocio_id, historico_negocio_situacao_situacao_id)

##filtrando apenas ano atual
historico_negocio_situacao_anat <- historico_negocio_situacao %>%
  filter(historico_negocio_situacao_data >= ano_atual)

negocio_ij_historico_ij_vendedor_total <- dplyr::inner_join(negocio_ij_vendedor, historico_negocio_situacao, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))
negocio_ij_historico_ij_vendedor_anat <-  dplyr::inner_join(negocio_ij_vendedor, historico_negocio_situacao_anat, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))



##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
negocio_ij_historico_ij_vendedor_anat <- negocio_ij_historico_ij_vendedor_anat[order(-negocio_ij_historico_ij_vendedor_anat$historico_negocio_situacao_situacao_id, negocio_ij_historico_ij_vendedor_anat$negocio_id),]
ng_ij_hist_ij_ven_anat <- negocio_ij_historico_ij_vendedor_anat[!duplicated(negocio_ij_historico_ij_vendedor_anat$negocio_id),]

ng_ij_hist_ij_ven_ij_np_anat <- inner_join(ng_ij_hist_ij_ven_anat, negocio_produto, by=c("negocio_id" = "np_negocio_id"))

##vou tentar remover apenas os que tem status fechado (4 - faturado, 5 - financiamento não aprovado, 6 - desistência do cliente, 7 - perdemos para a concorrência)
##Tempo: Total
##status abertos
#ng_rj já havia feito o corte apenas em fechados após anat
ng_ij_hist_ij_ven_fec_anat <- ng_ij_hist_ij_ven_ij_np_anat %>%
  filter(negocio_negocio_situacao_id %in% st_f)

##Funil já filtrou (na verdade ng_ij_hist_ij_ven_anat já filtrou) vendedor ativo e empresa = 16

#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_ij_hist_ij_ven_fec_anat <- ng_ij_hist_ij_ven_fec_anat %>%
  mutate(negocio_status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_hist_ij_ven_fec_anat$negocio_status <- with(ng_ij_hist_ij_ven_fec_anat, cut(negocio_status, breaks = c(0,4,5,6,7),
                                                                                  labels = status_f))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_hist_ij_ven_funil_fat_fec_anat <- ng_ij_hist_ij_ven_fec_anat %>%
  select (negocio_status, np_valor) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  collect()


##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_ij_hist_ij_ven_funil_fat_fec_anat  <- ng_ij_hist_ij_ven_funil_fat_fec_anat  %>%
  select (negocio_status, total_faturado) %>%
  mutate(tot_fat_m = as.integer(total_faturado/1000000))


if (nrow(ng_ij_hist_ij_ven_funil_fat_fec_anat) > 0){		
  ng_ij_hist_ij_ven_funil_fat_fec_anat <- ng_ij_hist_ij_ven_funil_fat_fec_anat %>%	rowwise() %>%	
    mutate(total_fat_t = func_fmt_din_small(total_faturado))		
}

### Gráfico n10 - Pizza fechados do ano
##############################################

######################################################################################################
## Adicionando cores através de junção
cor <- c("#32CD32", "yellow" , "orange" , "#DE0D26")
df_stat_cor <- tibble(status_f, cor)
ng_ij_hist_ij_ven_funil_fat_fec_anat <- inner_join(ng_ij_hist_ij_ven_funil_fat_fec_anat, df_stat_cor, by=c("negocio_status" = "status_f"))
######################################################################################################
if (nrow(ng_ij_hist_ij_ven_funil_fat_fec_anat) > 0 & sum(ng_ij_hist_ij_ven_funil_fat_fec_anat$total_faturado) > 0){
  n10 <- plot_ly(ng_ij_hist_ij_ven_funil_fat_fec_anat, labels = ~negocio_status, values = ~total_faturado, type = 'pie', sort = F,
                 text = ~total_fat_t,
                 texttemplate = "%{text} (%{percent})",	
                 hovertemplate = paste0 ("%{label} <br>",	
                                         "%{text}<br>",	
                                         "Equivalente a %{percent}",
                                         "<extra></extra>"),
                 marker = list(colors = ~cor)) %>%
    layout(legend = list(orientation = 'h'))
}else {
  n10 <- include_graphics(s_dados_path)
}

if(dash == F){
  n10
}

##############################################
##Criando nova tabela com dados apenas do mês anterior
##Auxiliares para meses
mes_ant <- fct_mes_ant(mes_atual)

ng_ij_hist_ij_ven_funil_fat_fec_anat_mes <- ng_ij_hist_ij_ven_fec_anat %>%
  filter(mes_ant <= historico_negocio_situacao_data,  historico_negocio_situacao_data < mes_atual) %>%
  mutate(negocio_status = negocio_negocio_situacao_id)

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_hist_ij_ven_funil_fat_fec_anat_mes$negocio_status <- with(ng_ij_hist_ij_ven_funil_fat_fec_anat_mes, cut(negocio_negocio_situacao_id, breaks = c(0,4,5,6,7),
                                                                                                              labels = status_f))

##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_hist_ij_ven_funil_fat_fec_anat_mes <- ng_ij_hist_ij_ven_funil_fat_fec_anat_mes %>%
  select (negocio_status, np_valor) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  collect()


##Coluna nova criada para facilitar compreensão (diminui as casas para exibir em milhões)
ng_ij_hist_ij_ven_funil_fat_fec_anat_mes  <- ng_ij_hist_ij_ven_funil_fat_fec_anat_mes  %>%
  select (negocio_status, total_faturado) %>%
  mutate(tot_fat_m = as.integer(total_faturado/1000000))


#criando uma coluna nova para usar como texto dentro do gráfico
if (nrow(ng_ij_hist_ij_ven_funil_fat_fec_anat_mes) > 0) {	
  ng_ij_hist_ij_ven_funil_fat_fec_anat_mes <- ng_ij_hist_ij_ven_funil_fat_fec_anat_mes %>%	rowwise() %>%		
    mutate(total_fat_t = func_fmt_din_small(total_faturado))	
}

### Gráfico n11 - Pizza fechados no último mês
##############################################

colors_pie <- c("#32CD32", "yellow" , "orange" , "#DE0D26")

if (nrow(ng_ij_hist_ij_ven_funil_fat_fec_anat_mes) > 0 & sum(ng_ij_hist_ij_ven_funil_fat_fec_anat_mes$total_faturado) > 0){
  n11 <- plot_ly(ng_ij_hist_ij_ven_funil_fat_fec_anat_mes, labels = ~negocio_status, values = ~total_faturado, type = 'pie', sort = F,
                 text = ~total_fat_t,
                 texttemplate = "%{text} (%{percent})",	
                 hovertemplate = paste0 ("%{label} <br>",	
                                         "%{text}<br>",
                                         "%{text} milhões <br>",
                                         "Equivalente a %{percent}",
                                         "<extra></extra>"),
                 marker = list(colors = colors_pie))
}else {
  n11 <- include_graphics(s_dados_path)
}


if(dash == F){
  n11
}

if (teste == F){
  #tabelas
  rm(ng_ij_hist_ij_ven_funil_fat_fec_anat, ng_ij_hist_ij_ven_funil_fat_fec_anat_mes, ng_ij_hist_ij_ven_fec_anat)
  #variáveis
  rm(colors_pie, st_f, status_f)
}
#####################################################################
### Faturamento anual (ano atual + dois anteriores se disponíveis)

##Filtrando empresa, vendedores ativos e negocios de anat
ng_ij_hist_ij_ven_anat_fat <- ng_ij_hist_ij_ven_ij_np_anat %>%
  filter(negocio_negocio_situacao_id == 4)	
flag_fat = NULL		
if(nrow(ng_ij_hist_ij_ven_anat_fat) > 0){		
  flag_fat = F		
} else{		
  flag_fat = T		
}

##Já filtrado apenas empresa (variavel global)


##Adicionando "0" para ficar padrão a todos, gerando apenas para os meses (do ano atual) que já passaram
ym_aux <- fct_meses_ant(mes_atual)
ym <- as.character(ym_aux)
for (i in (1:max(ym_aux))){
  if(ym_aux[i] < 10){
    ym[i] <- paste("0", ym[i], sep="")
  }
}


fat_anat_mes_aux <-data.frame(ym)
fat_anat_mes <- ng_ij_hist_ij_ven_anat_fat %>%
  mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
  group_by (ym) %>%
  summarize(ym_sum = sum(np_valor), .groups = 'drop') %>%
  mutate (ym = as.character(ym)) %>%
  ungroup()

fat_anat_mes <- left_join(fat_anat_mes_aux, fat_anat_mes, by = c('ym'))
#fat_anat_mes$ym_sum[is.na(fat_anat_mes$ym_sum)] <- 0

##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
negocio_ij_historico_ij_vendedor_total <- negocio_ij_historico_ij_vendedor_total[order(-negocio_ij_historico_ij_vendedor_total$historico_negocio_situacao_situacao_id, negocio_ij_historico_ij_vendedor_total$negocio_id),]
ng_ij_hist_ij_ven_total <- negocio_ij_historico_ij_vendedor_total[!duplicated(negocio_ij_historico_ij_vendedor_total$negocio_id),]


##tabela vem dessa: negocio_ij_historico_ij_vendedor_total, todos os vendedores, filtrando empresa_id, vendedor_ativo
ng_ij_hist_ij_ven_total <- ng_ij_hist_ij_ven_total %>%
  filter(vendedor_empresa_id == empresa) %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_vendedor_id, vendedor_nome, negocio_data_cadastro, historico_negocio_situacao_data)

##filtrando dois negócios que tem seu status 0 (possivel erro)
ng_ij_hist_ij_ven_total <- ng_ij_hist_ij_ven_total %>%
  filter(negocio_negocio_situacao_id != 0)


ng_ij_hist_ij_ven_ij_np_total <- inner_join(ng_ij_hist_ij_ven_total, negocio_produto, by=c("negocio_id" = "np_negocio_id"))


##Começando o cálculo do faturamento médio dos três anos anteriores

anos_ant = year(today()) - min(year(ng_ij_hist_ij_ven_ij_np_total$historico_negocio_situacao_data))

ano_1ant = ymd(ano_atual) - years(1)
ano_2ant = ymd(ano_atual) - years(2)

##primeiro vou selecionar por ano, e depois apenas status faturado
if(anos_ant > 0) {
  ng_ij_hist_ij_ven_ij_np_1ant_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
    filter (historico_negocio_situacao_data < ano_atual & historico_negocio_situacao_data >= ano_1ant & negocio_negocio_situacao_id == 4)
}
if(anos_ant > 1) {
  ng_ij_hist_ij_ven_ij_np_2ant_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
    filter (historico_negocio_situacao_data < ano_1ant & historico_negocio_situacao_data >= ano_2ant & negocio_negocio_situacao_id == 4)
}

##Usando só os dois anos anteriores
#ng_ij_hist_ij_ven_ij_np_2017_fat <- ng_ij_hist_ij_ven_ij_np_total %>%
#  filter (historico_negocio_situacao_data < '2ant-01-01' & historico_negocio_situacao_data > '2016-12-31' & negocio_negocio_situacao_id == 4)

ym <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#Aqui vou fazer a mesma soma dos valores dos três anos anteriores, dividindo por mês
if(anos_ant > 1){
  if(nrow(ng_ij_hist_ij_ven_ij_np_2ant_fat) > 0){
    fat_2ant_mes <- ng_ij_hist_ij_ven_ij_np_2ant_fat %>%
      mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
      group_by (ym) %>%
      summarize(ym_sum_2ant = sum(np_valor), .groups = 'drop')%>%
      ungroup ()
    
    fat_1ant_mes <- ng_ij_hist_ij_ven_ij_np_1ant_fat %>%
      mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
      group_by (ym) %>%
      summarize(ym_sum_1ant = sum(np_valor), .groups = 'drop') %>%
      ungroup ()
  }else{
    ym_sum_2ant <- rep(NA, 12)
    fat_2ant_mes <- data.frame(ym, ym_sum_2ant)
    anos_ant = 1
  }
}
if(anos_ant > 0) {
  if(nrow(ng_ij_hist_ij_ven_ij_np_1ant_fat) > 0){
    fat_1ant_mes <- ng_ij_hist_ij_ven_ij_np_1ant_fat %>%
      mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
      group_by (ym) %>%
      summarize(ym_sum_1ant = sum(np_valor), .groups = 'drop') %>%
      ungroup ()
  }else{
    ym_sum_1ant <- rep(NA, 12)
    fat_1ant_mes <- data.frame(ym, ym_sum_1ant)
    anos_ant = 0
  }
}

###Fazer o gráfico de linhas com faturamento anual (substituindo com NA linhas faltantes)
###Na super temos 2ant, na komatsu tem q verificar se possui 2ant e 1ant
#######################################################################
n_linhas <- nrow(fat_anat_mes)
fat_anat_mes_aux <- data.frame(ym)
fat_anat_mes_aux$ym_sum <- NA
fat_anat_mes_aux$ym_sum[1:n_linhas] <- fat_anat_mes$ym_sum
fat_anat_1ant_2ant_mes <- data.frame(ym)
fat_anat_1ant_2ant_mes <- left_join(fat_anat_1ant_2ant_mes, fat_anat_mes, by = c("ym"))

if(anos_ant > 1){
  fat_anat_1ant_2ant_mes <- left_join(fat_anat_1ant_2ant_mes, fat_1ant_mes, by = c("ym"))
  fat_anat_1ant_2ant_mes <- left_join(fat_anat_1ant_2ant_mes, fat_2ant_mes, by = c("ym"))
}else{
  if(anos_ant > 0) {
    fat_anat_1ant_2ant_mes <- left_join(fat_anat_1ant_2ant_mes, fat_1ant_mes, by = c("ym"))
  }
}


meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
#meses = c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')
## alterando pra números pra poder fazer da mesma forma
fat_anat_1ant_2ant_mes$ym <- as.integer(fat_anat_1ant_2ant_mes$ym)
##Vou usar dessa forma, ao invés de transformar, irei apenas criar uma nova coluna (poderia ser feito com um join também)
fat_anat_1ant_2ant_mes <- fat_anat_1ant_2ant_mes %>%
  mutate(mes = cut(ym,breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
                   labels = meses))
if(anos_ant > 0) {
  fat_anat_1ant_2ant_mes$ym_sum_1ant[is.na(fat_anat_1ant_2ant_mes$ym_sum_1ant)] <- 0
}
if(anos_ant > 1) {
  fat_anat_1ant_2ant_mes$ym_sum_2ant[is.na(fat_anat_1ant_2ant_mes$ym_sum_2ant)] <- 0
}
# Aqui eu vou preencher com valores NA para os meses seguintes (se estamos em jan, só teremos valores de jan, então os demais serão NA para não aparecerem como zero no plot)
for (value in fat_anat_1ant_2ant_mes$ym){
  if(mes_atual < value){
    fat_anat_1ant_2ant_mes$ym_sum[value] <- NA
  }else{
  }
}
#######################################################################

##Começando o gráfico
a <- list(
  title = '',
  showticklabels = T
)
ay <- list(
  overlaying = 'y',
  side = 'right',
  title = ''
)
if(flag_fat == T && anos_ant == 0) {	
  fat_anat_1ant_2ant_mes <- fat_anat_1ant_2ant_mes %>%	
    dplyr::mutate(ym_sum_fat = paste0("R$0"))	
} else{	
  fat_anat_1ant_2ant_mes <- fat_anat_1ant_2ant_mes %>% rowwise() %>%
    dplyr::mutate(ym_sum_fat = func_fmt_din(ym_sum))
}


###Assim estarei mostrando anat, 1ant e 2ant
### Gráfico n12 - Faturamento anual (ano atual + dois anteriores)
n12 <- plot_ly(fat_anat_1ant_2ant_mes
               #,width = 800, height = 700
)
n12 <- n12 %>%
  add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~ym_sum, yaxis = ay,
            name = paste('Faturamento de', ano),
            text = ~ym_sum_fat,
            hovertemplate = paste0 ("Faturamento de ", ano,
                                    "<br>",
                                    "", fat_anat_1ant_2ant_mes$mes,
                                    "<br>",
                                    "%{text} milhões <br>",
                                    "<extra></extra>"),
            color = I("green"))
if(anos_ant > 0) {
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y =~ym_sum_1ant,
              name = paste('Faturamento de', ano-1),
              text = ~func_fmt_din_milhoes(ym_sum_1ant),
              hovertemplate = paste0 ("Faturamento de ", ano-1,
                                      "<br>",
                                      "", fat_anat_1ant_2ant_mes$mes,
                                      "<br>",
                                      "%{text} milhões <br>",
                                      "<extra></extra>"),
              color = I("#4682B4")
    )
}
if(anos_ant > 1) {
  n12 <- n12 %>%
    add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~ym_sum_2ant, yaxis = ay,
              name = paste('Faturamento de', ano-2),
              text = ~func_fmt_din_milhoes(ym_sum_2ant),
              hovertemplate = paste0 ("Faturamento de ", ano-2,
                                      "<br>",
                                      "", fat_anat_1ant_2ant_mes$mes,
                                      "<br>",
                                      "%{text} milhões <br>",
                                      "<extra></extra>"),
              color = I("#1E90FF"))
}
n12 <- n12 %>%
  layout(xaxis = list(title = ''), yaxis = list(title = '')
         #aqui eu ajusto onde quero que apareça a legenda
         ,legend = list(orientation = 'h', x=0.2, y=-0.2,
                        xanchor = "bottom")#)
  )
if (dash == F){
  n12
}


if (teste == F){
  #tabelas
  rm(ng_ij_hist_ij_ven_ij_np_anat, ng_ij_hist_ij_ven_total,
     fat_anat_mes_aux, ay, a,
     ng_ij_hist_ij_ven_anat_fat, ng_ij_hist_ij_ven_ij_np_total)
  if(anos_ant > 0){
    rm(fat_1ant_mes, ng_ij_hist_ij_ven_ij_np_1ant_fat)
    if (anos_ant > 1){
      rm(fat_2ant_mes, ng_ij_hist_ij_ven_ij_np_2ant_fat)
    }
  }
  #variáveis
  rm(meses, n_linhas, ym, anos_ant)
}

###Contar clientes/visitas/negocios cadastrados por mês -> vem do script visita_clientes
####################################

cliente <- fread("scripts_final_ano/Tabelas_final_ano/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_ultima_visita) %>%
  filter(cliente_empresa_id == empresa)


##Collect cria o df resultado da query, nesse caso, visitas_cliente, já filtrando apenas ano atual
visita_cliente <- fread("scripts_final_ano/Tabelas_final_ano/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro) %>%
  filter (vc_data_cadastro >= ano_atual)

##Pra pegar o nome do status
visita_status <- fread("scripts_final_ano/Tabelas_final_ano/visita_status.csv") %>%
  select(vs_id, vs_nome, vs_ativo) %>%
  filter(vs_ativo == 1) %>%
  select(-vs_ativo) %>%
  rename (motivo = vs_nome)

##Pra filtrar os status da empresa
visita_status_empresa <- fread("scripts_final_ano/Tabelas_final_ano/visita_status_empresa.csv") %>%
  select(vse_status_id, vse_empresa_id, vse_ativo) %>%
  filter(vse_ativo == 1, vse_empresa_id == empresa) %>%
  select(-vse_ativo)

#Arrumando encoding
Encoding(visita_status$motivo) <- 'latin1'

##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id'))

##Já filtrado apenas empresa (variavel global)
ym_aux <- fct_meses_ant(mes_atual)
ym <- as.character(ym_aux)

clientes_mes_aux <- data.frame(ym)
clientes_mes <- cliente %>%
  filter (cliente_data_cadastro >= ano_atual) %>%
  mutate (ym = as.integer(format(cliente_data_cadastro, '%m'))) %>%
  group_by (ym) %>%
  summarize(n_cli = n(), .groups = 'drop') %>%
  mutate(ym = as.character(ym)) %>%
  ungroup ()

clientes_mes <- left_join(clientes_mes_aux, clientes_mes, by = c("ym"))

clientes_mes$n_cli[is.na(clientes_mes$n_cli)] <- 0

##Visita precisa juntar com visita_status ou _resultado () pra obter empresa_id ##poderia ser vendedor, mas a tabela vis_st_emp (status c/ status_empresa) já está pronta
##vis_st_emp já vem filtrada pela empresa (var global)
vc_ij_emp <- inner_join(visita_cliente, vis_st_emp, by = c("vc_status_id" = "vs_id")) %>%
  #select(vc_id, vc_status_id, vc_data_cadastro) ##Sem empresa_id
  select(vc_id, vc_status_id, vc_data_cadastro, vse_empresa_id)

visitas_mes_aux <- data.frame(ym)
visitas_mes <- vc_ij_emp %>%
  filter (vc_data_cadastro >= ano_atual) %>%
  mutate (ym = as.integer(format(vc_data_cadastro, '%m'))) %>%
  group_by (ym) %>%
  summarize(n_vis = n(), .groups = 'drop') %>%
  mutate(ym = as.character(ym)) %>%
  ungroup ()

visitas_mes <- left_join(visitas_mes_aux, visitas_mes, by = c("ym"))
visitas_mes$n_vis[is.na(visitas_mes$n_vis)] <- 0

##Negocio precisa juntar com vendedor pra obter empresa_id ##Poderia ser cliente também, mas a tabela vendedor é menor
##vendedor já vem filtrado pela empresa (var global)
ng_ij_emp <- inner_join(negocio, vendedor, by = c("negocio_vendedor_id" = "vendedor_id")) %>%
  #select(negocio_id, negocio_vendedor_id, negocio_data_cadastro)%>% ##Sem empresa_id
  select(negocio_id, negocio_vendedor_id, negocio_data_cadastro, vendedor_empresa_id)

negocios_mes_aux <- data.frame(ym)
negocios_mes <- ng_ij_emp %>%
  filter (negocio_data_cadastro >= ano_atual) %>%
  mutate (ym = as.integer(format(negocio_data_cadastro, '%m'))) %>%
  group_by (ym) %>%
  summarize(n_neg = n(), .groups = 'drop') %>%
  mutate(ym = as.character(ym)) %>%
  ungroup ()

negocios_mes <- left_join(negocios_mes_aux, negocios_mes, by = c("ym"))
negocios_mes$n_neg[is.na(negocios_mes$n_neg)] <- 0
##Juntando tudo em um só dataframe
cli_ij_vc_mes <- inner_join(clientes_mes, visitas_mes, by = c("ym"))
cli_ij_vc_ij_ng_mes <- inner_join(cli_ij_vc_mes, negocios_mes, by = c("ym"))


##Criando nomes de colunas
meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
## alterando pra números pra poder fazer da mesma forma
cli_ij_vc_ij_ng_mes$ym <- as.integer(cli_ij_vc_ij_ng_mes$ym)

##Vou usar dessa forma, ao invés de transformar, irei apenas criar uma nova coluna (poderia ser feito com um join também)
cli_ij_vc_ij_ng_mes <- cli_ij_vc_ij_ng_mes %>%
  mutate(mes = cut(ym,breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
                   labels = meses))
# Aqui eu vou preencher com valores NA para os meses seguintes (se estamos em jan, só teremos valores de jan, então os demais serão NA para não aparecerem como zero no plot)
for (value in cli_ij_vc_ij_ng_mes$ym){
  if(fct_data_num_meses(mes_atual) < value){
    cli_ij_vc_ij_ng_mes$n_cli[value] <- NA
    cli_ij_vc_ij_ng_mes$n_vis[value] <- NA
    cli_ij_vc_ij_ng_mes$n_neg[value] <- NA
  }else{
  }
}
###Não farei dessa forma pois quero manter a coluna
#cli_ij_vc_ij_ng_mes$ym <- with(cli_ij_vc_ij_ng_mes, cut(ym, breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
#                                                        labels = meses))

### Gráfico c3 - Cadastro de clientes, visitas e negócios no ano de 2020
c3 <- plot_ly(cli_ij_vc_ij_ng_mes,
              #width = 800, height = 650,
              type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~n_cli,
              name = 'Clientes',
              text = ~paste(n_cli, 'clientes'),
              hoverinfo = "text",
              color = I('#7B68EE'))
c3 <- c3 %>%
  add_trace (type = 'scatter', mode = 'lines+markers', y = ~n_vis,
             name = 'Visitas',
             text = ~paste(n_vis, 'visitas'),
             hoverinfo = "text",
             color = I('#DAA520'))
c3 <- c3 %>%
  add_trace (type = 'scatter', mode = 'lines+markers', y = ~n_neg,
             name = 'Negócios',
             text = ~paste(n_neg, 'negócios'),
             hoverinfo = "text",
             color = I('green'))

c3 <- c3 %>%
  layout(xaxis = list(title = '', range = c(min(0), max(12))), ##Dessa forma pego os 12 meses do ano
         yaxis = list(title = ''),
         legend = list(orientation = 'h', x=0.2, y=-0.25,
                       xanchor = "bottom"))
if(dash == F){
  c3
}

if(teste == F){
  #tabelas
  rm(cli_ij_vc_mes, cli_ij_vc_ij_ng_mes);
  #variáveis
  rm();
}
################################################################################################################################

################################################################################################
### Tabelas e graficos de Clientes

##Juntando visita_cliente com os motivos (status)
vc_ij_vse <- inner_join(visita_cliente, vis_st_emp, by = c('vc_status_id' = 'vs_id'))%>%
  select (vc_id, vc_vendedor_id, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vse_ij_v <- inner_join(vc_ij_vse, vendedor_a, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  select (vc_id, vc_vendedor_id, vendedor_nome, vc_status_id, motivo)

##Agrupando por vendedor e por motivo (vc_status_id, depois mostrar so motivo)
vc_ij_vse_ij_v <- vc_ij_vse_ij_v %>%
  group_by(vc_vendedor_id, vc_status_id) %>%
  mutate(motivo_n = n()) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup()

##Clientes cadastrados por vendedor
cli_p_v <- cliente %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes) %>%
  arrange(cliente_vendedor_id) %>%
  ungroup()

cli_p_v$n_clientes[is.na(cli_p_v$n_clientes)] <- 0

##Juncao pra pegar nome do vendedor
cli_p_v_ij_vend <- inner_join(cli_p_v, vendedor_a, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
  select(cliente_vendedor_id, vendedor_nome, n_clientes)


##Já é filtrado em anat (filtra na tabela de visitas_cliente)
vc_ij_vse_ij_v_count <- vc_ij_vse_ij_v %>%
  select (vc_vendedor_id, motivo_n) %>% ##Não vou salvar o nome pq vou fazer uma junção com a outra tabela, então só preciso do id
  group_by(vc_vendedor_id) %>%
  mutate(n_visitas = sum(motivo_n)) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup ()

vc_ij_vse_ij_v_count$n_visitas[is.na(vc_ij_vse_ij_v_count$n_visitas)] <- 0

##Distribuição de clientes total, visita e negócios em anat por vendedor
vend_cli_vis <- left_join(cli_p_v_ij_vend, vc_ij_vse_ij_v_count, by = c("cliente_vendedor_id" = "vc_vendedor_id")) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_visitas)

##Contar quantos negócios são feitos por vendedor em anat
negocio <- fread("scripts_final_ano/Tabelas_final_ano/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character", negocio_cliente_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_cliente_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_produto_id)

##Juncao pra pegar a empresa do negócio (e vendedor)
neg_ij_vend <- inner_join(negocio, vendedor_a, by = c('negocio_vendedor_id' = 'vendedor_id')) %>%
  select(negocio_id, negocio_vendedor_id, negocio_data_cadastro)

neg_ij_vend_count <- neg_ij_vend %>%
  filter (negocio_data_cadastro >= ano_atual) %>%
  select(negocio_vendedor_id) %>%
  group_by(negocio_vendedor_id) %>%
  mutate(n_negocios = n()) %>%
  distinct(negocio_vendedor_id, .keep_all = T) %>%
  ungroup()

neg_ij_vend_count$n_negocios[is.na(neg_ij_vend_count$n_negocios)] <- 0

vend_cli_vis_neg <- left_join(vend_cli_vis, neg_ij_vend_count, by = c("cliente_vendedor_id" = "negocio_vendedor_id")) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_visitas, n_negocios)

### Grafico c0 - Distribuicao de clientes (total), visitas (anat) e negocios (anat) cadastrados por vendedor
if (nrow(vend_cli_vis_neg) > 0){
  c0 <- plot_ly(vend_cli_vis_neg, x = ~n_clientes, y= ~reorder(vendedor_nome, desc(vendedor_nome)), type = 'bar',
                name = 'Clientes (total)')
  c0 <- c0 %>%
    add_trace(x= ~n_visitas, name =  paste('Visitas', ano))
  c0 <- c0 %>%
    add_trace(x= ~n_negocios, name = paste('Negócios', ano))
  c0 <- c0 %>%
    layout(barmode = 'grouped',
           xaxis = list(title = '', tickangle = 30, tickfont = list(size = 12)),
           yaxis = list(title = ''),
           legend = list(orientation = 'h',
                         xanchor = "bottom"))
}else {
  c0 <- include_graphics(s_dados_path)
}
if(dash == F){
  c0
}

if(teste == F){
  #tabelas
  rm(cli_p_v, vend_cli_vis, vend_cli_vis_neg, neg_ij_vend, neg_ij_vend_count, vc_ij_vse_ij_v,
     vc_ij_vse_ij_v_count, cli_p_v_ij_vend);
  #variáveis
  rm();
}
### Gráficos de pizza de clientes com e sem negócio
##Contando número de clientes geral e em anat
n_clientes <- nrow(cliente)
##Clientes cadastrados em anat por vendedor
cliente_anat <- cliente %>%
  filter(cliente_data_cadastro >= ano_atual)
n_clientes_anat <- nrow(cliente_anat)

##Contar quantos clientes aparecem em negócio
cli_ij_ng <- inner_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  select(cliente_id, negocio_id)
cli_anat_ij_ng <- inner_join(cliente_anat, negocio, by=c("cliente_id" = "negocio_cliente_id"))%>%
  select(cliente_id, negocio_id)
##Variáveis usadas
n_cli_cneg <- nrow(cli_ij_ng)
n_cli_cneg_anat <- nrow(cli_anat_ij_ng)

Clientes <- c("Clientes com negócios", "Clientes sem negócio")

n_total <- c(n_cli_cneg, n_clientes-nrow(cli_ij_ng))
n_total_p <- c(round((n_cli_cneg/n_clientes), 2), round(((n_clientes-n_cli_cneg)/n_clientes), 2))

n_anat <- c(n_cli_cneg_anat, n_clientes_anat-n_cli_cneg_anat)
n_anat_p <- c(round((n_cli_cneg_anat/n_clientes_anat), 2), round(((n_clientes_anat-n_cli_cneg_anat)/n_clientes_anat), 2))

cli_c_s_ng <- data.frame(Clientes, n_total, n_total_p, n_anat, n_anat_p)


### Grafico c1 - Clientes cadastrados que possuem negocio (pizza)
colors_pie<- c("#32CD32", "#FFA500")
c1 <- plot_ly(cli_c_s_ng, labels = ~Clientes, values = ~n_total, type = 'pie', sort = F,
              text = n_total,
              texttemplate = "%{text} (%{percent})",
              hovertemplate = paste0 ("%{label}: %{text}<br>",
                                      "Equivalente a %{percent}",
                                      "<extra></extra>"),
              marker = list(colors = colors_pie))

if(dash == F){
  c1
}

### Grafico c2 - Clientes cadastrados em anat que possuem negocio (pizza)
if (sum(cli_c_s_ng$n_anat) > 0){
  c2 <- plot_ly(cli_c_s_ng, labels = ~Clientes, values = ~n_anat, type = 'pie', sort = F,
                text = n_anat,
                texttemplate = "%{text} (%{percent})",
                hovertemplate = paste0 ("%{label}: %{text}<br>",
                                        "Equivalente a %{percent}",
                                        "<extra></extra>"),
                marker = list(colors = colors_pie))
}else {
  c2 <- include_graphics(s_dados_path)
}
if(dash == F){
  c2
}
t <- list(
  family = "sans serif",
  size = 16)
if (sum(cli_c_s_ng$n_anat) > 0){
  c1_c2 <- plot_ly(cli_c_s_ng, labels = ~Clientes, values = ~n_total, type = 'pie', sort = F,
                   title = list(text = "Total", font = t),
                   domain = list(row = 0, column = 0),
                   text = n_total,
                   textposition = 'inside',
                   texttemplate = "%{text} (%{percent})",
                   hovertemplate = paste0 ("%{label}: %{text}<br>",
                                           "Equivalente a %{percent}",
                                           "<extra></extra>"),
                   marker = list(colors = colors_pie)) %>%
    
    add_pie(cli_c_s_ng, labels = ~Clientes, values = ~n_anat, type = 'pie', sort = F,
            title = list(text = paste0("", ano), font = t),
            domain = list(row = 1, column = 0),
            text = n_anat,
            textposition = 'inside',
            texttemplate = "%{text} (%{percent})",
            hovertemplate = paste0 ("%{label}: %{text}<br>",
                                    "Equivalente a %{percent}",
                                    "<extra></extra>"),
            marker = list(colors = colors_pie)) %>%
    layout(showlegend = T, title = F,
           grid=list(rows=2, columns=1),
           legend = list(orientation = 'h', #x=0.2, y=-0.2,
                         xanchor = "bottom")
    )
} else{
  c1_c2 <- include_graphics(s_dados_path)
}
if(dash == F){
  c1_c2
}
if(teste == F){
  #tabelas
  rm(cli_anat_ij_ng, cli_c_s_ng, Clientes, n_total, n_total_p, n_anat, n_anat_p, cliente_anat);
  #variáveis
  rm(n_cli_cneg, n_cli_cneg_anat, n_clientes, n_clientes_anat, colors_pie);
}
####################################
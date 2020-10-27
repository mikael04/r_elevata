#rm(list = ls())
#Lib q será futuramente usada pros painéis interativos
#library(shiny)
#Lib pra conexão com o banco
#library(odbc)
#Lib para ler (mais rapidamente) os csvs
library(data.table)
#lib com uma cacetada de outras libs para manipular dados
library(tidyverse)
#Libs pra trabalhar com a base (cortes e funções similares ao SQL)
#library(dplyr) #Contido no tidyverse
#Lib pro gráfico
library(ggplot2)
#lib pros gráficos mais "interativos"
library(plotly)
library(highcharter)
#library(htmlwidgets)
#Lib pra usar paletas de cores
library(RColorBrewer)
#library(viridis)
#Lib usada pros treemaps
#library(treemap)
#Lib usada pros waffles
#library(waffle)
#usada para converter números em moeda
#library(scales)
#Lib para lidar com o tempo
library(lubridate)
#Lib usada para emojis/fonts/box de valores
library(ggplot2)
#Lib usada para os quadros
#library(emojifont)



###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = F
####Variável usada para não plotar os gráficos na dash
dash = F
####Variavel global c/ ano atual (para comparação)
ano_atual = ymd(today()) - months(month(today())-1) - days(day(today())-1)
####Variável global para ver se tem usados Ainda não usada
#usados = T


##Variável "Global"
emp_am = 42 # Amazonia
emp_ar = 77 # Araguaia
emp_ko = 78 # Komatsu
emp_ms = 35 # Ms
emp_si = 59 # Simex
emp_su = 16 # Super
emp_ta = 60 # Taisa

  empresa <- emp_su

###################################

##Alterar o valor de inteiro para reais
func_fmt_din <- function(inteiro)
{
  inteiro_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_em_reais)
}
##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) -> posteriormente adicionar o "mi"
func_fmt_din_mi <- function(inteiro)
{
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1))
  return(inteiro_mi_em_reais)
}
##Alterar o nome completo pra primeiro nome mais iniciais dos sobrenomes
func_nome <- function (nome_comp)
{
  lista <- str_split_fixed(nome_comp, " ", 4)
  lista <- lista[, -4]
  lista[,3] = str_sub(lista[,3], 1, 1)
  lista[,2] = str_sub(lista[,2], 1, 1)
  lista[,2] = paste(lista[,2], '.', sep='')
  lista[,3] = paste(lista[,3], '.', sep='')
  lista[,2] <- gsub('^.$', '',lista[,2])
  lista[,3] <- gsub('^.$', '',lista[,3])
  lista[,1] <- paste(lista[,1], lista[,2], lista[,3], sep=' ')
  return (lista[,1])
}
##Alterar o número apresentado na forma americana (com vírgula) para a forma brasileira (com ponto), através da transformação em string
func_fmt_numbr <- function(inteiro)
{
  inteiro_br <- paste("", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_br)
}

############################################################################################
##Declaração das tabelas usadas
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id)


##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter (vendedor_empresa_id == empresa, vendedor_ativo == 1) %>%
  select(-vendedor_ativo)


#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

##negocio_produto para pegar os valores de cada negócio
negocio_produto <- fread("Tabelas/negocio_produto.csv", colClasses = c(np_id = "character", np_negocio_id = "character", np_produto_id = "character")) %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade, np_ativo, np_valor)

historico_negocio_situacao <- fread("Tabelas/historico_negocio_situacao.csv", colClasses = c(historico_negocio_situacao_situacao_id = "character")) %>%
  select(historico_negocio_situacao_data, historico_negocio_situacao_negocio_id, historico_negocio_situacao_situacao_id)

############################################################################################

############################################################################################
##Funções geradoras (gráficos)

func_fat_ano <- function(ano) {
  
  ##join de negocios e vendedores
  ano <- as.integer(ano)
  ano
  negocio_ij_vendedor <- inner_join(negocio, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))
  
  negocio_ij_historico_ij_vendedor_total <- inner_join(negocio_ij_vendedor, historico_negocio_situacao, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))
  #aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
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
  
  ##Criando anos anteriores
  ano_1ant = ymd(ano_atual) - years(1)
  ano_2ant = ymd(ano_atual) - years(2)
  ##junções anteriores
  ##filtrando apenas ano (parametro)
  if (ano == year(ano_atual)){
    historico_negocio_situacao_ano <- historico_negocio_situacao %>%
      filter(historico_negocio_situacao_data >= ano_atual & historico_negocio_situacao_situacao_id == 4)
  } else{
    if(ano == year(ano_1ant)){
      historico_negocio_situacao_ano <- historico_negocio_situacao %>%
        filter (historico_negocio_situacao_data < ano_atual & historico_negocio_situacao_data >= ano_1ant & historico_negocio_situacao_situacao_id == 4)
    }else{
      if(ano == year(ano_2ant)){
        historico_negocio_situacao_ano <- historico_negocio_situacao %>%
          filter (historico_negocio_situacao_data < ano_1ant & historico_negocio_situacao_data >= ano_2ant & historico_negocio_situacao_situacao_id == 4)
      }
    }
  }
  negocio_ij_historico_ij_vendedor_ano <- inner_join(negocio_ij_vendedor, historico_negocio_situacao_ano, by=c("negocio_id" = "historico_negocio_situacao_negocio_id"))
  ##aqui estou ordenando por historico_negocio_situacao_situacao_id pra depois remover as atualizações mais antigas, ficar só com a última atualização no negócio
  negocio_ij_historico_ij_vendedor_ano <- negocio_ij_historico_ij_vendedor_ano[order(-negocio_ij_historico_ij_vendedor_ano$historico_negocio_situacao_situacao_id, negocio_ij_historico_ij_vendedor_ano$negocio_id),]
  ng_ij_hist_ij_ven_ano <- negocio_ij_historico_ij_vendedor_ano[!duplicated(negocio_ij_historico_ij_vendedor_ano$negocio_id),]
  
  # Teste
  # historico_negocio_situacao_ano[historico_negocio_situacao_ano$historico_negocio_situacao_negocio_id == "120170316092933"]
  
  ng_ij_hist_ij_ven_ij_np_ano <- inner_join(ng_ij_hist_ij_ven_ano, negocio_produto, by=c("negocio_id" = "np_negocio_id"))
  
  ##Filtrando empresa, vendedores ativos e negocios de ano
  ng_ij_hist_ij_ven_ano_fat <- ng_ij_hist_ij_ven_ij_np_ano %>%
    filter(negocio_negocio_situacao_id == 4)
  
  fat_ano_mes <- ng_ij_hist_ij_ven_ano_fat %>%
    mutate (ym = format(historico_negocio_situacao_data, '%m')) %>%
    group_by (ym) %>%
    summarize(ym_sum = sum(np_valor), .groups = 'drop')
  ###Fazer o gráfico de linhas com faturamento anual (substituindo com NA linhas faltantes)
  ###Na super temos 2ant, na komatsu tem q verificar se possui 2ant e 1ant
  #######################################################################
  n_linhas <- nrow(fat_ano_mes)
  ym <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  fat_ano_mes_aux <- data.frame(ym)
  fat_ano_mes_aux$ym_sum <- NA
  fat_ano_mes_aux$ym_sum[1:n_linhas] <- fat_ano_mes$ym_sum
  
  meses = c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
  ## alterando pra números pra poder fazer da mesma forma
  fat_ano_mes$ym <- as.integer(fat_ano_mes$ym)
  ##Vou usar dessa forma, ao invés de transformar, irei apenas criar uma nova coluna (poderia ser feito com um join também)
  fat_ano_mes <- fat_ano_mes %>%
    mutate(mes = cut(ym,breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
                     labels = meses))
  
  return (fat_ano_mes)
  #######################################################################
  
}



fat_ano_mes <- func_fat_ano("2020")
n12 <- plot_ly(fat_ano_mes)
n12 <- n12 %>%
  add_trace(type = 'scatter', mode = 'lines+markers', x = ~mes, y =~ym_sum,
            name = 'Faturamento de 2020',
            text = ~paste(func_fmt_din_mi(ym_sum),'milhões'),
            hoverinfo = "text",
            color = I("green")
  )

if (dash == F){
  n12
}
n15

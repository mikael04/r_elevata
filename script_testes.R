rm(list = ls())
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
#Lib para funções de tempo
library(lubridate)
#usada para converter números em moeda
#library(scales)
#Lib usada para emojis/fonts/box de valores
library(ggplot2)
#Lib usada para os quadros
library(emojifont)
#lib para plotar a imagem (s_dados)
library(knitr)


###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = T
####Variável usada para não plotar os gráficos na dash
dash = F
####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
ano_atual = ymd(today()) - months(month(today())-1) - days(day(today())-1)
####Variavel global c/ mês atual (para comparação)

mes_atual = month(today())
####Variável global para ver se tem usados Ainda não usada
#usados = T
##plotando texto sem informações #usado para gráficos que não tiverem nenhuma informação no período
#caminho para imagem de sem dados
s_dados_path <- "s_dados.png"

##Variável "Global"
empresa = 16
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

###Começando scripts negocio_scripts
###########################################################################################################

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id, negocio_probabilidade, negocio_cliente_id)


##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
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
negocio_produto <- fread("Tabelas/negocio_produto.csv", colClasses = c(np_id = "character", np_negocio_id = "character", np_produto_id = "character")) %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  mutate (np_valor_tot = np_valor*np_quantidade)


##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character", produto_marca_id = "character", produto_categoria_id = "character")) %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

##pegar histórico e últimas atualizações
historico_negocio_situacao <- fread("Tabelas/historico_negocio_situacao.csv", colClasses = c(historico_negocio_situacao_situacao_id = "character")) %>%
  select(historico_negocio_situacao_data, historico_negocio_situacao_negocio_id, historico_negocio_situacao_situacao_id)

##coleta todas as propostas
proposta <- fread("Tabelas/proposta.csv", colClasses = c(proposta_id = "character", proposta_negocio_id = "character")) %>%
  select(proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status)

##coleta clientes da empresa
cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_ultima_visita, cliente_nome) %>%
  filter(cliente_empresa_id == empresa)

###############################################################################################################################

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
ng_ij_ven_funil_ab <- neg_ij_ven_ij_np %>%
  filter(negocio_negocio_situacao_id %in% st_a)

status_f = c(NA,0,1,4)
ng_ij_ven_ij_pr_funil_ab <- left_join(ng_ij_ven_funil_ab, proposta, by=c("negocio_id" = "proposta_negocio_id")) %>%
  #filter(proposta_status %in% status_f) %>%
  arrange(negocio_id)

status = c("Em negociação", "Montagem de cadastro", "Aguardando aprovação", "Financiamento aprovado", "Intenção ou prospecção")
#Criando nova coluna para não perder o id status (ao mudar pra string)
ng_ij_ven_funil_ab <- ng_ij_ven_funil_ab %>%
  rename(negocio_status = negocio_negocio_situacao_id)

ng_ij_ven_ij_pr_funil_ab <- ng_ij_ven_ij_pr_funil_ab %>%
  rename(negocio_status = negocio_negocio_situacao_id)

##Usar pra renomear os status das propostas
status = c("0 - Pendente", "1 - Aceito", "2 - Recusado", "3 - Cancelado", "4 - Finalizado")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
ng_ij_ven_ij_pr_funil_ab$proposta_status <- with(ng_ij_ven_ij_pr_funil_ab, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                       labels = status))

mont_cad_s_prop <- ng_ij_ven_ij_pr_funil_ab %>%
  select (negocio_id, negocio_status, negocio_vendedor_id, vendedor_nome, vendedor_ativo, negocio_data_cadastro, np_valor_tot, proposta_id, proposta_data_cadastro, proposta_status) %>%
  filter(negocio_status == 2, is.na(proposta_id))

data.table::fwrite(mont_cad_s_prop, file="Testes/montagem_cadastro_sem_propostas.csv", sep = ";")

mont_cad_c_prop <- ng_ij_ven_ij_pr_funil_ab %>%
  select (negocio_id, negocio_status, negocio_vendedor_id, vendedor_nome, vendedor_ativo, negocio_data_cadastro, np_valor_tot, proposta_id, proposta_data_cadastro, proposta_status) %>%
  filter(negocio_status == 2, !is.na(proposta_id))

data.table::fwrite(mont_cad_c_prop, file="Testes/montagem_cadastro_com_propostas.csv", sep = ";")

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_ven_ij_pr_funil_inten$negocio_status <- with(ng_ij_ven_ij_pr_funil_inten, cut(negocio_status, breaks = c(0,1,2,3,8,10),
                                                                                    labels = status))
negocio_aberto_proposta_finalizada <- ng_ij_ven_ij_pr_funil_inten %>%
  filter(proposta_status == 4)

negocio_aberto_proposta_aceita <- ng_ij_ven_ij_pr_funil_inten %>%
  filter(proposta_status == 1)

negocio_aberto_proposta_pendente <- ng_ij_ven_ij_pr_funil_inten %>%
  filter(proposta_status == 0)


##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_ven_ij_pr_funil_inten <- ng_ij_ven_ij_pr_funil_inten %>%
  select (negocio_status, np_valor_tot, np_quantidade, np_valor) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor_tot)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  select (-np_valor_tot, -np_quantidade) %>%
  ungroup()

##Agora será agrupado por pedidos em aberto e faturamento total
ng_ij_ven_funil_fat <- ng_ij_ven_funil_ab %>%
  select (negocio_status, np_valor_tot, np_quantidade, np_valor) %>%
  group_by(negocio_status) %>%
  mutate(total_faturado = sum(np_valor_tot)) %>%
  arrange(negocio_status) %>%
  distinct(negocio_status, .keep_all = TRUE) %>%
  select (-np_valor_tot, -np_quantidade) %>%
  collect()

##aqui ele substitui linha a linha cada situação pelo seu respectivo em string
ng_ij_ven_funil_fat$negocio_status <- with(ng_ij_ven_funil_fat, cut(negocio_status, breaks = c(0,1,2,3,8,10),
                                                                                    labels = status))

if (teste == T){
  ###Testando e imprimindo tabela para envio
  #negocio_print <- ng_ij_ven_funil_ab %>%
  #  select (negocio_id, negocio_vendedor_id, vendedor_nome, negocio_status, negocio_data_cadastro, negocio_usado, negocio_produto_id, np_quantidade, np_valor)
  
  #write_excel_csv(negocio_print, "negocios_vendedores_negocio_produto.csv", delim = ";")
  
}

##conversão de faturamento para texto
ng_ij_ven_funil_fat <- ng_ij_ven_funil_fat %>%
  mutate(tot_fat_t = func_fmt_din(total_faturado))

##Começando a gambiarra (criar nova columa com nome da categoria + valor da categoria)
ng_ij_ven_funil_fat <- ng_ij_ven_funil_fat %>%
  mutate(nome_cat_valor_fat = paste(negocio_status, "\n", tot_fat_t))

##Vou converter o id de situação pra 0, pra poder ordenar (intenção seria o status 0, é o primeiro)
ng_ij_ven_funil_fat$negocio_negocio_situacao_id[ng_ij_ven_funil_fat$negocio_negocio_situacao_id == 10] <- 0
ng_ij_ven_funil_fat <- ng_ij_ven_funil_fat %>%
  arrange(negocio_negocio_situacao_id)


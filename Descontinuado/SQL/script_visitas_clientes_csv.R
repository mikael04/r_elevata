rm(list = ls())

#rm(list = ls())
#Lib q será futuramente usada pros painéis interativos
#library(shiny)
#Lib pra conexão com o banco
library(odbc)
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
#Lib para mapas
library(leaflet)

###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = F
####Variável usada para não plotar os gráficos na dash
dash = F
####Variavel global c/ ano atual (para comparação)
ano_atual = '2020-01-01'
####Variável global para ver se tem usados Ainda não usada
#usados = T


##Emprsas"
emp_am = 42 # Amazonia
emp_ar = 77 # Araguaia
emp_ko = 78 # Komatsu
emp_ms = 35 # Ms
emp_si = 59 # Simex
emp_su = 16 # Super
emp_ta = 60 # Taisa

empresa <- emp_ms
###################################

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

##Começando script visitas_clientes

##Collect cria o df resultado da query, nesse caso, visitas_cliente, já filtrando apenas ano atual
visita_cliente <- fread("Tabelas/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_status_id, vc_resultado_id, vc_data_cadastro) %>%
  filter (vc_data_cadastro >= ano_atual)

##Pra pegar o nome do resultado
visita_resultado <- fread("Tabelas/visita_resultado.csv") %>%
  select(vr_id, vr_nome, vr_ativo) %>%
  filter(vr_ativo == 1) %>% ##Ja sao todos ativos
  select(-vr_ativo) %>%
  rename (resultado = vr_nome)

##Pra filtrar os resultados da empresa
visita_resultado_empresa <- fread("Tabelas/visita_resultado_empresa.csv") %>%
  select(vre_resultado_id, vre_empresa_id, vre_ativo) %>%
  filter(vre_ativo == 1, vre_empresa_id == empresa) %>% ##Ja sao todos ativos mas mantive pra manter o filtro, ja filtro a empresa
  select(-vre_ativo)
#Arrumando encoding
Encoding(visita_resultado$resultado) <- 'latin1'

##junta as duas tabelas (resultado e resultado_empresa) pra pegar o id da empresa (vre_empresa_id) e o nome do resultado (vr_nome)
vis_res_emp <- inner_join(visita_resultado, visita_resultado_empresa, by = c('vr_id'= 'vre_resultado_id'))

##Pra pegar o nome do status
visita_status <- fread("Tabelas/visita_status.csv") %>%
  select(vs_id, vs_nome, vs_ativo) %>%
  filter(vs_ativo == 1) %>%
  select(-vs_ativo) %>%
  rename (motivo = vs_nome)

##Pra filtrar os status da empresa
visita_status_empresa <- fread("Tabelas/visita_status_empresa.csv") %>%
  select(vse_status_id, vse_empresa_id, vse_ativo) %>%
  filter(vse_ativo == 1, vse_empresa_id == empresa) %>%
  select(-vse_ativo)

#Arrumando encoding
Encoding(visita_status$motivo) <- 'latin1'

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


##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id'))

##Juntando visita_cliente com os resultados
vc_ij_vre <- inner_join(visita_cliente, vis_res_emp, by = c('vc_resultado_id' = 'vr_id')) %>%
  select (vc_id, vc_vendedor_id, vc_resultado_id, resultado)
##Juntando visita_cliente com os motivos (status)
vc_ij_vse <- inner_join(visita_cliente, vis_st_emp, by = c('vc_status_id' = 'vs_id'))%>%
  select (vc_id, vc_vendedor_id, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vse_ij_v <- inner_join(vc_ij_vse, vendedor, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  select (vc_id, vc_vendedor_id, vendedor_nome, vc_status_id, motivo)

##juntando com vendedor pra separar por vendedor
vc_ij_vre_ij_v <- inner_join(vc_ij_vre, vendedor, by = c('vc_vendedor_id' = 'vendedor_id')) %>%
  select (vc_id, vc_vendedor_id, vendedor_nome, vc_resultado_id, resultado)

##Agrupando por vendedor e por motivo (vc_status_id, depois mostrar so motivo)
vc_ij_vse_ij_v <- vc_ij_vse_ij_v %>%
  group_by(vc_vendedor_id, vc_status_id) %>%
  mutate(motivo_n = n()) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup()

### Gráfico v0 - Motivo das visitas (status) por vendedor em 2020

##descobrindo numero de status diferentes para criar paleta de cores
n_m_color <- n_distinct(vc_ij_vse_ij_v$vc_status_id)
if (n_m_color <3){
  brbg_mot <- '#9ACD32'
} else{
  if(n_m_color > 11){
    brbg_mot <- colorRampPalette(brewer.pal(name="Dark2", n = 8)) (n_m_color)
  }else{
    brbg_mot <- brewer.pal(n_m_color,'BrBG')
  }
}

axis_h <- list(
  title = "")
v0 <- plot_ly(vc_ij_vse_ij_v, type = "bar", orientation = 'h', x = ~motivo_n, y = ~reorder(vendedor_nome, desc(vendedor_nome)), color = ~motivo,
              colors = brbg_mot,
              name = ~motivo)
v0 <- v0 %>%
  layout(barmode = 'stack',
         xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
         yaxis = list(title = ''))
if(dash == F){
  v0
}


##Agrupando por vendedor e por resultado (vc_status_id, depois mostrar so motivo)
vc_ij_vre_ij_v_ag <- vc_ij_vre_ij_v %>%
  group_by(vc_vendedor_id, vc_resultado_id) %>%
  mutate(resultado_n = n()) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup()

### Gráfico v1 - Resultado das visitas (resultados) por vendedor em 2020
##descobrindo numero de status diferentes para criar paleta de cores
n_r_color <- n_distinct(vc_ij_vre_ij_v_ag$vc_resultado_id)
if (n_r_color <3){
  brbg_res <- '#9ACD32'
} else{
  if(n_r_color > 11){
    brbg_res <- c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="BrBG", n = 8))
  }else{
    brbg_res <- brewer.pal(n_m_color,'BrBG')
  }
}

v1 <- plot_ly(vc_ij_vre_ij_v_ag, type = "bar", orientation = 'h', x = ~resultado_n, y = ~reorder(vendedor_nome, desc(vendedor_nome)), color = ~resultado,
              colors = brbg_res,
              name = ~resultado)

v1 <- v1 %>%
  layout(barmode = 'stack',
         xaxis = list(title = '', tickangle = 30, tickfont = list(size = 11)),
         yaxis = list(title = ''))
if(dash == F){
  v1
}

if(teste == F){
  #tabelas
  rm(visita_resultado_empresa, vis_res_emp, vc_ij_vre, vc_ij_vse,
     ##vis_st_emp, vc_ij_vse_ij_v, ##vou usar a vis_st_emp para uma junção (saber qual empresa são feitas as visitas)
     vc_ij_vre_ij_v, vc_ij_vre_ij_v_ag, visita_resultado,axis_h);
  #variáveis
  rm(brbg_mot, brbg_res, n_r_color, n_m_color);
}
################################################################################################
################################################################################################
### Tabelas e graficos de Clientes

cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_vendedor_id, cliente_empresa_id, cliente_data_cadastro, cliente_ultima_visita) %>%
  filter(cliente_empresa_id == empresa)

##Clientes cadastrados por vendedor
cli_p_v <- cliente %>%
  group_by(cliente_vendedor_id) %>%
  mutate(n_clientes = n()) %>%
  distinct(cliente_vendedor_id, .keep_all = T) %>%
  select(cliente_vendedor_id, n_clientes) %>%
  arrange(cliente_vendedor_id) %>%
  ungroup()

##Juncao pra pegar nome do vendedor
cli_p_v_ij_vend <- inner_join(cli_p_v, vendedor, by = c('cliente_vendedor_id' = 'vendedor_id')) %>%
  select(cliente_vendedor_id, vendedor_nome, n_clientes)


##Já é filtrado em 2020 (filtra na tabela de visitas_cliente)
vc_ij_vse_ij_v_count <- vc_ij_vse_ij_v %>%
  select (vc_vendedor_id, motivo_n) %>% ##Não vou salvar o nome pq vou fazer uma junção com a outra tabela, então só preciso do id
  group_by(vc_vendedor_id) %>%
  mutate(n_visitas = sum(motivo_n)) %>%
  distinct(vc_vendedor_id, .keep_all = T) %>%
  ungroup ()

##Distribuição de clientes total, visita e negócios em 2020 por vendedor
vend_cli_vis <- left_join(cli_p_v_ij_vend, vc_ij_vse_ij_v_count, by = c("cliente_vendedor_id" = "vc_vendedor_id")) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_visitas)

##Contar quantos negócios são feitos por vendedor em 2020
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character", negocio_cliente_id = "character")) %>%
  select(negocio_id, negocio_vendedor_id, negocio_cliente_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_produto_id)

##Juncao pra pegar a empresa do negócio (e vendedor)
neg_ij_vend <- inner_join(negocio, vendedor, by = c('negocio_vendedor_id' = 'vendedor_id')) %>%
  select(negocio_id, negocio_vendedor_id)

neg_ij_vend_count <- neg_ij_vend %>%
  select(negocio_vendedor_id) %>%
  group_by(negocio_vendedor_id) %>%
  mutate(n_negocios = n()) %>%
  distinct(negocio_vendedor_id, .keep_all = T) %>%
  ungroup()

vend_cli_vis_neg <- left_join(vend_cli_vis, neg_ij_vend_count, by = c("cliente_vendedor_id" = "negocio_vendedor_id")) %>%
  select (cliente_vendedor_id, vendedor_nome, n_clientes, n_visitas, n_negocios)

### Grafico c0 - Distribuicao de clientes (total), visitas (2020) e negocios (2020) cadastrados por vendedor
c0 <- plot_ly(vend_cli_vis_neg, x = ~vendedor_nome, y= ~n_clientes, type = 'bar',
              name = 'Clientes (total)')
c0 <- c0 %>%
  add_trace(y= ~n_visitas, name = 'Visitas (2020)')
c0 <- c0 %>%
  add_trace(y= ~n_negocios, name = 'Negócios (2020)')
c0 <- c0 %>%
 layout(barmode = 'grouped',
        xaxis = list(title = '', tickangle = 30, tickfont = list(size = 12)),
        yaxis = list(title = ''))

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
##Contando número de clientes geral e em 2020
n_clientes <- nrow(cliente)
##Clientes cadastrados em 2020 por vendedor
cliente_2020 <- cliente %>%
  filter(cliente_data_cadastro >= ano_atual)
n_clientes_2020 <- nrow(cliente_2020)

##Contar quantos clientes aparecem em negócio
cli_ij_ng <- inner_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  select(cliente_id, negocio_id)
cli_2020_ij_ng <- inner_join(cliente_2020, negocio, by=c("cliente_id" = "negocio_cliente_id"))%>%
  select(cliente_id, negocio_id)
##Variáveis usadas
n_cli_cneg <- nrow(cli_ij_ng)
n_cli_cneg_2020 <- nrow(cli_2020_ij_ng)

Clientes <- c("Clientes com negócios", "Clientes sem negócio")

n_total <- c(n_cli_cneg, n_clientes-nrow(cli_ij_ng))
n_total_p <- c(round((n_cli_cneg/n_clientes), 2), round(((n_clientes-n_cli_cneg)/n_clientes), 2))

n_2020 <- c(n_cli_cneg_2020, n_clientes_2020-n_cli_cneg_2020)
n_2020_p <- c(round((n_cli_cneg_2020/n_clientes_2020), 2), round(((n_clientes_2020-n_cli_cneg_2020)/n_clientes_2020), 2))

cli_c_s_ng <- data.frame(Clientes, n_total, n_total_p, n_2020, n_2020_p)


### Grafico c1 - Clientes cadastrados que possuem negocio (pizza)
colors_pie<- c("#32CD32", "#FFA500")
c1 <- plot_ly(cli_c_s_ng, labels = ~Clientes, values = ~n_total, type = 'pie', sort = F,
              text = func_fmt_numbr(n_total),
              texttemplate = "%{text} (%{percent})",
              hovertemplate = paste ("%{label} <br>",
                                     "Equivalente a %{percent}",
                                     "<extra></extra>"),
              marker = list(colors = colors_pie))

if(dash == F){
  c1
}

### Grafico c2 - Clientes cadastrados em 2020 que possuem negocio (pizza)
c2 <- plot_ly(cli_c_s_ng, labels = ~Clientes, values = ~n_2020, type = 'pie', sort = F,
              text = func_fmt_numbr(n_2020),
              texttemplate = "%{text} (%{percent})",
              hovertemplate = paste ("%{label} <br>",
                                     "Equivalente a %{percent}",
                                     "<extra></extra>"),
              marker = list(colors = colors_pie))

if(dash == F){
  c2
}
if(teste == F){
  #tabelas
  rm(cli_2020_ij_ng, cli_c_s_ng, Clientes, n_total, n_total_p, n_2020, n_2020_p, cliente_2020);
  #variáveis
  rm(n_cli_cneg, n_cli_cneg_2020, n_clientes, n_clientes_2020, colors_pie);
}
####################################


###Contar clientes/visitas/negocios cadastrados por mês -> vem do script visita_clientes
####################################

##declarado cliente, já usado

##declarado visita_cliente , já usado

##declarado visita_status, já usado

##declarado visita_status_empresa, já usado


##junta as duas tabelas (status e status_empresa) pra pegar o id da empresa (vs_empresa_id) e o nome do status (vs_nome)
vis_st_emp <- inner_join(visita_status, visita_status_empresa, by = c('vs_id'= 'vse_status_id'))

##Já filtrado apenas empresa (variavel global)
clientes_mes <- cliente %>%
  filter (cliente_data_cadastro >= ano_atual) %>%
  mutate (ym = format(cliente_data_cadastro, '%m')) %>%
  group_by (ym) %>%
  summarize(n_cli = n(), .groups = 'drop') %>%
  ungroup ()

##Visita precisa juntar com visita_status ou _resultado () pra obter empresa_id ##poderia ser vendedor, mas a tabela vis_st_emp (status c/ status_empresa) já está pronta
##vis_st_emp já vem filtrada pela empresa (var global)
vc_ij_emp <- inner_join(visita_cliente, vis_st_emp, by = c("vc_status_id" = "vs_id")) %>%
  #select(vc_id, vc_status_id, vc_data_cadastro) ##Sem empresa_id
  select(vc_id, vc_status_id, vc_data_cadastro, vse_empresa_id)

visitas_mes <- vc_ij_emp %>%
  filter (vc_data_cadastro >= ano_atual) %>%
  mutate (ym = format(vc_data_cadastro, '%m')) %>%
  group_by (ym) %>%
  summarize(n_vis = n(), .groups = 'drop') %>%
  ungroup ()

##Negocio precisa juntar com vendedor pra obter empresa_id ##Poderia ser cliente também, mas a tabela vendedor é menor
##vendedor já vem filtrado pela empresa (var global)
ng_ij_emp <- inner_join(negocio, vendedor, by = c("negocio_vendedor_id" = "vendedor_id")) %>%
  #select(negocio_id, negocio_vendedor_id, negocio_data_cadastro)%>% ##Sem empresa_id
  select(negocio_id, negocio_vendedor_id, negocio_data_cadastro, vendedor_empresa_id)

negocios_mes <- ng_ij_emp %>%
  filter (negocio_data_cadastro >= ano_atual) %>%
  mutate (ym = format(negocio_data_cadastro, '%m')) %>%
  group_by (ym) %>%
  summarize(n_neg = n(), .groups = 'drop') %>%
  ungroup ()

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
###Não farei dessa forma pois quero manter a coluna
#cli_ij_vc_ij_ng_mes$ym <- with(cli_ij_vc_ij_ng_mes, cut(ym, breaks = c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12),
#                                                        labels = meses))

### Gráfico c3 - Cadastro de clientes, visitas e negócios no ano de 2020
c3 <- plot_ly(cli_ij_vc_ij_ng_mes, type = 'scatter', mode = 'lines+markers', x = ~mes, y = ~n_cli,
              name = 'Clientes',
              text = ~paste(func_fmt_numbr(n_cli), 'clientes'),
              hoverinfo = "text",
              color = I('#7B68EE'))
c3 <- c3 %>%
  add_trace (type = 'scatter', mode = 'lines+markers', y = ~n_vis,
             name = 'Visitas',
             text = ~paste(func_fmt_numbr(n_vis), 'visitas'),
             hoverinfo = "text",
             color = I('#DAA520'))
c3 <- c3 %>%
  add_trace (type = 'scatter', mode = 'lines+markers', y = ~n_neg,
             name = 'Negócios',
             text = ~paste(func_fmt_numbr(n_neg), 'negócios'),
             hoverinfo = "text",
             color = I('green'))

c3 <- c3 %>%
  layout(xaxis = list(title = '', range = c(min(0), max(12))), ##Dessa forma pego os 12 meses do ano
         yaxis = list(title = ''))
if(dash == F){
  c3
}

if(teste == F){
  #tabelas
  rm(vendedor, vis_st_emp, cli_ij_vc_mes, cli_ij_vc_ij_ng_mes,
     clientes_mes, negocios_mes, ng_ij_emp, visitas_mes, vc_ij_emp, visita_status, visita_status_empresa);
  #variáveis
  rm(meses);
}
####################################
###Clientes que tiveram negócios, quantas visitas recebem (histograma) e clientes que não tiveram negócios, quantas visitas recebem
####################################

#cli_ij_ng <- já tenho, clientes que já possuem negócios
##Clientes sem negócio
cli_s_neg <- anti_join(cliente, negocio, by=c("cliente_id" = "negocio_cliente_id")) %>%
  select(cliente_id) %>%
  mutate (neg = F)
##Clientes com negócio
cli_c_neg <- cli_ij_ng %>%
  select (cliente_id, negocio_id) %>%
  ungroup() %>%
  mutate (neg = T)

cli_s_neg_ij_vis <- inner_join(cli_s_neg, visita_cliente, by = c("cliente_id" = "vc_cliente_id"))
cli_c_neg_ij_vis <- inner_join(cli_c_neg, visita_cliente, by = c("cliente_id" = "vc_cliente_id"))

##Contanto visitas por cliente, dos que não tem negócios
cli_s_neg_ij_vis_cont <- cli_s_neg_ij_vis %>%
  group_by(cliente_id) %>%
  count(name = "num_visitas") %>%
  ungroup()

##Contanto visitas por cliente, dos que tem negócios
cli_c_neg_ij_vis_cont <- cli_c_neg_ij_vis %>%
  group_by(cliente_id, negocio_id) %>%
  count(name = "num_visitas") %>%
  ungroup()


### Gráfico c4 - Histograma de distirbuição clientes por visitas, clientes com negócios

c4 <- plot_ly() %>%
  add_histogram(data =  cli_c_neg_ij_vis_cont, x = ~num_visitas, name = "Intervalo de visitas, clientes COM negócios", nbinsx = max(cli_c_neg_ij_vis_cont$num_visitas),
                marker = list(color = "blue"), opacity = 0.6) %>%
  layout (xaxis = list(title = 'Intervalos'),
          yaxis = list(title = 'Número de clientes'))

if(dash == F){
  c4
}

### Gráfico c5 - Histograma de distirbuição clientes por visitas, clientes sem negócios
c5 <- plot_ly() %>%
  
add_histogram(data =  cli_s_neg_ij_vis_cont, x = ~num_visitas, name = "Intervalo de visitas, clientes SEM negócios", nbinsx = max(cli_c_neg_ij_vis_cont$num_visitas),
              marker = list(color = "darkorange"), opacity = 0.6) %>%
  layout (xaxis = list(title = 'Intervalos'),
          yaxis = list(title = 'Número de clientes'))

if(dash == F){
  c5
}

### Gráfico c4_c5 - Ambos histogramas anteriores (c4 e c5)
c4_c5 <- subplot(c4, c5) %>%
  layout(legend = list(x=0.7, y=0.9),
         title = "Clientes por intervalo",
         xaxis = list(title = ''),
         yaxis = list(title = 'Distribuição de clientes'))

if(dash == F){
  c4_c5
}

if(teste == F){
  #tabelas
  rm(visita_cliente, cliente, negocio, cli_ij_ng, cli_c_neg_ij_vis_cont, cli_s_neg_ij_vis_cont,
     cli_c_neg_ij_vis, cli_s_neg_ij_vis, cli_c_neg, cli_s_neg);
  #variáveis
  rm();
}


#########################################################################################################
#########################################################################################################
##Começando scripts de mapas
#########################################################################################################
#########################################################################################################

##Distribuição de clientes, mostrando no label nome do cliente, nome do vendedor e última visita
cliente <- fread("Tabelas/cliente.csv", colClasses = c(cliente_id = "character")) %>%
  select (cliente_id, cliente_nome, cliente_latitude, cliente_longitude, cliente_vendedor_id, cliente_empresa_id)
#Arrumando encoding
Encoding(cliente$cliente_nome) <- 'latin1'

visita_cliente <- fread("Tabelas/visita_cliente.csv", colClasses = c(vc_id = "character", vc_cliente_id = "character")) %>%
  select (vc_id, vc_vendedor_id, vc_cliente_id, vc_data_cadastro)

vendedor <- fread("Tabelas/vendedor.csv") %>%
  select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  filter(vendedor_ativo == 1, vendedor_empresa_id == empresa) %>%
  select(-vendedor_ativo)

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

##Clientes da empresa correta ##Clientes com valor NA, valores 0 e valores positivos de latlong (hemisf norte, leste do globo) removidos
cliente_c_loc <- cliente %>%
  filter (cliente_empresa_id == empresa, cliente_latitude < 0, cliente_longitude < 0) %>%
  rename(lat = cliente_latitude, long = cliente_longitude) %>%
  mutate(lat = as.numeric(lat), long = as.numeric(long)) %>%
  filter (!is.null(lat), !is.na(lat))


##Serão feitas as junções apenas com clientes que possuem
##Aqui já vou filtrar só por vendedores ativos, fazer pra inativos depois
cli_in_ven <- inner_join(cliente_c_loc, vendedor, by = c('cliente_vendedor_id'='vendedor_id'))

##pegar apenas última visita por cliente
vcUltVis <- visita_cliente %>%
  arrange(desc(vc_data_cadastro)) %>%
  group_by(vc_cliente_id) %>%
  distinct(vc_cliente_id, .keep_all = T) %>%
  ungroup ()


##Categorias de idades
idades_vis = c("Sem visita", "Até 2 meses", "De 2 a 6 meses", "De 6 a 12 meses", "De 12 a 24 meses", "Mais de 24 meses")
##Cores das categorias
cor_idade_vis = c("#000000", "#32CD32", "#87CEFA" , "yellow" , "orange" , "#DE0D26")

##left join pra manter todos, mesmo os q não tem visita
cli_in_ven_in_vcUltVis <- left_join(cli_in_ven, vcUltVis, by = c("cliente_id" = "vc_cliente_id")) %>%
  select(cliente_id, cliente_nome, lat, long, cliente_vendedor_id, vendedor_nome, vc_vendedor_id, vc_data_cadastro) %>%
  mutate (idade = (as.integer(today() - as_date(vc_data_cadastro)))) %>%
  mutate (idade_vis = "Sem Visita") %>%
  arrange (idade)

cli_in_ven_in_vcUltVis$idade[is.na(cli_in_ven_in_vcUltVis$idade)] <- -1

cli_in_ven_in_vcUltVis$idade_vis <- with(cli_in_ven_in_vcUltVis, cut(idade, breaks = c(-2,0,60, 180, 365, 730, 10000),
                                                                     label = idades_vis))

pal = colorFactor(palette = cor_idade_vis, domain = cli_in_ven_in_vcUltVis$idade_vis)

### Gráfico m0 de distribuição dos clientes por empresa
m0 <- leaflet(cli_in_ven_in_vcUltVis) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~lat, lng = ~long, weight = 1,
                   popup = paste0( "Nome do cliente:", cli_in_ven_in_vcUltVis$cliente_nome,
                                   "<br>",
                                   "Vendedor: ", cli_in_ven_in_vcUltVis$vendedor_nome,
                                   "<br>",
                                   "Tempo desde a última visita: ", cli_in_ven_in_vcUltVis$vc_data_cadastro),
                   label = ~cliente_nome,
                   color = ~pal(idade_vis),
                   #clusterOptions = markerClusterOptions()
  ) %>%
  addLegend("topright",
            value = ~idade_vis,
            pal = pal,
            title = 'Tempo desde a última visita',
            opacity = .8)

if(teste == F){
  #tabelas
  rm(cliente, pal, cli_in_ven, cli_in_ven_in_vcUltVis, visita_cliente, vendedor, vcUltVis);
  #var
  rm(cor_idade_vis, idades_vis)
}
#######################################################################

########################################################

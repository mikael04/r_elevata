rm(list = ls())

#Lib pra conexão com o banco
library(odbc)
#Libs pra trabalhar com a base (cortes e funções similares ao SQL)
library(dplyr)
#library(dbplyr)
library(tidyverse)
#Lib pro gráfico
library(ggplot2)
#lib pros gráficos mais "interativos"
library(plotly)
library(highcharter)
library(htmlwidgets)
#Lib pra usar paletas de cores
#library(RColorBrewer)
#Lib usada pros treemaps
#library(treemap)
#Lib usada para porcentagem
library(scales)



####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = F

####Variável usada para não apagar coisas na dash
dash = F

####Variável global da empresa a ser trabalhada
empresa = 16

#formatar dinheiro
function_format_din <- function(inteiro)
{
  inteiro_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_em_reais)
}

function_format_din_mi <- function(inteiro)
{
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1))
  return(inteiro_mi_em_reais)
}

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = "localhost\\SQLEXPRESS01",
                      Database = "nhmobile_agriculture",
                      Trusted_Connection = "True")

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
##coleta todas as propostas
proposta <- tbl(con, "proposta") %>%
  select(proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status) %>%
  collect()

##coleta todos os negócios
negocio <- tbl(con, "negocio") %>%
  select(negocio_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_vendedor_id, negocio_usado) %>%
  collect()

##coleta todos os vendedores
vendedor <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_id, vendedor_empresa_id, vendedor_ativo) %>%
  collect()

##Aqui vou filtrar os ativos e já vou filtrar a empresa
vendedor <- vendedor %>%
  filter(vendedor_ativo == TRUE)

##junção de proposta com negócio
prop_ij_neg <- inner_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado)

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_ij_neg <- prop_ij_neg[!is.na(prop_ij_neg$negocio_vendedor_id),]

if (teste == F){
  rm(negocio)
}

prop_ij_neg_2020 <- prop_ij_neg %>%
  filter(proposta_data_cadastro >= '2020-01-01')

##Aqui tenho a contagem de status da empresa
prop_ij_neg_cont <- prop_ij_neg_2020 %>%
  select (proposta_status) %>%
  group_by(proposta_status) %>%
  mutate(cont_status = n()) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  collect()

status = c("0 - Pendente", "1 - Aceito", "2 - Recusado", "3 - Cancelado", "4 - Finalizado")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_cont$proposta_status <- with(prop_ij_neg_cont, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                               labels = status))

##Juntando com vendedor pra obter o nome do vendedor
prop_ij_neg_ij_vend <- inner_join(prop_ij_neg, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

##Filtrando a empresa
prop_ij_neg_ij_vend_2020 <- prop_ij_neg_ij_vend %>%
  filter(vendedor_empresa_id == 16, proposta_data_cadastro >= '2020-01-01')

#aqui eu estou alterando o joão paulo, que havia problemas com codificação
prop_ij_neg_ij_vend_2020$vendedor_nome[prop_ij_neg_ij_vend_2020$negocio_vendedor_id == 45] <- "JOÃO PAULO"

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)
prop_ij_neg_ij_vend_2020 <- prop_ij_neg_ij_vend_2020[!is.na(prop_ij_neg_ij_vend_2020$negocio_vendedor_id),]

##Aqui tenho a contagem de status por vendedor
prop_ij_neg_cont_vend <- prop_ij_neg_ij_vend_2020 %>%
  select (negocio_vendedor_id, negocio_vendedor_id, vendedor_nome, proposta_status) %>%
  group_by(proposta_status, negocio_vendedor_id) %>%
  mutate(cont_status = n()) %>%
  distinct(negocio_vendedor_id, proposta_status, .keep_all = TRUE) %>%
  collect()

##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_cont_vend$proposta_status <- with(prop_ij_neg_cont_vend, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                         labels = status))

##Gráfico 9 - Número propostas, por tipo, por vendedor (total)
p0 <- ggplot(prop_ij_neg_cont_vend, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), cont_status, fill=factor(proposta_status), label = cont_status,
                                        text = paste('Número de pedidos nesta categoria:', cont_status))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))+
  coord_flip(expand = F)

p0 <- ggplotly(p0, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.3, y = -0.15))

if(dash == F){
  p0
}

###Gráfico 10 - Número propostas, por tipo em 2020 (total)
p1 <- ggplot(prop_ij_neg_cont, aes(x = proposta_status, y = cont_status, fill=as.factor(proposta_status),
                                   text = paste('Número de pedidos nesta categoria:', cont_status))) +
  geom_col(position = "identity") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))

p1 <- ggplotly(p1, tooltip = 'text') %>%
  layout(showlegend = FALSE)
if(dash == F){
  p1
}
if(teste == F){
  #tabelas
  rm(vendedor, prop_ij_neg_2020, prop_ij_neg_cont, prop_ij_neg_cont_vend, prop_ij_neg_ij_vend_2020);
  #variáveis
  rm(status);
}

##############################################

##Propostas por categoria (usado)
##############################################
##Aqui tenho a contagem de usados em propostasda empresa
prop_ij_neg_cont_us <- prop_ij_neg %>%
  select (negocio_usado) %>%
  group_by(negocio_usado) %>%
  mutate(usado = n()) %>%
  distinct(negocio_usado, .keep_all = TRUE) %>%
  collect()

total = sum(prop_ij_neg_cont_us$usado)

prop_ij_neg_cont_us <- prop_ij_neg_cont_us %>%
  mutate(total = total)

prop_ij_neg_cont_us$negocio_usado[prop_ij_neg_cont_us$negocio_usado == TRUE] <- "Proposta com usado"
prop_ij_neg_cont_us$negocio_usado[prop_ij_neg_cont_us$negocio_usado == FALSE] <- "Proposta sem usado"

###Gráfico 11 - Proporção de usados (pizza)
p2 <- plot_ly(prop_ij_neg_cont_us, labels = ~negocio_usado, values = ~usado, type = 'pie', sort = F,
              texttemplate = "%{value} (%{percent})",
              hovertemplate = paste ("%{label} <br>",
                                     "%{value} <br>",
                                     "Equivalente a %{percent} do total",
                                     "<extra></extra>"))

if(dash == F){
  p2
}

if(teste == F){
  #tabelas
  rm(prop_ij_neg_cont_us, prop_ij_neg);
  #variáveis
  rm(total);
}

##############################################
### Ticket médio por proposta

### Uma proposta tem n proposta_pagamento (cuidar as ativas, pp_ativo = 1)
##coleta todos proposta_pagamenmto
proposta_pagamento <- tbl(con, "proposta_pagamento") %>%
  select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
  collect()

proposta_pagamento <- proposta_pagamento %>%
  filter(pp_ativo == TRUE)

proposta_produto <- tbl(con, "proposta_produto") %>%
  select(pp_id, pp_proposta_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo) %>%
  collect()

proposta_produto <- proposta_produto %>%
  filter(pp_ativo == TRUE)

produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()

pprod_ij_prod <- inner_join(proposta_produto, produto, by = c("pp_produto_id" = "produto_id"))

##Junção pra termos valor da proposta (preciso do negócio pra filtrar empresa e vendedores se preciso)
p_ij_n_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_pagamento, by = c("proposta_id" = "pp_proposta_id")) %>%
  arrange(pp_valor)

##filtro da empresa e também propostas finalizadas
p_ij_n_ij_pp_empresa <- p_ij_n_ij_pp %>%
  filter(vendedor_empresa_id == empresa, proposta_status == 4)

p_ij_n_ij_pp_empresa <- p_ij_n_ij_pp_empresa %>%
  group_by(proposta_id) %>%
  mutate (valor_proposta = sum(pp_valor)) %>%
  collect ()

##media geral da empresa
total_empresa <- sum(p_ij_n_ij_pp_empresa$pp_valor)
media_empresa <- round(total_empresa/nrow(p_ij_n_ij_pp_empresa), 2)

##Começando as junções pra chegar nas categorias
p_ij_pprod_ij_prod <- inner_join(proposta, pprod_ij_prod, by = c('proposta_id' = 'pp_proposta_id')) %>%
  select(proposta_id, proposta_status, pp_produto_id, pp_quantidade, pp_valor, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  filter (proposta_status == 4)

## Primeira vez pra verificar quais são os top10
pr_top10_fat <- p_ij_pprod_ij_prod %>%
  select(produto_categoria_id, pp_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(fat = sum(pp_valor)) %>%
  mutate(n = n()) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  collect ()


##Isso aqui tudo é pra pegar o top 10, substituir os que não estão por -Outros e depois refazer a média
top10_fat <- head.matrix(pr_top10_fat, n=10)

##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- tbl(con, "categoria") %>%
  select(categoria_id, categoria_nome) %>%
  collect()

top10_fat_ij_cat <- inner_join(top10_fat, categoria, by = c("produto_categoria_id"="categoria_id"))
top10_fat_ij_cat <- top10_fat_ij_cat[, -2:-4]
top10_fat_ij_cat <- as.data.frame(top10_fat_ij_cat)

##Aqui estou juntando para agrupar primeiro através do na
pr_top10_fat_aux <- left_join(pr_top10_fat, top10_fat_ij_cat, by=c("produto_categoria_id" = "produto_categoria_id"))
pr_top10_fat_aux$produto_categoria_id[is.na(pr_top10_fat_aux$categoria_nome)] <- "-1"


##Depois de setar as linhas q tem nome de coluna = na (não estão no top10) e ter setado todas pra id = -1, faço a soma dos n e faturamentos
n_out <- sum(pr_top10_fat_aux$n[which(pr_top10_fat_aux$produto_categoria_id=="-1")])
fat_out <- sum(pr_top10_fat_aux$fat[which(pr_top10_fat_aux$produto_categoria_id=="-1")])

##E então, repasso o valor de volta para a coluna "outros", de faturamento total (de todas as categorias menos as q estão no top10) e número de vezes que aparecem
pr_top10_fat_aux$n[pr_top10_fat_aux$produto_categoria_id=='-1'] <- n_out
pr_top10_fat_aux$fat[pr_top10_fat_aux$produto_categoria_id=='-1'] <- fat_out

##E aqui removo as demais linhas, que já foram adicionadas a "Outros"
pr_top10_fat_aux <- pr_top10_fat_aux[!is.na(pr_top10_fat_aux$categoria_nome),]

##Aqui só renomeio pra ver que a categoria outros tem um * representando a soma de todas as outras categorias também
pr_top10_fat_aux$categoria_nome[(pr_top10_fat_aux$categoria_nome == "OUTRA")] <- "OUTRA *"


## Aqui já estõu fazendo a média das categorias
pr_top10_fat_med <- pr_top10_fat_aux %>%
  select(categoria_nome, produto_categoria_id, fat, n) %>%
  group_by(produto_categoria_id) %>%
  mutate(fat_med = fat/n) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  collect ()


fat_tot_categorias <- pr_top10_fat_med

fat_tot_categorias <- fat_tot_categorias %>%
  mutate(med_emp = media_empresa) %>%
  collect()


## Gráfico 6 - Ticket médio por categoria e geral da empresa
ax <- list(
  autotick = TRUE,
  title = "",
  showticklabels = TRUE)
p3 <- plot_ly(fat_tot_categorias, type = "bar", x = ~categoria_nome, y = ~fat_med,
              name = 'Ticket médio por categoria',
              marker = list(color = 'lightblue'),
              text = ~paste(categoria_nome,'<br>' , function_format_din(fat_med)),
              hoverinfo = "text")

p3 <- p3 %>%
  layout(barmode = 'identity', xaxis = ax, yaxis = ax)

p3 <- p3 %>% add_trace(type = 'scatter', mode = 'markers+line', yaxis = 'y2',
                       name = 'Ticket médio da empresa',
                       x = ~categoria_nome,
                       y = ~med_emp,
                       line = list(color = 'red'),
                       text = ~paste('Ticket médio<br>' , function_format_din(med_emp)),
                       hoverinfo = "text",
                       marker = list(color = 'orange'))

p3 <- p3 %>%
  layout(
    yaxis = list(side = 'left', title = 'Faturamento', showgrid = TRUE, zeroline = FALSE),
    #range nos dois eixos iguais pra ficar na mesma proporção
    yaxis2 = list(overlaying = "y", showgrid = FALSE, zeroline = FALSE, showticklabels= F, range = c(0,400000)), yaxis = list(range = c(0,400000)),
    ##aqui eu ajusto onde quero que apareça a legenda
    legend = list(x=0.8, y=0.9)#)
  )
if(dash == F){
  p3
}


if(teste == F){
  #tabelas
  rm(ax, categoria, fat_tot_categorias, p_ij_n_ij_pp_empresa, p_ij_pprod_ij_prod, pprod_ij_prod, pr_top10_fat, pr_top10_fat_aux, pr_top10_fat_med, produto, proposta, prop_ij_neg_ij_vend, proposta_pagamento, proposta_produto, top10_fat, top10_fat_ij_cat)
  #variáveis
  rm(fat_out, media_empresa, n_out, total_empresa)
}

##############################################
### Faturamento de propostas em 2020 por status

##Aqui tenho a soma de faturamento da empresa dividido em categorias
p_ij_n_ij_pp_sum <- p_ij_n_ij_pp %>%
  select (proposta_status, pp_valor) %>%
  group_by(proposta_status) %>%
  mutate (valor_proposta = sum(pp_valor)) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  collect()

## Aqui agrupo as categorias
p_ij_n_ij_pp_sum_cat <- p_ij_n_ij_pp_sum %>%
  select (proposta_status, valor_proposta) %>%
  group_by(proposta_status) %>%
  mutate (valor_status = sum(valor_proposta)) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  arrange(proposta_status) %>%
  collect()

##Usar pra renomear os status das propostas
status = c("0 - Pendente", "1 - Aceito", "2 - Recusado", "3 - Cancelado", "4 - Finalizado")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
p_ij_n_ij_pp_sum_cat$proposta_status <- with(p_ij_n_ij_pp_sum_cat, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                       labels = status))

###Gráfico 7 - Faturamento de propostas por status
ax <- list(
  autotick = TRUE,
  title = "",
  showticklabels = TRUE)
p4 <- plot_ly(p_ij_n_ij_pp_sum_cat, x = ~proposta_status, y = ~valor_status, type = 'bar',
              name = 'Faturamento por status',
              marker = list(color = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32")),
              text = ~paste(proposta_status,'<br>' , function_format_din(valor_status)),
              hoverinfo = "text")

p4 <- p4 %>%
  layout(barmode = 'identity', xaxis = ax, yaxis = ax)

if(dash == F){
  p4
}
if(teste == F){
  #tabelas
  rm(ax, p_ij_n_ij_pp, p_ij_n_ij_pp_sum, p_ij_n_ij_pp_sum_cat);
  #variáveis
  rm(status);
}
##############################################

if (teste == 0) {
  #rm(list=ls())
}

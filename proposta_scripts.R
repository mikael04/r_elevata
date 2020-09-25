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

##Variável "Global"
#empresa = 16 #Super
empresa = 78 #Komatsu

#formatar dinheiro
func_fmt_din <- function(inteiro)
{
  inteiro_em_reais <- paste("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(inteiro_em_reais)
}
#formatar dinheiro (milhões)
func_fmt_din_mi <- function(inteiro)
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
  select(negocio_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_vendedor_id, negocio_usado, negocio_produto_id) %>%
  collect()

##coleta todos os vendedores
vendedor <- tbl(con, "vendedor") %>%
  select(vendedor_id, vendedor_nome, vendedor_id, vendedor_empresa_id, vendedor_ativo) %>%
  collect()

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
##Aqui vou filtrar os ativos e já vou filtrar a empresa
vendedor <- vendedor %>%
  filter(vendedor_ativo == TRUE)

##junção de proposta com negócio
prop_ij_neg <- inner_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado, negocio_produto_id)

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?)/Acho que era erro na junção (inner parece ter resolvido)
#prop_ij_neg <- prop_ij_neg[!is.na(prop_ij_neg$negocio_vendedor_id),]

if (teste == F){
  rm(negocio)
}

prop_ij_neg_2020 <- prop_ij_neg %>%
  filter(proposta_data_cadastro >= '2020-01-01')

##Aqui tenho a contagem de status da empresa
prop_ij_neg_cont_2020 <- prop_ij_neg_2020 %>%
  select (proposta_status) %>%
  group_by(proposta_status) %>%
  mutate(cont_status = n()) %>%
  distinct(proposta_status, .keep_all = TRUE) %>%
  collect()

status = c("0 - Pendente", "1 - Aceito", "2 - Recusado", "3 - Cancelado", "4 - Finalizado")
##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_cont_2020$proposta_status <- with(prop_ij_neg_cont_2020, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                         labels = status))

##Juntando com vendedor pra obter o nome do vendedor
prop_ij_neg_ij_vend <- inner_join(prop_ij_neg, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

##Filtrando a empresa
prop_ij_neg_ij_vend_2020 <- prop_ij_neg_ij_vend %>%
  filter(vendedor_empresa_id == empresa, proposta_data_cadastro >= '2020-01-01')

##removendo os campos onde um ID de proposta não corresponde a um ID de negócio (erro no banco?) /Acho que era erro na junção (inner parece ter resolvido)
#prop_ij_neg_ij_vend_2020 <- prop_ij_neg_ij_vend_2020[!is.na(prop_ij_neg_ij_vend_2020$negocio_vendedor_id),]

##Aqui tenho a contagem de status por vendedor
prop_ij_neg_cont_vend_2020 <- prop_ij_neg_ij_vend_2020 %>%
  select (negocio_vendedor_id, negocio_vendedor_id, vendedor_nome, proposta_status) %>%
  group_by(proposta_status, negocio_vendedor_id) %>%
  mutate(cont_status = n()) %>%
  distinct(negocio_vendedor_id, proposta_status, .keep_all = TRUE) %>%
  collect()

##Jeito mais eficiente de fazer (testar eficiência, mas logicamente mais eficiente já que quebra em intervalos e depois substitui, ao invés de rodar toda a matrix)
prop_ij_neg_cont_vend_2020$proposta_status <- with(prop_ij_neg_cont_vend_2020, cut(proposta_status, breaks = c(-1,0,1,2,3,4),
                                                                                   labels = status))

### Gráfico p0 - Número propostas, por tipo, por vendedor (total)
p0 <- ggplot(prop_ij_neg_cont_vend_2020, aes(x = reorder(vendedor_nome, desc(vendedor_nome)), cont_status, fill=factor(proposta_status), label = cont_status,
                                             text = paste('Número de pedidos nesta categoria:', cont_status))) + #usar o fill pra criar os léveis, ele já ordena por ordem alfabética
  geom_col(position = "stack") +
  theme (axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_blank()) +
  scale_fill_manual(values = c("#ADD8E6", "#00BFFF", "orange", "#DE0D26", "#32CD32"))+
  coord_flip(expand = F)

p0 <- ggplotly(p0, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = -0.3, y = -0.15))

if(dash == F){
  p0
}

### Gráfico p1 - Número propostas, por tipo em 2020 (total)
p1 <- ggplot(prop_ij_neg_cont_2020, aes(x = proposta_status, y = cont_status, fill=as.factor(proposta_status),
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
  rm(vendedor, prop_ij_neg_2020, prop_ij_neg_cont_2020, prop_ij_neg_cont_vend_2020, prop_ij_neg_ij_vend_2020);
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

### Gráfico p2 - Proporção de usados (pizza)
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
###Ticket médio por proposta (apenas novos)

negocio_produto <- tbl(con, "negocio_produto") %>%
  select(np_id, np_negocio_id, np_produto_id, np_quantidade,np_ativo, np_valor) %>%
  collect()

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- tbl(con, "produto") %>%
  select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id) %>%
  collect()

proposta_produto <- tbl(con, "proposta_produto") %>%
  select(pp_id, pp_proposta_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo) %>%
  filter (pp_ativo == 1) %>% #Filtrando pp_ativo = true
  collect()

p_ij_n_ij_v_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_produto, by = c("proposta_id" = "pp_proposta_id"))

##Vou puxar negócio novamente pra pegar a coluna de se é usado ou não (acho que é mais eficiente do que usar a tabela completa desde o início)
negocio <- tbl(con, "negocio") %>%
  select(negocio_id, negocio_tipo_negocio) %>%
  collect()

p_ij_n_ij_v_ij_pp_n <- inner_join(p_ij_n_ij_v_ij_pp, negocio, by = c("proposta_negocio_id" = "negocio_id"))

##filtro da empresa e também propostas finalizadas, também filtrando só negócios novos e com valores acima de 10
p_ij_n_ij_pp_empresa <- p_ij_n_ij_v_ij_pp_n %>%
  filter(vendedor_empresa_id == empresa, proposta_status == 4, negocio_tipo_negocio == 'N', pp_valor > 10)

p_ij_n_ij_pp_empresa <- p_ij_n_ij_pp_empresa %>%
  group_by(proposta_id) %>%
  mutate (valor_proposta = sum(pp_valor)) %>%
  collect ()

##media geral da empresa
total_empresa <- sum(p_ij_n_ij_pp_empresa$pp_valor)
n_empresa <- nrow(p_ij_n_ij_pp_empresa)
media_empresa <- round((total_empresa/n_empresa), 2)


##Calculando ticket médio por categoria

p_ij_n_ij_pp_ij_prod <- inner_join (p_ij_n_ij_pp_empresa, produto, by= c('pp_produto_id' = 'produto_id')) %>%
  select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo, produto_categoria_id, vendedor_empresa_id) %>%
  filter(proposta_status == 4, vendedor_empresa_id == empresa) #Proposta finalizada


## Primeira vez pra verificar quais são os top5
pr_top5_fat <- p_ij_n_ij_pp_ij_prod %>%
  select(produto_categoria_id, pp_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(fat = sum(pp_valor)) %>%
  mutate(n = n()) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  collect ()

##Isso aqui tudo é pra pegar o top 5 (antes top10, mas aparecia uma categoria que não queríamos), substituir os que não estão por -Outros e depois refazer a média
top5_fat <- head.matrix(pr_top5_fat, n=5)

##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- tbl(con, "categoria") %>%
  select(categoria_id, categoria_nome) %>%
  collect()

##Super apenas (para diminuir o tamanho do nome)
if (empresa == 16)
{
  categoria$categoria_nome[categoria$categoria_id == '120160830102326'] <- 'SEMEADORA'
}
#Arrumando encoding
Encoding(categoria$categoria_nome) <- 'latin1'

top5_fat_ij_cat <- inner_join(top5_fat, categoria, by = c("produto_categoria_id"="categoria_id"))
top5_fat_ij_cat <- top5_fat_ij_cat[, -1]
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
  collect ()


fat_tot_categorias <- pr_top5_fat_med

##Adicionando a matriz pra poder exibir no gráfico
fat_tot_categorias <- fat_tot_categorias %>%
  mutate(med_emp = media_empresa) %>%
  collect()

### Gráfico p3 - Ticket médio novos
ax <- list(
  autotick = TRUE,
  title = "",
  showticklabels = TRUE)
p3 <- plot_ly(fat_tot_categorias, type = "bar", x = ~categoria_nome, y = ~fat_med,
              name = 'Categorias',
              marker = list(color = 'lightblue'),
              text = ~paste(categoria_nome,'<br>' , func_fmt_din(fat_med)),
              hoverinfo = "text")

p3 <- p3 %>%
  layout(barmode = 'identity', xaxis = ax, yaxis = ax)

p3 <- p3 %>% add_trace(type = 'scatter', mode = 'markers+line', yaxis = 'y2',
                       name = 'Empresa (geral)',
                       x = ~categoria_nome,
                       y = ~med_emp,
                       line = list(color = 'red'),
                       text = ~paste('Ticket médio<br>' , func_fmt_din(med_emp)),
                       hoverinfo = "text",
                       marker = list(color = 'orange'))


if(empresa == 16){
  p3 <- p3 %>%
    layout(
      autosize = F,
      yaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE, title = '', range = c(0,700000)),
      #range nos dois eixos iguais pra ficar na mesma proporção
      yaxis2 = list(overlaying = "y", showgrid = FALSE, zeroline = FALSE, showticklabels= F, range = c(0,700000)),
      ##aqui eu ajusto onde quero que apareça a legenda
      legend = list(x=0.7, y=0.8)#)
    )
}else if(empresa == 78){
  p3 <- p3 %>%
    layout(
      autosize = F,
      yaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE, title = '', range = c(0,2000000)),
      #range nos dois eixos iguais pra ficar na mesma proporção
      yaxis2 = list(overlaying = "y", showgrid = FALSE, zeroline = FALSE, showticklabels= F, range = c(0,2000000)),
      ##aqui eu ajusto onde quero que apareça a legenda
      legend = list(x=0.7, y=0.8)#)
    )
}
if(dash == F){
  p3
}

###Ticket médio por proposta (apenas usados)

p_ij_n_ij_v_ij_pp_n <- inner_join(p_ij_n_ij_v_ij_pp, negocio, by = c("proposta_negocio_id" = "negocio_id"))

##filtro da empresa e também propostas finalizadas, também filtrando só negócios novos e com valores acima de 10
p_ij_n_ij_pp_empresa_us <- p_ij_n_ij_v_ij_pp_n %>%
  filter(vendedor_empresa_id == empresa, proposta_status == 4, negocio_tipo_negocio == 'U', pp_valor > 10)


p_ij_n_ij_pp_empresa_us <- p_ij_n_ij_pp_empresa_us %>%
  group_by(proposta_id) %>%
  mutate (valor_proposta = sum(pp_valor)) %>%
  collect ()

##media geral da empresa
total_empresa_us <- sum(p_ij_n_ij_pp_empresa_us$pp_valor)
n_empresa_us <- nrow(p_ij_n_ij_pp_empresa_us)
media_empresa_us <- round((total_empresa_us/n_empresa_us), 2)


##Calculando ticket médio por categoria

p_ij_n_ij_pp_ij_prod_us <- inner_join (p_ij_n_ij_pp_empresa_us, produto, by= c('pp_produto_id' = 'produto_id')) %>%
  select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo, produto_categoria_id, vendedor_empresa_id, negocio_tipo_negocio) %>%
  filter(proposta_status == 4, vendedor_empresa_id == empresa) #Proposta finalizada

##Teste
##Adicionar categoria pra ver se é isso mesmo
p_ij_n_ij_pp_ij_prod_us <- inner_join(p_ij_n_ij_pp_ij_prod_us, categoria, by = c("produto_categoria_id" = "categoria_id"))

##Fim do teste

## Primeira vez pra verificar quais são os top5
pr_top5_fat_us <- p_ij_n_ij_pp_ij_prod_us %>%
  select(produto_categoria_id, pp_valor) %>%
  group_by(produto_categoria_id) %>%
  mutate(fat = sum(pp_valor)) %>%
  mutate(n = n()) %>%
  distinct (produto_categoria_id, .keep_all = TRUE) %>%
  collect ()

##Vou fazer um join pra pegar os nomes de cada categoria
categoria <- tbl(con, "categoria") %>%
  select(categoria_id, categoria_nome) %>%
  collect()

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
  collect ()


fat_tot_categorias_us <- pr_top5_fat_med_us


##Adicionando a matriz pra poder exibir no gráfico
fat_tot_categorias_us <- fat_tot_categorias_us %>%
  mutate(med_emp = media_empresa_us) %>%
  collect()

### Gráfico p4 - Ticket médio usados
ax <- list(
  autotick = TRUE,
  title = "",
  showticklabels = TRUE)
p4 <- plot_ly(fat_tot_categorias_us, type = "bar", x = ~categoria_nome, y = ~fat_med,
              name = 'Categorias',
              marker = list(color = 'lightblue'),
              text = ~paste(categoria_nome,'<br>' , func_fmt_din(fat_med)),
              hoverinfo = "text")

p4 <- p4 %>%
  layout(barmode = 'identity', xaxis = ax, yaxis = ax)

p4 <- p4 %>% add_trace(type = 'scatter', mode = 'markers+line', yaxis = 'y2',
                       name = 'Empresa (geral)',
                       x = ~categoria_nome,
                       y = ~med_emp,
                       line = list(color = 'red'),
                       text = ~paste('Ticket médio<br>' , func_fmt_din(med_emp)),
                       hoverinfo = "text",
                       marker = list(color = 'orange'))

p4 <- p4 %>%
  layout(
    autosize = F,
    yaxis = list(side = 'left', title = '', showgrid = TRUE, zeroline = FALSE, title = '', range = c(0,170000)),
    #range nos dois eixos iguais pra ficar na mesma proporção
    yaxis2 = list(overlaying = "y", showgrid = FALSE, zeroline = FALSE, showticklabels= F, range = c(0,170000)),
    ##aqui eu ajusto onde quero que apareça a legenda
    legend = list(x=0.7, y=0.8)#)
  )
if(dash == F){
  p4
}



if(teste == F){
  #tabelas
  rm(ax, categoria, fat_tot_categorias, p_ij_n_ij_pp_empresa, p_ij_n_ij_pp_empresa_us, pr_top5_fat, pr_top5_fat_aux,
     pr_top5_fat_med, produto, proposta_produto, top5_fat, top5_fat_ij_cat, p_ij_n_ij_pp_ij_prod,
     p_ij_n_ij_v_ij_pp, p_ij_n_ij_v_ij_pp_n, negocio_produto, negocio, fat_tot_categorias_us, p_ij_n_ij_pp_ij_prod_us,
     pr_top5_fat_med_us, pr_top5_fat_us)
  #variáveis
  rm(fat_out, media_empresa, n_out, total_empresa)
}

##################################################################
### Faturamento de propostas em 2020 por status

##coleta todos proposta_pagamenmto
proposta_pagamento <- tbl(con, "proposta_pagamento") %>%
  select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
  collect()

proposta_pagamento <- proposta_pagamento %>%
  filter(pp_ativo == TRUE)

##Junção pra termos valor da proposta (preciso do negócio pra filtrar empresa e vendedores se preciso)
p_ij_n_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_pagamento, by = c("proposta_id" = "pp_proposta_id")) %>%
  arrange(pp_valor)

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

###Gráfico p5 - Faturamento de propostas por status (não usado)
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
proposta_modo_forma <-tbl(con, "proposta_modo_forma") %>%
  select (pmf_id, pmf_nome, pmf_ativo, PMF_EMPRESA_ID) %>%
  filter (pmf_ativo == 1 & PMF_EMPRESA_ID == empresa) %>%
  collect()

#Arrumando encoding
Encoding(proposta_modo_forma$pmf_nome) <- 'latin1'

##Criada sem nome e pmf_empresa (não consegui fazer a junção duas vezes, provavelmente por já ter o tempo)
#proposta_modo_forma_aux <-proposta_modo_forma[,1:2]

p_ij_ppa <- inner_join(proposta, proposta_pagamento, by=c('proposta_id' = 'pp_proposta_id')) %>%
  select (proposta_id, proposta_negocio_id, proposta_data_cadastro, proposta_status, pp_id, pp_modo_id, pp_forma_id, pp_usado_id, pp_ativo) %>%
  filter (pp_ativo == T, pp_modo_id != 0, pp_forma_id != 0)

##conta antes de substituir (provavelmente mais rápido lidar com int do que string) (1 contando modo) (conta todos, depois filtro através de proposta_modo_forma)
p_ij_ppa_count_modo <- p_ij_ppa %>%
  select (pp_id, pp_modo_id) %>%
  group_by(pp_modo_id) %>%
  mutate(cont_modo_aux = n()) %>%
  ungroup() %>%
  distinct(pp_modo_id, .keep_all = T)

if (empresa == 16)
{
  ##Vou calcular 5% e agrupar esses que forem menores que 5% em outra categoria (outros)
  n_total_modo <- sum(p_ij_ppa_count_modo$cont_modo_aux)
  aux_3perc <- n_total_modo/33
  p_ij_ppa_count_modo$pp_modo_id[p_ij_ppa_count_modo$cont_modo_aux < aux_3perc] <- 26
}

##Aqui eu estou refazendo a contagem pq já havia uma categoria "Outras", só estou adicionando os demais (com taxa <3%) nela
p_ij_ppa_count_modo <- p_ij_ppa_count_modo %>%
  select (pp_modo_id, cont_modo_aux) %>%
  group_by(pp_modo_id) %>%
  mutate(cont_modo = sum(cont_modo_aux)) %>%
  ungroup() %>%
  distinct(pp_modo_id, .keep_all = T)

p_ij_ppa_cont_modo_ij_pmf <- inner_join (p_ij_ppa_count_modo, proposta_modo_forma, by = c("pp_modo_id" = "pmf_id")) %>%
  select(pp_modo_id, cont_modo, pmf_nome) %>%
  rename(
    Modo = pmf_nome)



#conta antes de substituir (provavelmente mais rápido lidar com int do que string) (2 contando forma) (conta todos, depois filtro através de proposta_modo_forma)
p_ij_ppa_count_forma <- p_ij_ppa %>%
  select (pp_id, pp_forma_id) %>%
  group_by(pp_forma_id) %>%
  mutate(cont_forma = n()) %>%
  ungroup() %>%
  distinct(pp_forma_id, .keep_all = T)

if (empresa == 16){
  ##Aqui terei que fazer difernte, porque não existe a coluna OUTRAS, criarei uma
  p_ij_ppa_count_forma$pp_forma_id[p_ij_ppa_count_forma$cont_forma < aux_3perc] <- 2
  n_outros_forma <- sum(p_ij_ppa_count_forma$cont_forma[which(p_ij_ppa_count_forma$pp_forma_id==2)])
}

p_ij_ppa_cont_forma_ij_pmf <- inner_join (p_ij_ppa_count_forma, proposta_modo_forma, by = c("pp_forma_id" = "pmf_id")) %>%
  select(pp_forma_id, cont_forma, pmf_nome) %>%
  rename(
    Forma = pmf_nome)

if (empresa == 16){
  ##Adicicionando a linha "outros"
  p_ij_ppa_cont_forma_ij_pmf <- p_ij_ppa_cont_forma_ij_pmf %>%
    add_row(pp_forma_id = -1, cont_forma = n_outros_forma, Forma = "OUTRAS")
}

### Gráfico p6 - Modos de pagamento

p6 <- plot_ly()
p6 <- p6 %>%
  add_pie(data = p_ij_ppa_cont_modo_ij_pmf, values = ~cont_modo, labels = ~Modo,
          name = 'Modo de pagamento'
  )
if(dash == F){
  p6
}

### Gráfico p7 - Formas de pagamento

p7 <- plot_ly()
p7 <- p7 %>%
  add_pie(data = p_ij_ppa_cont_forma_ij_pmf, values = ~cont_forma, labels = ~Forma,
          name = 'Forma de pagamento'
  )
if(dash == F){
  p7
}

if(teste == F){
  #tabelas
  rm(p_ij_ppa, proposta, proposta_modo_forma, p_ij_ppa_count_modo, p_ij_ppa_cont_modo_ij_pmf,
     p_ij_ppa_count_forma, p_ij_ppa_cont_forma_ij_pmf, proposta_pagamento);
  #variáveis
  rm(aux_3perc, media_empresa_us, n_empresa, n_empresa_us, n_outros_forma, n_total_modo, total_empresa_us);
}
if (teste == 0) {
  #rm(list=ls())
}


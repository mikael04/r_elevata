# rm(list = ls())
#Lib q será futuramente usada pros painéis interativos
#library(shiny)
#Lib pra conexão com o banco
#library(odbc)
#Lib para ler (mais rapidamente) os csvs
library(data.table)
#lib com uma cacetada de outras libs para manipular dados
#library(tidyverse)
#Libs pra trabalhar com a base (cortes e funções similares ao SQL)
library(dplyr) #Contido no tidyverse
#lib para lidar com as palavras
#library(stringr)
#library(htmlwidgets)
#library(viridis)
#Lib usada pros treemaps
#library(treemap)
#Lib usada pros waffles
#library(waffle)
#usada para converter números em moeda
#library(scales)
#lib para plotar a imagem (s_dados)
library(knitr)
source("fct_tempo.R")
source("fct_fmt_din.R")
source("fct_fmt_nome.R")


###################################
##Variáveis "Globais"
####Variavel de teste para não remover e imprimir valores de teste, 1 para teste, 0 para não estou testando, rodando
teste = F
####Variável usada para não plotar os gráficos na dash
dash = T

### Tempo
if(!teste){
  ##Teste se estou gerando via rstudio (knit)
  if(as.integer(params$num_dias) == 0) {
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual = fct_ano_atual()
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = fct_mes_atual()
    ## Apenas ano, para gerar títulos
    ano <- year(ano_atual)
    ##Execução normal, recebendo data do gerador de dashs
  }else{
    data <- (lubridate::today()-lubridate::days(params$num_dias))
    ####Variavel global c/ ano atual (para comparação) ##primeiro dia do ano no formato ano-mes-dia
    ano_atual= lubridate::ymd(data-months(lubridate::month(data)-1)- days(lubridate::day(data)-1)) 
    ####Variavel global c/ mês atual (para comparação)
    mes_atual = lubridate::ymd(data -days(lubridate::day(data)-1))
    ## Apenas ano, para gerar títulos
    ano <- lubridate::year(ano_atual)
  }
}else{
  ##Teste setando dia
  #data <- lubridate::ymd("2020-12-31")
  data <- lubridate::today()
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

##Teste, senão tiver parâmetro, estou fazendo o teste e entra no if, senão vai pro else
if(params$variable1 == 'emp_par'){
  empresa = 16
}else{
  empresa = as.integer(params$variable1)
}

###################################################################################
## Gerando tabela

##-> Collect cria o dataframe resultado da query, proposta será a tabela na qual estou lendo (FROM cliente)
##coleta todas as propostas
proposta <- fread("Tabelas/proposta.csv", colClasses = c(proposta_id = "character", proposta_negocio_id = "character")) %>%
  dplyr::select(proposta_id, proposta_versao, proposta_negocio_id, proposta_data_cadastro, proposta_status)

##-> Collect cria o dataframe resultado da query, negocio será a tabela na qual estou lendo (FROM cliente)
negocio <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character", negocio_produto_id = "character")) %>%
  dplyr::select(negocio_id, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_data_cadastro, negocio_usado, negocio_produto_id, negocio_cliente_id)


##coleta todos os vendedores
vendedor <- fread("Tabelas/vendedor.csv") %>%
  dplyr::select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  dplyr::filter (vendedor_empresa_id == empresa)

vendedor_a <- vendedor %>%
  dplyr::filter (vendedor_ativo == T)

#Arrumando encoding
Encoding(vendedor$vendedor_nome) <- 'latin1'
vendedor$vendedor_nome <- func_nome(vendedor$vendedor_nome)

if(empresa == 16){
  vendedor$vendedor_nome[vendedor$vendedor_id == 723] <- "BRUNO PE.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 812] <- "BRUNO PO.";
  vendedor$vendedor_nome[vendedor$vendedor_id == 942] <- "LUCAS V. I.";
}

##coleta proposta_pagamento
proposta_pagamento <- fread("Tabelas/proposta_pagamento.csv", colClasses = c(pp_id = "character", pp_proposta_id = "character")) %>%
  dplyr::select(pp_id, pp_proposta_id, pp_modo_id, pp_forma_id, pp_valor, pp_ativo, pp_usado_id) %>%
  dplyr::filter(pp_ativo == 1) %>%
  dplyr::select(-pp_ativo)

###################################

##junção de proposta com negócio
prop_ij_neg <- inner_join(proposta, negocio, by=c("proposta_negocio_id" = "negocio_id")) %>%
  dplyr::select (proposta_id, proposta_data_cadastro, proposta_status, proposta_negocio_id, negocio_data_cadastro, negocio_vendedor_id, negocio_negocio_situacao_id, negocio_usado, negocio_produto_id, negocio_cliente_id)

##Juntando com vendedor pra obter o nome do vendedor
prop_ij_neg_ij_vend <- inner_join(prop_ij_neg, vendedor, by=c("negocio_vendedor_id" = "vendedor_id"))

proposta_produto <- fread("Tabelas/proposta_produto.csv", colClasses = c(pp_id = "character", pp_proposta_id = "character", pp_produto_id = "character")) %>%
  dplyr::select(pp_id, pp_proposta_id, pp_produto_id, pp_quantidade, pp_valor, pp_ativo) %>%
  dplyr::filter (pp_ativo == 1) %>% #Filtrando pp_ativo = true
  dplyr::select(-pp_ativo) %>%
  dplyr::mutate (pp_valor_tot = pp_valor*pp_quantidade)

p_ij_n_ij_v_ij_pp <- inner_join(prop_ij_neg_ij_vend, proposta_produto, by = c("proposta_id" = "pp_proposta_id"))

##Vou puxar negócio novamente pra pegar a coluna de se é usado ou não (acho que é mais eficiente do que usar a tabela completa desde o início)
negocio_aux <- fread("Tabelas/negocio.csv", colClasses = c(negocio_id = "character")) %>%
  dplyr::select(negocio_id, negocio_tipo_negocio)


p_ij_n_ij_v_ij_pp_n <- inner_join(p_ij_n_ij_v_ij_pp, negocio_aux, by = c("proposta_negocio_id" = "negocio_id"))


###################################################################################
## Criação das tabelas
######################
## joins e filtros adicionais
#############################

##coleta todos os vendedores
cliente <- fread("Tabelas/cliente.csv") %>%
  dplyr::select(cliente_id, cliente_nome, cliente_empresa_id) %>%
  dplyr::filter (cliente_empresa_id == empresa)

#Arrumando encoding
Encoding(cliente$cliente_nome) <- 'latin1'

##Vou selecionar produto_nome pra não ter q mudar depois, mas posso cortar essa coluna se preciso e ir só por prod_id
produto <- fread("Tabelas/produto.csv", colClasses = c(produto_id = "character", produto_marca_id = "character", produto_categoria_id = "character")) %>%
  dplyr::select(produto_id, produto_nome, produto_marca_id, produto_categoria_id, produto_empresa_id)

## vendedor, cliente, produto e valor, status, data de cadastro (ordem de cadastro, mais novas primeiro), atualização
p_ij_n_ij_pp_ij_prod <- dplyr::inner_join (p_ij_n_ij_v_ij_pp_n, produto, by= c('pp_produto_id' = 'produto_id')) %>%
  dplyr::select(proposta_id, vendedor_nome, negocio_cliente_id, proposta_data_cadastro, proposta_status, pp_valor_tot, produto_nome, negocio_tipo_negocio)
p_ij_n_ij_pp_ij_prod_cli <- dplyr::inner_join (p_ij_n_ij_pp_ij_prod, cliente, by= c('negocio_cliente_id' = 'cliente_id')) %>%
  dplyr::select(-negocio_cliente_id)

#https://letmegooglethat.com/?q= 2020012416165952745
prop_ate_1ano_ant <- p_ij_n_ij_pp_ij_prod_cli %>%
  dplyr::filter(proposta_data_cadastro > ano_atual - years(1)) %>%
  dplyr::filter(proposta_status == 0) %>%
  dplyr::select(proposta_id, vendedor_nome, cliente_nome, proposta_data_cadastro, proposta_status, pp_valor_tot, produto_nome, negocio_tipo_negocio)

prop_ate_1ano_ant <- prop_ate_1ano_ant %>%
  dplyr::group_by(proposta_id) %>%
  dplyr::mutate (valor_proposta = sum(pp_valor_tot)) %>%
  dplyr::mutate('Produto + Valor' = paste(produto_nome, format(valor_proposta, scientific = F), sep = "  - R$")) %>%
  # dplyr::select(-pp_valor_tot) %>%
  dplyr::distinct(proposta_id, .keep_all = T) %>%
  dplyr::ungroup () %>%
  
  #Selecionando colunas e alterando nomes
  dplyr::select(-proposta_id, -valor_proposta) %>%
  dplyr::rename(Vendedor = vendedor_nome, Cliente = cliente_nome) %>%
  
  #Selecionando colunas (com if e alterando tipo de data) e alterando nomes
  dplyr::select(-negocio_tipo_negocio) %>%
  dplyr::mutate('Data de Cadastro' = lubridate::date(proposta_data_cadastro)) %>%
  dplyr::arrange(desc(proposta_data_cadastro)) %>%
  dplyr::mutate('Status' = dplyr::if_else(proposta_status == 0, 'Pendente', 'Revisada')) %>%
  dplyr::select(Vendedor, Cliente, 'Produto + Valor', Status, 'Data de Cadastro')

#'<a href="http://rstudio.com">RStudio</a>'
#prop_ate_1ano_ant <- prop_ate_1ano_ant %>%
#  dplyr::mutate(Link = paste0('<a href="', paste0("https://letmegooglethat.com/?q=", 'Data de Cadastro'),'", #target=\"_blank\">Link da proposta</a>'))

data.table::fwrite(prop_ate_1ano_ant, "Testes/propostas_pendentes.csv")
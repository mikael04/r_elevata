library(purrr)
library(data.table)
library(dplyr)

setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
teste = T
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas_mapas

# geral
# marcas
# marcas_k
# negocios
# propostas
# visitas_clientes

render_report_ger <- function(var1) {
  
  template <- "dash_geral.Rmd"
  print(var1)
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  out_file <- sprintf("Dashs/Geral_%s", var1)
  #teste
  #var1 = 16
  parameters <- list(variable1 = as.list(param_list[var1]))
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_mar <- function(var1) {
  
  template <- "dash_marcas.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Mapas_%s", var1)
  out_file <- sprintf("Dashs/Marcas_%s", (var1))
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_mar_k <- function(var1) {
  
  template <- "dash_marcas_k.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Mapas_%s", var1)
  out_file <- sprintf("Dashs/Marcas_%s", (var1))
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_neg <- function(var1) {
  
  template <- "dash_negocios.Rmd"
  
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  out_file <- sprintf("Dashs/Negocios_%s", (var1))
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_prop <- function(var1) {
  
  template <- "dash_propostas.Rmd"
  
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  out_file <- sprintf("Dashs/Propostas_%s", (var1))
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_vis_cli <- function(var1) {

  template <- "dash_visitas_clientes.Rmd"
  
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Visitas_%s", var1)
  out_file <- sprintf("Dashs/Visitas_Clientes_%s", (var1))

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
empresas_ativas <- fread("empresas_ativas_id.csv") %>%
  select(empresa_id)

params_list_i <- empresas_ativas$empresa_id
params_list <- as.list(params_list_i)

x <- length(params_list)

########################################################
###Teste
params_test <- list(65)
template <- "dash_geral.Rmd"
out_file <- sprintf("Dashs/Geral_%s", params_test[1])
parameters <- list(variable1 = params_list[i])

rmarkdown::render(template,
                  output_file = out_file,
                  params = parameters)

########################################################


##Gerando dashs geral ##Funcionando, apenas comentada pra facilitar teste das marcas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste')))
  template <- "dash_geral.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs/Geral_%s", params_list[i])
  parameters <- list(variable1 = params_list[i])

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
##Gerando dashs marcas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'empresas_ativas')))
  template <- "dash_marcas.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs/Marcas_%s", params_list[i])
  parameters <- list(variable1 = params_list[i])
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

## Gerando dash marcas_k
for(i in params_list){
  if(i == 78) {
    template <- "dash_marcas_k.Rmd"
    #Teste (nome da empresa, mais fácil de analisar)
    # out_file <- sprintf("Dashs/Mapas_%s", var1)
    out_file <- sprintf("Dashs/Marcas_%s", i)
    
    parameters <- list(variable1 = as.list(params_list[i]))
    
    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }
}
## Gerando dash negocios
for(i in params_list){
  template <- "dash_negocios.Rmd"
  
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  out_file <- sprintf("Dashs/Negocios_%s", i)
  
  parameters <- list(variable1 = as.list(params_list[i]))
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash propostas
for(i in params_list){
  if(i == 16 | i == 50 | i == 78){
    template <- "dash_propostas.Rmd"
    
    #Teste (nome da empresa, mais fácil de analisar)
    # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
    out_file <- sprintf("Dashs/Propostas_%s", i)
    
    parameters <- list(variable1 = as.list(params_list[i]))
    
    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }
}
## Gerando dash visitas_clientes
for(i in params_list){
  template <- "dash_visitas_clientes.Rmd"
  
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Visitas_%s", var1)
  out_file <- sprintf("Dashs/Visitas_Clientes_%s", i)
  
  parameters <- list(variable1 = as.list(params_list[i]))
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

param_list[1]
typeof(param_list[1])
as.integer(param_list[1])
typeof(as.integer(param_list[1]))

pmap(params_list[[1]], render_report_ger)
pmap(params_list[[2]], render_report_ger)
pmap(params_list[[3]], render_report_ger)
pmap(params_list[[4]], render_report_ger)
pmap(params_list[[5]], render_report_ger)
pmap(params_list[[6]], render_report_ger)
pmap(params_list[[7]], render_report_ger)
#Gerando relatórios marcas, todas menos komatsu
pmap(params_list[[1]], render_report_mar)
pmap(params_list[[2]], render_report_mar)
pmap(params_list[[4]], render_report_mar)
pmap(params_list[[5]], render_report_mar)
pmap(params_list[[6]], render_report_mar)
pmap(params_list[[7]], render_report_mar)
#Gerando relatórios marcas da komatsu
pmap(params_list[[3]], render_report_mar_k) #Kom
#Gerando relatórios negócios, todas
pmap(params_list[[1]], render_report_neg)
pmap(params_list[[2]], render_report_neg)
pmap(params_list[[3]], render_report_neg)
pmap(params_list[[4]], render_report_neg)
pmap(params_list[[5]], render_report_neg)
pmap(params_list[[6]], render_report_neg)
pmap(params_list[[7]], render_report_neg)
##Proposta Super e Komatsu
pmap(params_list[[3]], render_report_prop) #Kom
pmap(params_list[[6]], render_report_prop) #Sup
#Gerando relatórios visitas_clientes, todas
pmap(params_list[[1]], render_report_vis_cli)
pmap(params_list[[2]], render_report_vis_cli)
pmap(params_list[[3]], render_report_vis_cli)
pmap(params_list[[4]], render_report_vis_cli)
pmap(params_list[[5]], render_report_vis_cli)
pmap(params_list[[6]], render_report_vis_cli)
pmap(params_list[[7]], render_report_vis_cli)
#pmap(params_list[[7]], render_report_vis_cli)


#############################################################
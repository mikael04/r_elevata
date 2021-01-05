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


empresas_ativas <- fread("empresas_ativas_id.csv") %>%
  select(empresa_id)

params_list_i <- empresas_ativas$empresa_id
params_list <- as.list(params_list_i)
num_dias_list <- as.list(5)
x <- length(params_list)
####################################
####################################
########################################################
###Teste
# params_test <- list(78)
# template <- "dash_marcas_k.Rmd"
# out_file <- sprintf("Dashs_final_ano/Marcas_%s", params_test[1])
# parameters <- list(variable1 = params_list[i])
# 
# rmarkdown::render(template,
#                   output_file = out_file,
#                   params = parameters)
########################################################
## Gerando dashs geral ##Funcionando, apenas comentada pra facilitar teste das marcas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
  template <- "dash_geral_final_ano.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs_final_ano/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    #print(params_list[[i]])
    #print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs_final_ano/Geral_%s", params_list[i])
  ##Final de ano
  parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dashs marcas
for(i in (1:x)){
  if (params_list[i] != 78 & params_list[i] != 79)
  {
    rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
    template <- "dash_marcas_final_ano.Rmd"
    if(teste){
      print(i)
      print(params_list[i])
      print(params_list[[i]])
      print(as.list(params_list[[i]]))
    }
    out_file <- sprintf("Dashs_final_ano/Marcas_%s", params_list[i])
    parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
    
    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }else{ ##########Caso seja a komatsu, marcas diferentes
    template <- "dash_marcas_k_final_ano.Rmd"
    out_file <- sprintf("Dashs_final_ano/Marcas_%s", params_list[i])
    parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
    
    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }
}
## Gerando dash negocios
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
  template <- "dash_negocios_final_ano.Rmd"
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs_final_ano/Negocios_%s", params_list[i])
  parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash propostas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
  template <- "dash_propostas_final_ano.Rmd"
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs_final_ano/Propostas_%s", params_list[i])
  parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash visitas_clientes
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
  template <- "dash_visitas_clientes_final_ano.Rmd"
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs_final_ano/Visitas_Clientes_%s", params_list[i])
  parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
#############################################################
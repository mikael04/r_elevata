rm(list = ls())
library(purrr)
library(data.table)
library(dplyr)

setwd("C:\\Users\\sysadmin\\Documents\\R\\Scripts")
teste = T
Sys.setenv(RSTUDIO_PANDOC="C:\\Program Files\\RStudio\\bin\\pandoc")

# geral
# marcas
# marcas_k
# negocios
# propostas
# visitas_clientes

empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
  select(empresa_id)

params_list_i <- empresas_ativas$empresa_id
params_list <- as.list(params_list_i)

x <- length(params_list)
## Parâmetro usado para gerar arquivos em pasta de teste
teste <- T

########################################################
###Teste
# params_test <- list(78)
# template <- "dash_geral.Rmd"
# out_file <- sprintf("Dashs/Geral_%s", params_test[1])
# parameters_test <- list(variable1 = params_test[1])
#
# parameters_test
#
# rmarkdown::render(template,
#                   output_file = out_file,
#                   params = parameters_test)

########################################################

##Gerando dashs geral ##Funcionando, apenas comentada pra facilitar teste das marcas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste')))
  template <- "dash_geral.Rmd"
  #Teste (nome da empresa, mais fÃ¡cil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  if(teste){
    out_file <- sprintf("Dashs_teste/Geral_%s", params_list[i])
  }else{
    out_file <- sprintf("Dashs/Geral_%s", params_list[i])
  }
  parameters <- list(variable1 = params_list[i])

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
##Gerando dashs marcas
for(i in (1:x)){
  if (params_list[i] != 78 & params_list[i] != 79)
  {
    rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'empresas_ativas')))
    template <- "dash_marcas.Rmd"
    #Teste (nome da empresa, mais fÃ¡cil de analisar)
    # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
    if(teste){
      print(i)
      print(params_list[i])
      print(params_list[[i]])
      print(as.list(params_list[[i]]))
    }
    if(teste){
      out_file <- sprintf("Dashs_teste/Marcas_%s", params_list[i])
    }else{
      out_file <- sprintf("Dashs/Marcas_%s", params_list[i])
    }
    parameters <- list(variable1 = params_list[i])

    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }else{ ##########Caso seja a komatsu, marcas diferentes
    template <- "dash_marcas_k.Rmd"
    if(teste){
      out_file <- sprintf("Dashs_teste/Marcas_%s", params_list[i])
    }else{
      out_file <- sprintf("Dashs/Marcas_%s", params_list[i])
    }
    parameters <- list(variable1 = params_list[i])

    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }
}
# ## Gerando dash negocios
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste')))
  template <- "dash_negocios.Rmd"
  #Teste (nome da empresa, mais fÃ¡cil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  if(teste){
    out_file <- sprintf("Dashs_teste/Negocios_%s", params_list[i])
  }else{
    out_file <- sprintf("Dashs/Negocios_%s", params_list[i])
  }
  parameters <- list(variable1 = params_list[i])

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash propostas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste')))
  template <- "dash_propostas.Rmd"
  #Teste (nome da empresa, mais fÃ¡cil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  if(teste){
    out_file <- sprintf("Dashs_teste/Propostas_%s", params_list[i])
  }else{
    out_file <- sprintf("Dashs/Propostas_%s", params_list[i])
  }
  parameters <- list(variable1 = params_list[i])

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash visitas_clientes
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste')))
  template <- "dash_visitas_clientes.Rmd"
  #Teste (nome da empresa, mais fÃ¡cil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    print(params_list[[i]])
    print(as.list(params_list[[i]]))
  }
  if(teste){
    out_file <- sprintf("Dashs_teste/Visitas_Clientes_%s", params_list[i])
  }else{
    out_file <- sprintf("Dashs/Visitas_Clientes_%s", params_list[i])
  }
  parameters <- list(variable1 = params_list[i])

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
#############################################################
#############################################################
## Gerar arquivo com horário de última atualização
#############################################################

# data <- paste0("Gráficos: ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
# writeLines(data, "update_time.txt")

#############################################################

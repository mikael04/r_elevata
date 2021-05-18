rm(list = ls())
library(purrr)
library(data.table)
library(dplyr)

teste = F
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas_mapas

# geral
# marcas
# marcas_k
# negocios
# propostas
# visitas_clientes

################################
## Parâmetros de empresas
# empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
#   select(empresa_id)
#
# params_list_i <- empresas_ativas$empresa_id
# params_list <- as.list(params_list_i)
# ## 0 -> indica que estamos rodando para o dia atual
# num_dias_list <- as.list(0)
# x <- length(params_list)


################################
## Parâmetros de vendedores
empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
  select(empresa_id)

params_list_i <- empresas_ativas$empresa_id
empresas_ativ <- as.list(params_list_i)

vendedores_all <- fread("Tabelas/vendedor.csv") %>%
  dplyr::select(vendedor_id, vendedor_nome, vendedor_empresa_id, vendedor_ativo) %>%
  dplyr::filter (vendedor_ativo == T) %>%
  dplyr::select(vendedor_id, vendedor_empresa_id)

#Arrumando #Encoding
#Encoding(vendedores_all$vendedor_nome) <- 'latin1' #'UTF-8'
vend_empresa <- NULL
#as.character(vendedor_a)
for(i in (1:length(empresas_ativ))){
  print(paste0("i = ",i))
  print(paste0("empresa = ", empresas_ativ[[i]]))
  vend_empresa[i] <- vendedores_all %>%
    dplyr::filter(vendedor_empresa_id == empresas_ativ[[i]]) %>%
    dplyr::select(vendedor_id)
  empresas_ativ[[i]][2] <- vend_empresa[i]
}
# as.list(vend_empresa[[3]])
# vend_empresa[[3]]
# length(as.list(vend_empresa[[3]]))
# empresas_ativ[[3]][2] <- vend_empresa[[3]]
# (empresas_ativ[[3]][2])
lengths(empresas_ativ[[1]][2])
empresas_ativ[[3]][[2]][1]
empresas_ativ[[1]][[1]][1]
length(empresas_ativ[[3]][[2]])
i=0
length(empresas_ativ)
for (i in (1:length(empresas_ativ))){
  if(length(empresas_ativ[[i]][[2]]) >0){
    print(paste0("i == ", i))
    print(paste0("empresa == ", empresas_ativ[[i]][[1]][1]))
    print(paste0("vendedor == ", empresas_ativ[[i]][[2]]))
  }
}
length(empresas_ativ[[i]][[2]])
empresas_ativ[[1]][[1]]
########################################################
## Funcionando, apenas comentada pra facilitar teste das marcas
# params_test <- list(78)
# template <- "dash_marcas_k.Rmd"
# out_file <- sprintf("Dashs/Marcas_%s", params_test[1])
# parameters <- list(variable1 = params_list[i])
#
# rmarkdown::render(template,
#                   output_file = out_file,
#                   params = parameters)
########################################################
# ## Gerando dashs das empresas
## Gerando dashs geral
for(i in (1:)){
  rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
  template <- "dash_geral.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste){
    print(i)
    print(params_list[i])
    #print(params_list[[i]])
    #print(as.list(params_list[[i]]))
  }
  out_file <- sprintf("Dashs/Geral_%s", params_list[i])
  ##Final de ano
  parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
# ## Gerando dashs marcas
# for(i in (1:x)){
#   if (params_list[i] != 78 & params_list[i] != 79)
#   {
#     rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
#     template <- "dash_marcas.Rmd"
#     if(teste){
#       print(i)
#       print(params_list[i])
#       print(params_list[[i]])
#       print(as.list(params_list[[i]]))
#     }
#     out_file <- sprintf("Dashs/Marcas_%s", params_list[i])
#     parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
#
#     rmarkdown::render(template,
#                       output_file = out_file,
#                       params = parameters)
#     invisible(TRUE)
#   }else{ ##########Caso seja a komatsu, marcas diferentes
#     template <- "dash_marcas_k.Rmd"
#     out_file <- sprintf("Dashs/Marcas_%s", params_list[i])
#     parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
#
#     rmarkdown::render(template,
#                       output_file = out_file,
#                       params = parameters)
#     invisible(TRUE)
#   }
# }
# ## Gerando dash negocios
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
#   template <- "dash_negocios.Rmd"
#   if(teste){
#     print(i)
#     print(params_list[i])
#     print(params_list[[i]])
#     print(as.list(params_list[[i]]))
#   }
#   out_file <- sprintf("Dashs/Negocios_%s", params_list[i])
#   parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
# ## Gerando dash propostas
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
#   template <- "dash_propostas.Rmd"
#   if(teste){
#     print(i)
#     print(params_list[i])
#     print(params_list[[i]])
#     print(as.list(params_list[[i]]))
#   }
#   out_file <- sprintf("Dashs/Propostas_%s", params_list[i])
#   parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
# ## Gerando dash visitas_clientes
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "params_list", "i", 'x', 'teste', 'num_dias_list')))
#   template <- "dash_visitas_clientes.Rmd"
#   if(teste){
#     print(i)
#     print(params_list[i])
#     print(params_list[[i]])
#     print(as.list(params_list[[i]]))
#   }
#   out_file <- sprintf("Dashs/Visitas_Clientes_%s", params_list[i])
#   parameters <- list(variable1 = params_list[i], num_dias = num_dias_list[[1]])
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
# #############################################################
# ## Gerar arquivo com horário de última atualização
# #############################################################
#
# # data <- paste0("Gráficos: ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
# # writeLines(data, "update_time.txt")
#
# #############################################################

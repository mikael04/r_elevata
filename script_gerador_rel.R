library(purrr)
library(data.table)
library(dplyr)
rm(list = ls())
source("fct_rmd_html.R")
#setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas_mapas

# geral
# marcas
# marcas_k
# negocios
# propostas
# visitas_clientes

## lê empresas ativas e prepara lista
empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
  select(empresa_id)
params_list_i <- empresas_ativas$empresa_id
empresas_ativ <- as.list(params_list_i)
## 0 -> indica que estamos rodando para o dia atual, podendo passar um parâmetro de dias para rodar antes
num_dias_list <- as.list(0)
x <- length(empresas_ativ)
## Parâmetro geral para debug
debug <- T

###Teste
# params_test <- list(78)
# template <- "dash_marcas_k.Rmd"
# out_file <- sprintf("Dashs/Marcas_%s", params_test[1])
# parameters <- list(variable1 = empresas_ativ[i])
#
# rmarkdown::render(template,
#                   output_file = out_file,
#                   params = parameters)
########################################################

################################################################################
## Empresas
################################################################################
########################################################
### Parâmetros de teste
param_vendedor <- "vend_par" ## Caso queira testar apenas um parâmetro
param_empresa <- "emp_ar" ## Caso queira testar apenas um parâmetro
## Parâmetro usado para gerar arquivos em pasta de teste e ver outros parâmetros de teste
teste_ger_rel <- F
teste_ger_rel_ind <- F
## Parâmetros para testes individuais em cada dash
teste_geral <- T
teste_marcas <- T
teste_negocios <- T
teste_propostas <- T
teste_visitas_clientes <- T
# teste_geral <- F
# teste_marcas <- F
# teste_negocios <- F
# teste_propostas <- F
# teste_visitas_clientes <- F
## Parâmetros de empresa (F para empresa, T para vendedor)
param_dash_vend <- F
## Parâmetros de mobile (ainda não usado)
param_dash_mob <- F
########################################################
########################################################
# Declaração de parâmetros gerais para knit
########################################################
dont_delete = c('empresas_ativ', 'empresas_ativas', 'vend_empresa', 'vendedores_all', 'i', 'j', 'x',
                'dont_delete', 'debug', 'teste', 'template', 'empresa', 'vendedor', 'out_f_emp',
                'nome_dash', 'num_dias_list', 'apenas_k', 'teste_ger_rel', 'out_f_vend', 'out_f',
                'template_list', 'nome_dash_list', 'param_list', 'func_rmd_html_emp',
                'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa',
                'teste_geral', 'teste_marcas', 'teste_negocios', 'teste_propostas',
                'teste_visitas_clientes')
out_f_emp = paste0(getwd(),"/Dashs/")
template_list <- list("dash_geral_unificada.Rmd", "dash_marcas_k_unificada.Rmd",
                      "dash_negocios_unificada.Rmd", "dash_propostas_unificada.Rmd",
                      "dash_visitas_clientes_unificada.Rmd")
nome_dash_list <- list("Geral", "Marcas", "Negocios", "Propostas", "Visitas_clientes")
########################################################
########################################################
## x = Tipo de dash que estará rodando (geral, marcas, negocios, propostas, visitas_clientes)
## i vai rodar por empresas
## empresas_ativ[[i]][[1]][1] -> i são as empresas ativas, posição [1][1] = primeiro [1] posição da empresa, segundo [1] valor da empresa
########################################################
x = 1
i = 1
for(x in (1:length(template_list))){
  for(i in (1:length(empresas_ativ))){
    if(teste_ger_rel){ ## Teste, vai pra pasta Dashs_teste
      if(teste_ger_rel_ind){ ## Teste, se vou gerar individualmente
        if(teste_geral){ ## Teste Geral
          func_rmd_html_emp(dont_delete, template = template_list[[1]], empresa = empresas_ativ[[i]][[1]],
                            param_dash_vend, param_dash_mob, out_f = out_f_emp,
                            nome_dash = nome_dash_list[[1]], teste_ger_rel, num_dias_list = 0,
                            debug = debug)
        }
        if(teste_marcas){ ## Teste Marcas
          func_rmd_html_emp(dont_delete, template = template_list[[2]], empresa = empresas_ativ[[i]][[1]],
                            param_dash_vend, param_dash_mob, out_f = out_f_emp,
                            nome_dash = nome_dash_list[[2]], teste_ger_rel, num_dias_list = 0,
                            debug = debug)
        }
        if(teste_negocios){ ## Teste Negocios
          func_rmd_html_emp(dont_delete, template = template_list[[3]], empresa = empresas_ativ[[i]][[1]],
                            param_dash_vend, param_dash_mob, out_f = out_f_emp,
                            nome_dash = nome_dash_list[[3]], teste_ger_rel, num_dias_list = 0,
                            debug = debug)
        }
        if(teste_propostas){ ## Teste Propostas
          func_rmd_html_emp(dont_delete, template = template_list[[4]], empresa = empresas_ativ[[i]][[1]],
                            param_dash_vend, param_dash_mob, out_f = out_f_emp,
                            nome_dash = nome_dash_list[[4]], teste_ger_rel, num_dias_list = 0,
                            debug = debug)
        }
        if(teste_visitas_clientes){ ## Teste Visitas_clientes
          func_rmd_html_emp(dont_delete, template = template_list[[5]], empresa = empresas_ativ[[i]][[1]],
                            param_dash_vend, param_dash_mob, out_f = out_f_emp,
                            nome_dash = nome_dash_list[[5]], teste_ger_rel, num_dias_list = 0,
                            debug = debug)
        }
      }else{
        func_rmd_html_emp(dont_delete, template = template_list[[x]], empresa = empresas_ativ[[i]][[1]],
                          param_dash_vend, param_dash_mob, out_f =  out_f_emp,
                          nome_dash = nome_dash_list[[x]], teste_ger_rel, num_dias_list = 0,
                          debug =  debug)
      }

    }else{ ## Não é teste, rodar todas as dash, jogar pra pasta Dashs
      ## Todas são rodadas atráves do x(dash) e i(empresa)
      func_rmd_html_emp(dont_delete, template = template_list[[x]], empresa = empresas_ativ[[i]][[1]],
                        param_dash_vend, param_dash_mob, out_f =  out_f_emp,
                        nome_dash = nome_dash_list[[x]], teste_ger_rel, num_dias_list = 0,
                        debug =  debug)
    }
  }
}

################################################################################
## Vendedores
################################################################################
########################################################
### Parâmetros de teste
## Parâmetro usado para gerar arquivos em pasta de teste e ver outros parâmetros de teste
teste_ger_rel <- T
teste_ger_rel_ind <- F
## Parâmetros para testes individuais em cada dash
teste_geral_v <- T
teste_marcas_v <- T
teste_negocios_v <- T
teste_propostas_v <- T
teste_visitas_clientes_v <- T
# teste_geral_v <- F
# teste_marcas_v <- F
# teste_negocios_v <- F
# teste_propostas_v <- F
# teste_visitas_clientes_v <- F
## Parâmetros de empresa (F para empresa, T para vendedor)
param_dash_vend <- T ## Alterando para gerar dashs de vendedores
## Parâmetros de mobile (ainda não usado)
param_dash_mob <- F
########################################################
########################################################
# Declaração de parâmetros gerais para knit
########################################################
dont_delete = c('empresas_ativ', 'empresas_ativas', 'vend_empresa', 'vendedores_all', 'i', 'j', 'x',
                'dont_delete', 'debug', 'teste', 'template', 'empresa', 'vendedor', 'out_f_emp',
                'nome_dash', 'num_dias_list', 'apenas_k', 'teste_ger_rel', 'out_f_vend', 'out_f',
                'template_list', 'nome_dash_list', 'param_list', 'func_rmd_html_vend',
                'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa',
                'teste_geral', 'teste_marcas', 'teste_negocios', 'teste_propostas',
                'teste_visitas_clientes')
out_f_vend = paste0(getwd(),"/Dashs_vendedores/")
########################################################
########################################################
## Para vendedores, inicialmente teremos apenas komatsu
## apenas komatsu
apenas_k <- T
## Vendedores
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
if (debug){
  for (i in (1:length(empresas_ativ))){
    if(length(empresas_ativ[[i]][[2]]) >0){
      print(paste0("i == ", i))
      print(paste0("empresa == ", empresas_ativ[[i]][[1]][1]))
      print(paste0("vendedor == ", empresas_ativ[[i]][[2]]))
    }
  }
}

####################################
## i vai rodar por número de empresas
## x vai ser pelo template
## j vai rodar por vendedores dentro da empresa
i = 0 ## Empresa 33 - Komatsu
j = 0
# for(i in (1:length(empresas_ativ))){ ## Se for rodar para todas as empresas
i = 33 ## Empresa 33 - Komatsu
empresas_ativ[[1]][[1]]
  for(x in (1:length(template_list))){
    for(j in (1:length(empresas_ativ[[i]][[2]]))){
      if(length(empresas_ativ[[i]][[2]]) > 0){ ## Se tiver algum vendedor
        if(teste_ger_rel){ ## Teste, vai pra pasta Dashs_teste
          if(teste_ger_rel_ind){ ## Teste, se vou gerar individualmente
            if(teste_geral_v){ ## Teste Geral
              func_rmd_html_vend(dont_delete, template = template_list[[1]], empresa = empresas_ativ[[i]][[1]],
                                vendedor = empresas_ativ[[i]][[1]][j],
                                param_dash_vend, param_dash_mob, out_f = out_f_emp,
                                nome_dash = nome_dash_list[[1]], teste_ger_rel, num_dias_list = 0,
                                debug = debug)
            }
            if(teste_marcas_v){ ## Teste Marcas
              func_rmd_html_vend(dont_delete, template = template_list[[2]], empresa = empresas_ativ[[i]][[1]],
                                vendedor = empresas_ativ[[i]][[1]][j],
                                param_dash_vend, param_dash_mob, out_f = out_f_emp,                                nome_dash = nome_dash_list[[2]], teste_ger_rel, num_dias_list = 0,
                                debug = debug)
            }
            if(teste_negocios_v){ ## Teste Negocios
              func_rmd_html_vend(dont_delete, template = template_list[[3]], empresa = empresas_ativ[[i]][[1]],
                                vendedor = empresas_ativ[[i]][[1]][j],
                                param_dash_vend, param_dash_mob, out_f = out_f_emp,                                nome_dash = nome_dash_list[[3]], teste_ger_rel, num_dias_list = 0,
                                debug = debug)
            }
            if(teste_propostas_v){ ## Teste Propostas
              func_rmd_html_vend(dont_delete, template = template_list[[4]], empresa = empresas_ativ[[i]][[1]],
                                vendedor = empresas_ativ[[i]][[1]][j],
                                param_dash_vend, param_dash_mob, out_f = out_f_emp,                                nome_dash = nome_dash_list[[4]], teste_ger_rel, num_dias_list = 0,
                                debug = debug)
            }
            if(teste_visitas_clientes){ ## Teste Visitas_clientes
              func_rmd_html_vend(dont_delete, template = template_list[[5]], empresa = empresas_ativ[[i]][[1]],
                                vendedor = empresas_ativ[[i]][[1]][j],
                                param_dash_vend, param_dash_mob, out_f = out_f_emp,                                nome_dash = nome_dash_list[[5]], teste_ger_rel, num_dias_list = 0,
                                debug = debug)
            }
          }else{
            func_rmd_html_vend(dont_delete, template = template_list[[x]], empresa = empresas_ativ[[i]][[1]],
                              vendedor = empresas_ativ[[i]][[1]][j],
                              param_dash_vend, param_dash_mob, out_f = out_f_emp,                              nome_dash = nome_dash_list[[x]], teste_ger_rel, num_dias_list = 0,
                              debug =  debug)
          }

        }else{ ## Não é teste, rodar todas as dash, jogar pra pasta Dashs
          ## Todas são rodadas atráves do j (dash) e x(empresa)
          func_rmd_html_vend(dont_delete, template = template_list[[x]], empresa = empresas_ativ[[i]][[1]],
                            vendedor = empresas_ativ[[i]][[1]][j],
                            param_dash_vend, param_dash_mob, out_f = out_f_emp,                            nome_dash = nome_dash_list[[x]], teste_ger_rel, num_dias_list = 0,
                            debug =  debug)
        }
      }
    }
  }
  # for(j in (1:length(empresas_ativ[[i]][[2]]))){
  #   if(length(empresas_ativ[[i]][[2]]) > 0){
  #     if(teste_ger_rel_v){
  #       if(teste_geral_v){ ## Teste Geral
  #         func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                            vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                            nome_dash = "Geral", teste_ger_rel, num_dias_list = 0,
  #                            debug = debug)
  #       }
  #       if(teste_marcas_v){ ## Teste Marcas
  #         func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                            vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                            nome_dash = "Marcas", teste_ger_rel, num_dias_list = 0,
  #                            debug =  debug)
  #       }
  #       if(teste_negocios_v){ ## Teste Negocios
  #         func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                            vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                            nome_dash = "Negocios", teste_ger_rel, num_dias_list = 0,
  #                            debug =  debug)
  #       }
  #       if(teste_propostas_v){ ## Teste Propostas
  #         func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                            vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                            nome_dash = "Propostas", teste_ger_rel, num_dias_list = 0,
  #                            debug =  debug)
  #       }
  #       if(teste_visitas_clientes_v){ ## Teste Visitas_clientes
  #         func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                            vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                            nome_dash = "Visitas_clientes", teste_ger_rel, num_dias_list = 0,
  #                            debug =  debug)
  #       }
  #     }else{ ## Não é teste, rodar todas as dashs
  #       ## Gerando Geral para todos vendedores da empresa
  #       func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                          vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                          nome_dash = "Geral", teste_ger_rel, num_dias_list = 0,
  #                          debug = debug)
  #
  #       ## Gerando Marcas para todos vendedores da empresa
  #       func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                          vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                          nome_dash = "Marcas", teste_ger_rel, num_dias_list = 0,
  #                          debug =  debug)
  #
  #       ## Gerando Negocios para todos vendedores da empresa
  #       func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                          vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                          nome_dash = "Negocios", teste_ger_rel, num_dias_list = 0,
  #                          debug =  debug)
  #
  #       ## Gerando Propostas para todos vendedores da empresa
  #       func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                          vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                          nome_dash = "Propostas", teste_ger_rel, num_dias_list = 0,
  #                          debug =  debug)
  #
  #       ## Gerando Visitas_clientes para todos vendedores da empresa
  #       func_rmd_html_vend(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
  #                          vendedor = empresas_ativ[[i]][[1]][j], param_dash_vend, param_dash_mob, out_f = out_f_emp,
  #                          nome_dash = "Visitas_clientes", teste_ger_rel, num_dias_list = 0,
  #                          debug =  debug)
  #     }
  #   }
  # }
#}
##Gerando dashs geral ##Funcionando, apenas comentada pra facilitar teste das marcas
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
#                           'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
#   template <- "dash_geral_unificada.Rmd"
#   #Teste (nome da empresa, mais fácil de analisar)
#   # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
#   print(teste_ger_rel)
#   if(teste_ger_rel){
#     print(i)
#     print(empresas_ativ[i])
#     print(empresas_ativ[[i]])
#     print(as.list(empresas_ativ[[i]]))
#   }
#   if(teste_ger_rel){
#     out_file <- sprintf("Dashs_teste/Geral_%s", empresas_ativ[i])
#   }else{
#     out_file <- sprintf("Dashs/Geral_%s", empresas_ativ[i])
#   }
#   if(teste_ger_rel){
#     print(out_file)
#   }
#   parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
#                      vendedor = param_vendedor)
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
# ##Gerando dashs marcas
# for(i in (1:x)){
#   if (empresas_ativ[i] != 78 & empresas_ativ[i] != 79)
#   {
#     rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
#                             'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
#     template <- "dash_marcas_unificada.Rmd"
#     #Teste (nome da empresa, mais fácil de analisar)
#     # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
#     if(teste_ger_rel){
#       print(i)
#       print(empresas_ativ[i])
#       print(empresas_ativ[[i]])
#       print(as.list(empresas_ativ[[i]]))
#     }
#     if(teste_ger_rel){
#       out_file <- sprintf("Dashs_teste/Marcas_%s", empresas_ativ[i])
#     }else{
#       out_file <- sprintf("Dashs/Marcas_%s", empresas_ativ[i])
#     }
#     parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
#                        vendedor = param_vendedor)
#
#     rmarkdown::render(template,
#                       output_file = out_file,
#                       params = parameters)
#     invisible(TRUE)
#   }else{ ##########Caso seja a komatsu, marcas diferentes
#     template <- "dash_marcas_k_unificada.Rmd"
#     if(teste_ger_rel){
#       out_file <- sprintf("Dashs_teste/Marcas_%s", empresas_ativ[i])
#     }else{
#       out_file <- sprintf("Dashs/Marcas_%s", empresas_ativ[i])
#     }
#     parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
#                        vendedor = param_vendedor)
#
#     rmarkdown::render(template,
#                       output_file = out_file,
#                       params = parameters)
#     invisible(TRUE)
#   }
# }
# # ## Gerando dash negocios
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
#                           'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
#   template <- "dash_negocios_unificada.Rmd"
#   #Teste (nome da empresa, mais fácil de analisar)
#   # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
#   if(teste_ger_rel){
#     print(i)
#     print(empresas_ativ[i])
#     print(empresas_ativ[[i]])
#     print(as.list(empresas_ativ[[i]]))
#   }
#   if(teste_ger_rel){
#     out_file <- sprintf("Dashs_teste/Negocios_%s", empresas_ativ[i])
#   }else{
#     out_file <- sprintf("Dashs/Negocios_%s", empresas_ativ[i])
#   }
#   parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
#                      vendedor = param_vendedor)
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
# ## Gerando dash propostas
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
#                           'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
#   template <- "dash_propostas_unificada.Rmd"
#   #Teste (nome da empresa, mais fácil de analisar)
#   # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
#   if(teste_ger_rel){
#     print(i)
#     print(empresas_ativ[i])
#     print(empresas_ativ[[i]])
#     print(as.list(empresas_ativ[[i]]))
#   }
#   if(teste_ger_rel){
#     out_file <- sprintf("Dashs_teste/Propostas_%s", empresas_ativ[i])
#   }else{
#     out_file <- sprintf("Dashs/Propostas_%s", empresas_ativ[i])
#   }
#   parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
#                      vendedor = param_vendedor)
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
# ## Gerando dash visitas_clientes
# for(i in (1:x)){
#   rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
#                           'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
#   template <- "dash_visitas_clientes_unificada.Rmd"
#   #Teste (nome da empresa, mais fácil de analisar)
#   # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
#   if(teste_ger_rel){
#     print(i)
#     print(empresas_ativ[i])
#     print(empresas_ativ[[i]])
#     print(as.list(empresas_ativ[[i]]))
#   }
#   if(teste_ger_rel){
#     out_file <- sprintf("Dashs_teste/Visitas_Clientes_%s", empresas_ativ[i])
#   }else{
#     out_file <- sprintf("Dashs/Visitas_Clientes_%s", empresas_ativ[i])
#   }
#   parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
#                      vendedor = param_vendedor)
#
#   rmarkdown::render(template,
#                     output_file = out_file,
#                     params = parameters)
#   invisible(TRUE)
# }
#############################################################
#############################################################
## Gerar arquivo com horário de última atualização
#############################################################

# data <- paste0("Gráficos: ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
# writeLines(data, "update_time.txt")

#############################################################

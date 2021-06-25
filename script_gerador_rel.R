library(purrr)
library(data.table)
library(dplyr)

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


empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
  select(empresa_id)
params_list_i <- empresas_ativas$empresa_id
empresas_ativ <- as.list(params_list_i)
## 0 -> indica que estamos rodando para o dia atual
num_dias_list <- as.list(0)
x <- length(empresas_ativ)
debug <- T
## Parâmetro usado para gerar arquivos em pasta de teste e ver outros parâmetros de teste
teste_ger_rel <- T
## Parâmetros de vendedor
param_dash_vend <- F
## Parâmetros de mobile (ainda não usado)
param_dash_mob <- F
param_vendedor <- "vend_par" ## Caso queira testar apenas um parâmetro
param_empresa <- "emp_ar" ## Caso queira testar apenas um parâmetro
## apenas komatsu
apenas_k <- T
####################################
## Para vendedores, inicialmente teremos apenas komatsu
if(param_dash_vend && apenas_k){
  params_list_i <- 78
  empresas_ativ <- as.list(params_list_i)
}
####################################
########################################################
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
# Declaração de parâmetros gerais para knit
########################################################
dont_delete = c('empresas_ativ', 'empresas_ativas', 'vend_empresa', 'vendedores_all', 'i', 'j', 'x',
                'dont_delete', 'debug', 'teste', 'template', 'empresa', 'vendedor', 'out_f_emp',
                'nome_dash', 'num_dias_list', 'apenas_k', 'teste_ger_rel', 'out_f_vend', 'out_f',
                'template_geral', 'template_marcas', 'template_negocios', 'template_propostas',
                'template_visitas_clientes', 'param_list',
                'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')
template_geral = "dash_geral_unificada.Rmd"
template_marcas = "dash_marcas_k_unificada.Rmd"
template_negocios = "dash_negocios_unificada.Rmd"
template_propostas = "dash_propostas_unificada.Rmd"
template_visitas_clientes = "dash_visitas_clientes_unificada.Rmd"
out_f_emp = paste0(getwd(),"/Dashs/")
out_f_vend = paste0(getwd(),"/Dashs_vendedores/")
########################################################
########################################################
## i vai rodar por número de empresas
## j vai rodar por vendedores dentro da empresa
## empresas_ativ[[i]][[1]][1] -> i são as empresas ativas, posição [1][1] = primeiro [1] posição da empresa, segundo [1] valor da empresa
########################################################
i = 1
for(i in (1:length(empresas_ativ))){
  ## Gerando Geral para todos vendedores da empresa
  func_rmd_html_emp(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
                    param_dash_vend, param_dash_mob, out_f = out_f_emp,
                    nome_dash = "Geral", teste_ger_rel, num_dias_list = 0,
                    debug = debug)

  ## Gerando Marcas para todos vendedores da empresa
  func_rmd_html_emp(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
                    param_dash_vend, param_dash_mob, out_f =  out_f_emp,
                    nome_dash = "Marcas", teste_ger_rel, num_dias_list = 0,
                    debug =  debug)
  ## Gerando Negocios para todos vendedores da empresa
  func_rmd_html_emp(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
                    param_dash_vend, param_dash_mob, out_f =  out_f_emp,
                    nome_dash = "Negocios", teste_ger_rel, num_dias_list = 0,
                    debug =  debug)

  ## Gerando Propostas para todos vendedores da empresa
  func_rmd_html_emp(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
                    param_dash_vend, param_dash_mob, out_f =  out_f_emp,
                    nome_dash = "Propostas", teste_ger_rel, num_dias_list = 0,
                    debug =  debug)

  ## Gerando Visitas_clientes para todos vendedores da empresa
  func_rmd_html_emp(dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
                    param_dash_vend, param_dash_mob, out_f =  out_f_emp,
                    nome_dash = "Visitas_clientes", teste_ger_rel, num_dias_list = 0,
                    debug =  debug)
}
##Gerando dashs geral ##Funcionando, apenas comentada pra facilitar teste das marcas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
                          'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
  template <- "dash_geral_unificada.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  print(teste_ger_rel)
  if(teste_ger_rel){
    print(i)
    print(empresas_ativ[i])
    print(empresas_ativ[[i]])
    print(as.list(empresas_ativ[[i]]))
  }
  if(teste_ger_rel){
    out_file <- sprintf("Dashs_teste/Geral_%s", empresas_ativ[i])
  }else{
    out_file <- sprintf("Dashs/Geral_%s", empresas_ativ[i])
  }
  if(teste_ger_rel){
    print(out_file)
  }
  parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
                     vendedor = param_vendedor)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
##Gerando dashs marcas
for(i in (1:x)){
  if (empresas_ativ[i] != 78 & empresas_ativ[i] != 79)
  {
    rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
                            'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
    template <- "dash_marcas_unificada.Rmd"
    #Teste (nome da empresa, mais fácil de analisar)
    # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
    if(teste_ger_rel){
      print(i)
      print(empresas_ativ[i])
      print(empresas_ativ[[i]])
      print(as.list(empresas_ativ[[i]]))
    }
    if(teste_ger_rel){
      out_file <- sprintf("Dashs_teste/Marcas_%s", empresas_ativ[i])
    }else{
      out_file <- sprintf("Dashs/Marcas_%s", empresas_ativ[i])
    }
    parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
                       vendedor = param_vendedor)

    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }else{ ##########Caso seja a komatsu, marcas diferentes
    template <- "dash_marcas_k_unificada.Rmd"
    if(teste_ger_rel){
      out_file <- sprintf("Dashs_teste/Marcas_%s", empresas_ativ[i])
    }else{
      out_file <- sprintf("Dashs/Marcas_%s", empresas_ativ[i])
    }
    parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
                       vendedor = param_vendedor)

    rmarkdown::render(template,
                      output_file = out_file,
                      params = parameters)
    invisible(TRUE)
  }
}
# ## Gerando dash negocios
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
                          'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
  template <- "dash_negocios_unificada.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste_ger_rel){
    print(i)
    print(empresas_ativ[i])
    print(empresas_ativ[[i]])
    print(as.list(empresas_ativ[[i]]))
  }
  if(teste_ger_rel){
    out_file <- sprintf("Dashs_teste/Negocios_%s", empresas_ativ[i])
  }else{
    out_file <- sprintf("Dashs/Negocios_%s", empresas_ativ[i])
  }
  parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
                     vendedor = param_vendedor)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash propostas
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
                          'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
  template <- "dash_propostas_unificada.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste_ger_rel){
    print(i)
    print(empresas_ativ[i])
    print(empresas_ativ[[i]])
    print(as.list(empresas_ativ[[i]]))
  }
  if(teste_ger_rel){
    out_file <- sprintf("Dashs_teste/Propostas_%s", empresas_ativ[i])
  }else{
    out_file <- sprintf("Dashs/Propostas_%s", empresas_ativ[i])
  }
  parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
                     vendedor = param_vendedor)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
## Gerando dash visitas_clientes
for(i in (1:x)){
  rm(list=setdiff(ls(), c("params_list_i", "empresas_ativ", "i", 'x', 'teste_ger_rel',
                          'param_dash_vend', 'param_dash_mob', 'param_vendedor', 'param_empresa')))
  template <- "dash_visitas_clientes_unificada.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  if(teste_ger_rel){
    print(i)
    print(empresas_ativ[i])
    print(empresas_ativ[[i]])
    print(as.list(empresas_ativ[[i]]))
  }
  if(teste_ger_rel){
    out_file <- sprintf("Dashs_teste/Visitas_Clientes_%s", empresas_ativ[i])
  }else{
    out_file <- sprintf("Dashs/Visitas_Clientes_%s", empresas_ativ[i])
  }
  parameters <- list(empresa = empresas_ativ[i], dash_vend = param_dash_vend, dash_mob = param_dash_mob,
                     vendedor = param_vendedor)

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

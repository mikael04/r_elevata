rm(list = ls())
library(purrr)
library(data.table)
library(dplyr)
source("Vendedor/func_rmd_html.R")

debug = T

# geral
# marcas
# marcas_k
# negocios
# propostas
# visitas_clientes

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
if (debug){
  for (i in (1:length(empresas_ativ))){
    if(length(empresas_ativ[[i]][[2]]) >0){
      print(paste0("i == ", i))
      print(paste0("empresa == ", empresas_ativ[[i]][[1]][1]))
      print(paste0("vendedor == ", empresas_ativ[[i]][[2]]))
    }
  }
}
###########################################
# Declaração de parâmetros gerais para knit
###########################################
dont_delete = c('empresas_ativ', 'empresas_ativas', 'vend_empresa', 'vendedores_all', 'i', 'j',
                'dont_delete', 'debug', 'teste', 'template', 'empresa', 'vendedor', 'out_f',
                'nome_dash', 'num_dias_list',
                'template_geral', 'template_marcas', 'template_negocios', 'template_propostas',
                'template_visitas_clientes')
template_geral = "Vendedor/dash_geral_vendedor.Rmd"
template_marcas = "Vendedor/dash_marcas_k_vendedor.Rmd"
template_negocios = "Vendedor/dash_negocios_vendedor.Rmd"
template_propostas = "Vendedor/dash_propostas_vendedor.Rmd"
template_visitas_clientes = "Vendedor/dash_visitas_clientes_vendedor.Rmd"
out_f = "/mnt/dados/Mikael/Projetos/Scripts_R/r_elevata/Dashs_vendedores/"
###########################################
## i vai rodar por número de empresas
## j vai rodar por vendedores dentro da empresa
# for(i in (1:length(empresas_ativ))){
  i = 33
  for(j in (1:length(empresas_ativ[[i]][[2]]))){
    if(length(empresas_ativ[[i]][[2]]) > 0){
      ## Gerando geral para todos vendedores da empresa
      func_rmd_html (dont_delete = dont_delete, template = template_geral, empresa = empresas_ativ[[i]][[1]],
                     vendedor = empresas_ativ[[i]][[2]][j], out_f =  out_f, nome_dash = "Geral", debug =  debug,
                     num_dias_list = 0)
      ## Gerando marcas para todos vendedores da empresa
      func_rmd_html (dont_delete = dont_delete, template = template_marcas, empresa = empresas_ativ[[i]][[1]],
                     vendedor = empresas_ativ[[i]][[2]][j], out_f =  out_f, nome_dash = "Marcas", debug =  debug,
                     num_dias_list = 0)
      ## Gerando negocios para todos vendedores da empresa
      func_rmd_html (dont_delete = dont_delete, template = template_negocios, empresa = empresas_ativ[[i]][[1]],
                     vendedor = empresas_ativ[[i]][[2]][j], out_f =  out_f, nome_dash = "Negocios", debug =  debug,
                     num_dias_list = 0)
      ## Gerando propostas para todos vendedores da empresa
      func_rmd_html (dont_delete = dont_delete, template = template_propostas, empresa = empresas_ativ[[i]][[1]],
                     vendedor = empresas_ativ[[i]][[2]][j], out_f =  out_f, nome_dash = "Propostas", debug =  debug,
                     num_dias_list = 0)
      ## Gerando visitas_clientes para todos vendedores da empresa
      func_rmd_html (dont_delete = dont_delete, template = template_visitas_clientes, empresa = empresas_ativ[[i]][[1]],
                     vendedor = empresas_ativ[[i]][[2]][j], out_f =  out_f, nome_dash = "Visitas_clientes", debug =  debug,
                     num_dias_list = 0)
    }
  }
# }

# #############################################################
# ## Gerar arquivo com horário de última atualização
# #############################################################
#
# # data <- paste0("Gráficos: ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
# # writeLines(data, "update_time.txt")
#
# #############################################################

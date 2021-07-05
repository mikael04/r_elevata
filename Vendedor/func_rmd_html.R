library(purrr)
## Parâmetros recebidos
## 1 - Itens do environment que não serão apagados
## 2 - template usado (geral, marcas, marcas_k, negocios, propostas, visitas_clientes)
## 3 - código de empresa, vem de empresas_ativ[[i]][[1]]
## 4 - vendedor da empresa, vem de empresas_ativ[[i]][[2]][j]
## 5 - outfile, arquivo de saída (caminho gerado por pedaço do nome, outfile, + código de empresa + código de vendedor)
## 6 - nome da dash
## 7 - debug, para testes
############################
##Teste
# dont_delete = c('empresas_ativ', 'empresas_ativas', 'vend_empresa', 'vendedores_all', 'i',
#                 'dont_delete', 'debug', 'teste', 'template', 'empresa', 'vendedor', 'out_f',
#                 'nome_dash', 'num_dias_list')
# template = "Vendedor/dash_geral_vendedor.Rmd"
# empresa = 78
# vendedor = 1058
# out_f = "/mnt/dados/Mikael/Projetos/Scripts_R/r_elevata/Dashs_vendedores/"
# nome_dash = "Geral"
# debug = TRUE
# num_dias_list = 0
############################

func_rmd_html <- function(dont_delete, template, empresa, vendedor, out_f, nome_dash , debug, num_dias_list){
  rm(list=setdiff(ls(), dont_delete))
  template_ <- template
  if(debug == T){
    print(paste0("empresa = ", empresa))
    print(paste0("vendedor = ", vendedor))
    #print(params_list[[i]])
    #print(as.list(params_list[[i]]))
  }
  ## Criando o diretório caso não exista
  dir.create(paste0(out_f, empresa, "/"))
  ## Arquivo de saída
  out_file <- paste0(out_f, empresa, "/", nome_dash, "_", vendedor)
  parameters <- list(empresa = empresa, vendedor = vendedor, num_dias = num_dias_list)
  rmarkdown::render(template_,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)

}


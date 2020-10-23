library(purrr)
library(tictoc)

#tictoc::tic("")
setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
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
  
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Negocios_Propostas_%s, var1)
  out_file <- sprintf("Dashs/Geral_%s", convert_str_emp(var1))
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_mar <- function(var1) {
  
  template <- "dash_marcas.Rmd"
  #Teste (nome da empresa, mais fácil de analisar)
  # out_file <- sprintf("Dashs/Mapas_%s", var1)
  out_file <- sprintf("Dashs/Marcas_%s", convert_str_emp(var1))
  
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
  out_file <- sprintf("Dashs/Marcas_%s", convert_str_emp(var1))
  
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
  out_file <- sprintf("Dashs/Negocios_%s", convert_str_emp(var1))
  
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
  out_file <- sprintf("Dashs/Propostas_%s", convert_str_emp(var1))
  
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
  out_file <- sprintf("Dashs/Visitas_Clientes_%s", convert_str_emp(var1))

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

convert_str_emp <- function(str) {
  empresa <- 0
  if(str == 'Super'){
    empresa <- emp_su
  }else{
    if(str == 'Komatsu'){
      empresa <- emp_ko
    }else{
      if (str == 'Amazonia'){
        empresa = emp_am
      }else{
        if(str == 'Araguaia'){
          empresa = emp_ar
        }else{
          if(str == 'MS'){
            empresa = emp_ms
          }else{
            if(str == 'Simex'){
              empresa = emp_si
            }else{
              if(str == 'Taisa'){
                empresa = emp_ta
              }
            }
          }
        }
      }
    }
  }
  return (empresa)
}

##Variável "Global"
emp_am = 42 # Amazonia
emp_ar = 77 # Araguaia
emp_ko = 78 # Komatsu
emp_ms = 35 # Ms
emp_si = 59 # Simex
emp_su = 16 # Super
emp_ta = 60 # Taisa

##Parâmetros em ordem alfabética, uma única lista, fazendo um por um já que alguns não são feitos para todas 
params_list <- list(list("Amazonia"),list("Araguaia"),list("Komatsu"),list("MS"),list("Simex"),list("Super"),list("Taisa"))

#Gerando relatórios geral, todas
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
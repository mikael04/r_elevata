library(purrr)
library(tictoc)

tictoc::tic("")
setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas_mapas

render_report_neg_prop_csv <- function(var1) {
  
  template <- "dash_negocios_propostas_csv.Rmd"
  
  out_file <- sprintf("Dashs/Negocios_Propostas_%s", var1)
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_vis_map_csv <- function(var1) {

  template <- "dash_visitas_mapas_csv.Rmd"

  out_file <- sprintf("Dashs/Visitas_Mapas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
render_report_vis_map_k_csv <- function(var1) {

  template <- "dash_visitas_mapas_k_csv.Rmd"

  out_file <- sprintf("Dashs/Visitas_Mapas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_neg_vis_csv <- function(var1) {
  
  template <- "dash_negocios_visitas_csv.Rmd"
  
  out_file <- sprintf("Dashs/Negocios_Visitas_%s", var1)
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}


render_report_map_csv <- function(var1) {
  
  template <- "dash_mapas_csv.Rmd"
  
  out_file <- sprintf("Dashs/Mapas_%s", var1)
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

##Gerando das que tem propostas (duas dashs separadas, a primeira negócios+propostas, segunda visitas+clientes)
#params_list_1 <- list(list("Super","Komatsu"))
params_list_1 <- list(list("Super"),list("Komatsu"))
#params_list_1 <- list(list("Super","Komatsu")) ##testar apenas a visitas_mapa

##negocios_proposta Super e Komatsu
pmap(params_list_1[[1]], render_report_neg_prop_csv)
pmap(params_list_1[[2]], render_report_neg_prop_csv)

##visitas_mapas Super e Komatsu
pmap(params_list_1[[1]], render_report_vis_map_csv)
pmap(params_list_1[[2]], render_report_vis_map_k_csv)


#############################################################

##Gerando das que não tem propostas (uma dash, negocios+visitas+clientes)
params_list_2 <- list(list("Amazonia","Araguaia", "MS", "Simex", "Taisa"))

##negocios_visitas "Amazonia","Araguaia", "MS", "Simex", "Taisa"
pmap(params_list_2, render_report_neg_vis_csv)

##negocios_visitas "Amazonia","Araguaia", "MS", "Simex", "Taisa"
pmap(params_list_2, render_report_map_csv)

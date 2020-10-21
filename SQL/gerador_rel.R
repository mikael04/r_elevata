library(purrr)
library(tictoc)

tictoc::tic("")
setwd("E:\\Mikael\\OneDrive\\Projetos\\Scripts_R\\r_elevata")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
##Usado para super e komatsu
##renderiza dash_negocios_propostas e dash_visitas
render_report_neg_prop <- function(var1) {

  template <- "dash_negocios_propostas.Rmd"

  out_file <- sprintf("Dashs/Negocios_Propostas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_neg_prop_csv <- function(var1) {
  
  template <- "dash_negocios_propostas_csv.Rmd"
  
  out_file <- sprintf("Dashs/Negocios_Propostas_%s", var1)
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_vis_map <- function(var1) {

  template <- "dash_visitas_mapas.Rmd"

  out_file <- sprintf("Dashs/Visitas_Mapas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
render_report_vis_map_k <- function(var1) {

  template <- "dash_visitas_mapas_k.Rmd"

  out_file <- sprintf("Dashs/Visitas_Mapas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
render_report_vis_map_k <- function(var1) {

  template <- "dash_visitas_mapas_k.Rmd"

  out_file <- sprintf("Dashs/Visitas_Mapas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
render_report_vis_map_k <- function(var1) {
  
  template <- "dash_visitas_mapas_k.Rmd"
  
  out_file <- sprintf("Dashs/Visitas_Mapas_%s", var1)
  
  parameters <- list(variable1 = var1)
  
  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}

render_report_neg_vis <- function(var1) {

  template <- "dash_negocios_visitas.Rmd"

  out_file <- sprintf("Dashs/Negocios_Visitas_%s", var1)

  parameters <- list(variable1 = var1)

  rmarkdown::render(template,
                    output_file = out_file,
                    params = parameters)
  invisible(TRUE)
}
render_report_map <- function(var1) {
  
  template <- "dash_mapas.Rmd"
  
  out_file <- sprintf("Dashs/Mapas_%s", var1)
  
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

# tic("SQL neg_prop")
pmap(params_list_1[[1]], render_report_neg_prop)
pmap(params_list_1[[2]], render_report_neg_prop)
# sql_neg_prop <- toc()
#pmap(params_list_1, render_report_vis_map)
##Um para cada, já que a komatsu vai mostrar apenas uma categoria enquanto a super divide em tratores e colheitadeiras
pmap(params_list_1[[1]], render_report_vis_map)
pmap(params_list_1[[2]], render_report_vis_map_k)
# toc()
# toc()

#############################################################
##Teste dos csvs
# tic("csv neg_prop")
pmap(params_list_1[[1]], render_report_neg_prop_csv)
pmap(params_list_1[[2]], render_report_neg_prop_csv)
# csv_neg_prop <- toc()
# tic("csv vis_map") 
# pmap(params_list_1[[1]], render_report_vis_map_csv)
# pmap(params_list_1[[2]], render_report_vis_map_k_csv)
# toc()
# toc()
#############################################################

##Gerando das que não tem propostas (uma dash, negocios+visitas+clientes)
params_list_2 <- list(list("Amazonia","Araguaia", "MS", "Simex", "Taisa"))


pmap(params_list_2, render_report_neg_vis)

# tic("SQL maps")
pmap(params_list_2, render_report_map)
# sql_maps <- toc()

#testando csvs
# tic("csv maps")
#pmap(params_list_2, render_report_map_csv)
# csv_maps <- toc()
# 
# sql_maps
# 
# csv_maps

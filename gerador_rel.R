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

render_report_vis_map <- function(var1) {
  
  template <- "dash_visitas_mapas.Rmd"
  
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

library(purrr)
##Gerando das que tem propostas (duas dashs separadas, a primeira negócios+propostas, segunda visitas+clientes)
params_list_1 <- list(list("Super"))
#params_list_1 <- list(list("Super","Komatsu")) ##testar apenas a visitas_mapa

pmap(params_list_1, render_report_neg_prop)
pmap(params_list_1, render_report_vis_map)

##Gerando das que não tem propostas (uma dash, negocios+visitas+clientes)
params_list_2 <- list(list("Amazonia","Araguaia", "MS", "Simex", "Taisa"))

pmap(params_list_2, render_report_neg_vis)
pmap(params_list_2, render_report_map)

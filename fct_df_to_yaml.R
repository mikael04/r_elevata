## Função para ler txt e converter para yaml
## Usada para ícones txt, já retorna em formato de lista (pode ser feito uma segunda func
## para gerar apenas formato yaml para ser salvo, teria apenas que remover o yaml.load e substituir
## pela função de escrita write_yaml)

func_conv_txt_to_yaml <- function (df_to_yaml){
  yaml_list <- df_to_yaml %>%
    split(by = "marca_id_i") %>%
    lapply(function(x) x[,marca_id_i := NULL] %>% .[]) %>%
    yaml::as.yaml() %>%
    yaml::yaml.load()
  return(yaml_list)
}

#df_to_yaml <- fread("Icons/marcas_icon_txt.txt")
func_conv_txt_to_yaml_icon <- function (df_to_yaml){
  yaml_list <- df_to_yaml %>%
    #dplyr::select(-marca, -cor) %>%
    split(by = "marca_id_i") %>%
    lapply(function(x) x[,marca_id_i := NULL] %>% .[]) %>%
    yaml::as.yaml() %>%
    yaml::yaml.load()
  return(yaml_list)
}

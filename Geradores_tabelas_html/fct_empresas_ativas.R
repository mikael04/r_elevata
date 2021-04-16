fct_empresas_ativas <- function(){
  empresas_ativas <- fread("Tabelas/empresas_ativas_id.csv") %>%
    select(empresa_id)
  params_list_i <- empresas_ativas$empresa_id
  params_list <- as.list(params_list_i)
  ## 0 -> indica que estamos rodando para o dia atual
  return(params_list)
}

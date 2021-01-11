library(lubridate)

fct_ano_atual <- function() {
  return (lubridate::ymd(lubridate::today()-months(lubridate::month(lubridate::today())-1)- days(day(lubridate::today())-1)))
  
}
fct_mes_atual <- function() {
  return(lubridate::ymd(lubridate::today()-days(day(lubridate::today())-1)))
}
fct_mes_ant <- function(mes_atual) {
  if(mes_atual > 1){
    return(lubridate::ymd(mes_atual-months(1)))
  }else{
    return(lubridate::ymd(mes_atual-months(1)))
  }
}
## Função para gerar meses anteriores, para plots de faturamento, visitas, clientes
fct_meses_ant <- function(mes_atual) {
  mes_at <- lubridate::month(mes_atual)
  if(mes_at == lubridate::month(1)){
    seq(1, 13-1, 1)
  }else{
    seq(1, mes_at, 1)
  }
}

fct_data_num_meses <- function(mes_atual){
  lubridate::month(mes_atual)
}
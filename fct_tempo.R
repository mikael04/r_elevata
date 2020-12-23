library(lubridate)

fct_ano_atual <- function() {
  return (ymd(today()-months(month(today())-1)- days(day(today())-1)))
  
}
fct_mes_atual <- function() {
  return(ymd(today()-days(day(today())-1)))
}
fct_mes_ant <- function(mes_atual) {
  if(mes_atual > 1){
    return(ymd(mes_atual-months(1)))
  }else{
    return(ymd(mes_atual-months(1)))
  }
}
## Função para gerar meses anteriores, para plots de faturamento, visitas, clientes
fct_meses_ant <- function(mes_atual) {
  mes_at <- month(mes_atual)
  if(mes_at == month(1)){
    seq(1, 13-1, 1)
  }else{
    seq(1, mes_at, 1)
  }
}

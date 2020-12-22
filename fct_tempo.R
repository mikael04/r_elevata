library(lubridate)

fct_ano_atual <- function() {
  return (ymd(today()-months(month(today())-1)- days(day(today())-1)))
  
}
fct_mes_atual <- function() {
  return(month(today()))
}
fct_mes_ant <- function() {
  mes_atual <- fct_mes_atual()
  if(mes_atual > 1){
    return(mes_atual-1)
  }else{
    return(mes_atual<-12)
  }
}
## Função para gerar meses anteriores, para plots de faturamento, visitas, clientes
fct_meses_ant <- function(mes_atual) {
  if(mes_atual == month(1)){
    seq(1, 13-1, 1)
  }else{
    seq(1, mes_atual, 1)
  }
}

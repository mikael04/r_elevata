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

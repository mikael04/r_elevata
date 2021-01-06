func_fmt_din_ <- function(inteiro){
  inteiro_em_reais <- paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  inteiro_em_reais
}

##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) -> posteriormente adicionar o "mi"
func_fmt_din_mil <- function(inteiro){
  inteiro <- round(inteiro/1000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)))
  inteiro_mi_em_reais
}
##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) com o "mi"
func_fmt_din_mil_ <- function(inteiro){
  inteiro <- round(inteiro/1000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)), 'mil')
  inteiro_mi_em_reais
}

##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) -> posteriormente adicionar o "mi"
func_fmt_din_milhoes <- function(inteiro){
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)))
  inteiro_mi_em_reais
}

##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) com o "mi"
func_fmt_din_milhoes_ <- function(inteiro){
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)), 'mi')
  inteiro_mi_em_reais
}

func_fmt_din <- function(inteiro){
  if(inteiro <= 0){
    "R$0"
  }else{
    if(inteiro < 1000){
      func_fmt_din_(inteiro)
    }else{
      if(inteiro < 1000000){
        func_fmt_din_mil(inteiro)
      }else{
        func_fmt_din_milhoes(inteiro)
      }
    }
  }
}
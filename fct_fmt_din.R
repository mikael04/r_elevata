func_fmt_din_ <- function(inteiro){
  inteiro_em_reais <- paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 2))
  inteiro_em_reais
}

##Alterar o valor de inteiro para reais
func_fmt_din_mil <- function(inteiro){
  inteiro <- round(inteiro/1000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)))
  inteiro_mi_em_reais
}
##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) com o "mi"
func_fmt_din_mil_ <- function(inteiro){
  inteiro_mi_em_reais <- paste(func_fmt_din_mil(inteiro), 'mil')
  inteiro_mi_em_reais
}

##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) -> posteriormente adicionar o "mi"
func_fmt_din_milhoes <- function(inteiro){
  inteiro <- round(inteiro/1000000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)))
  inteiro_mi_em_reais
}

##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) com o "milhões"
func_fmt_din_milhoes_ <- function(inteiro){
  inteiro_mi_em_reais <- paste(func_fmt_din_milhoes(inteiro), 'milhões')
  inteiro_mi_em_reais
}
##Alterar o valor de inteiro para reais convertendo para milhões (78000000 = R$78,0) com o "mi"
func_fmt_din_milhoes_mi <- function(inteiro){
  inteiro_mi_em_reais <- paste(func_fmt_din_milhoes(inteiro), 'mi')
  inteiro_mi_em_reais
}
##Alterar o valor de inteiro para reais convertendo para bilhões (78000000 = R$78,0) para método com "adicional" usar vriações abaixo
func_fmt_din_bilhoes <- function(inteiro){
  inteiro <- round(inteiro/1000000000, digits = 1)
  inteiro_mi_em_reais <- paste(paste0("R$", format(inteiro, decimal.mark = ",", big.mark = ".", nsmall = 1)))
  inteiro_mi_em_reais
}

##Alterar o valor de inteiro para reais convertendo para bilhões (78000000 = R$78,0) com o "bilhões"
func_fmt_din_bilhoes_ <- function(inteiro){
  inteiro_mi_em_reais <- paste(func_fmt_din_bilhoes(inteiro), 'bilhões')
  inteiro_mi_em_reais
}
##Alterar o valor de inteiro para reais convertendo para bilhões (78000000 = R$78,0) com o "mi"
func_fmt_din_bilhoes_mi <- function(inteiro){
  inteiro_mi_em_reais <- paste(func_fmt_din_bilhoes(inteiro), 'bi')
  inteiro_mi_em_reais
}


func_fmt_din <- function(inteiro){
  if(inteiro <= 0 | is.na(inteiro)){
    "R$0"
  }else{
    if(inteiro < 1000){
      func_fmt_din_(inteiro)
    }else{
      if(inteiro < 1000000){
        func_fmt_din_mil_(inteiro)
      }else{
        if(inteiro < 1000000000){
          func_fmt_din_milhoes_(inteiro)
        }else{
          func_fmt_din_bilhoes_(inteiro)
        }
      }
    }
  }
}

func_fmt_din_small <- function(inteiro){
  if(inteiro <= 0 | is.na(inteiro)){
    "R$0"
  }else{
    if(inteiro < 1000){
      func_fmt_din_(inteiro)
    }else{
      if(inteiro < 1000000){
        func_fmt_din_mil_(inteiro)
      }else{
        if(inteiro < 1000000000){
          func_fmt_din_milhoes_mi(inteiro)
        }else{
          func_fmt_din_bilhoes_mi(inteiro)
        }
      }
    }
  }
}

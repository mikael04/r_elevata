##Alterar o nome completo pra primeiro nome mais iniciais dos sobrenomes
func_nome <- function (nome_comp)
{
  lista <- stringr::str_split_fixed(nome_comp, " ", 4)
  lista <- lista[, -4]
  lista[,3] = stringr::str_sub(lista[,3], 1, 1)
  lista[,2] = stringr::str_sub(lista[,2], 1, 1)
  lista[,2] = paste(lista[,2], '.', sep='')
  lista[,3] = paste(lista[,3], '.', sep='')
  lista[,2] <- gsub('^.$', '',lista[,2])
  lista[,3] <- gsub('^.$', '',lista[,3])
  lista[,1] <- paste(lista[,1], lista[,2], lista[,3], sep=' ')
  return (lista[,1])
}
## Alterar o nome completo pra primeiro nome mais iniciais dos sobrenomes
## Ex: MIKAEL MARIN COLETTO -> MIKAEL M. C.
func_nome <- function (nome_comp)
{
  ## Separa os nomes por espaço
  lista <- stringr::str_split_fixed(nome_comp, " ", 4)
  ## Remove nomes após terceiro
  lista <- lista[,-4]
  ## Caso tenha 3 nomes ()
  if(length(lista) >= 3){
    lista[3] = stringr::str_sub(lista[3], 1, 1)
    lista[3] = paste(lista[3], '.', sep='') 
    lista[3] <- gsub('^.$', '',lista[3])
  }
  ## Caso tenha 2 nomes ()
  if(length(lista) >= 2){
    lista[2] = stringr::str_sub(lista[2], 1, 1)
    lista[2] = paste(lista[2], '.', sep='') 
    lista[2] <- gsub('^.$', '',lista[2])
  }
  lista[1] <- paste(lista[1], lista[2], lista[3], sep=' ')
  lista <- lista[c(-2, -3)]
  return (lista[1])
}
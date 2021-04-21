######################################################
##                                                  ##
##  Área       : B3                                 ##
##  Projeto    : Geral                              ##
##  Subprojeto : Base                               ##
##                                                  ##
######################################################
##                                                  ##
##  Módulo     : Formatos                           ##
##  Submódulo  :                                    ##
##  Autor      : Tiago Kratka                       ##
##                                                  ##
######################################################
##                                                  ##
##  Inicial : 20/02/2021                            ##
##  Final   : 20/02/2021                            ##
##  R       : 4.0.4                                 ##
##                                                  ##
######################################################


##################################
## Funções para Formatação

formatoCientifico <- function(l) {

  l <- format(l, scientific = TRUE) # turn in to character string in scientific notation
  l <- gsub("^(.*)e", "'\\1'e", l)  # quote the part before the exponent to keep all the digits
  l <- gsub("e", "%*%10^", l)       # turn the 'e+' into plotmath format
  parse(text=l)                     # return this as an expression

}

formatoBRNum = function(n){

  n = format(n, big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE)

  return(n)

}

formatoBRInt = function(n){

  n = format(n, big.mark = ".", decimal.mark = ",", nsmall = 0, scientific = FALSE)

  return(n)

}

formatoBRPorcentagem = function(n, digitos = 2){

  precisao <- 10^-digitos

  n = scales::percent(n, accuracy = precisao, big.mark = ".", decimal.mark = ",")

  return(n)

}



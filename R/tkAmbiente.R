######################################################
##                                                  ##
##  Pacote Base                                     ##
##  Autor: Tiago Kratka                             ##
##  Versao final: 03/04/2020                        ##
##  R version 3.6.3                                 ##
##                                                  ##
######################################################

preparaAmbiente = function (dirProjetos, nomeProjeto, moduloProjeto){

  # Limpar o console
  rm(list = ls())

  # Para problemas de memoria envolvendo R, Java e Excel
  options(java.parameters = "-Xmx14g")

  # Mudar Diretório de Trabalho
  setwd(paste(dirProjetos, nomeProjeto, moduloProjeto, sep = "/"))

  # Carregar .RData
  load(".RData")

}

carregaPacotes = function(pacotes) {

  # Verificando se os pacotes estao instalados
  novosPacotes = pacotes[!(pacotes %in% installed.packages()[,"Package"])]
  if(length(novosPacotes)) install.packages(novosPacotes, quiet = TRUE)

  # Carregando pacotes
  invisible(lapply(pacotes, library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

}

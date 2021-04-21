######################################################
##                                                  ##
##  Pacote Base                                     ##
##  Autor: Tiago Kratka                             ##
##  Versao final: 03/04/2020                        ##
##  R version 3.6.3                                 ##
##                                                  ##
######################################################

limpaString  = function(string){

  if(!is.character(string))
    str = as.character(string)

  #Removendo pontuação
  string = gsub("[[:punct:]]", "", string)
  # Caracteres indesejados
  unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  # Opção 1: utilizando biblioteca gsubfn
  string = gsubfn::gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,string)

  #Opção 2: utliziando função iconv
  #string = iconv(string, to='ASCII//TRANSLIT')

  # Opção 3: utilizando função chartr
  #string = chartr(paste(names(unwanted_array), collapse=''),
  #                paste(unwanted_array, collapse=''),
  #                string)

  # Convertendo para Maiúsculas
  string = toupper(string)

  return(string)
}

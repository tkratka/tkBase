######################################################
##                                                  ##
##  Pacote Base / Manipulação de Arquivos           ##
##  Autor: Tiago Kratka                             ##
##  Versao final: 03/04/2020                        ##
##  R version 4.0.2                                 ##
##                                                  ##
######################################################


# Verifica se o arquivo esta aberto para salvar os dados
salvaArquivo = function(dados, arq, tipo = "excel_csv2", tentativas = 9, tempoEspera = 0, adiciona = TRUE, com_cabecalho = !adiciona) {

  msgResult = paste0("Salvando arquivo: ", arq, "\n")
  if(nrow(dados) > 500000) { tipo = "big_data" } # Se df > 500k linhas, usa biblioteca data.table

  # Se o arquivo nao existir, escrever com  cabecalho
  if(!file.exists(arq)) { com_cabecalho = TRUE }

  for(t in 1:tentativas) {


    # Verifica se o arquivo (!)NÃO esta aberto
    podeSalvar = !suppressWarnings(
      "try-error" %in% class(
        try(connFile <- file(arq, open = ifelse(adiciona, "a", "w")), silent = TRUE)))
    if(podeSalvar){

      close(connFile) # Fecha a conexão com o arquivo, que foi aberta no TRY de podeSalvar
      msgResult = paste0("Arquivo ", arq, " salvo com sucesso.\n")

      switch (tipo,

              "csv"          = readr::write_csv(dados, arq, append = adiciona, col_names = com_cabecalho),
              "csv2"         = readr::write_csv2(dados, arq, append = adiciona, col_names = com_cabecalho),
              "excel_csv2"   = readr::write_excel_csv2(dados, arq, append = adiciona, col_names = com_cabecalho),
              "excel_csv"    = readr::write_excel_csv(dados, arq, append = adiciona, col_names = com_cabecalho),
              "big_data"     = data.table::fwrite(dados, arq, append = adiciona, col.names = com_cabecalho),
              { # Se nenhuma das extensoes disponiveis
                msgResult = paste0("Erro ao salvar dados. Arquivo ", arq, " nao possui extensão valida\n")
              }

      )
      break
    }

    tempoEspera = tempoEspera + t
    Sys.sleep(tempoEspera)           # Espera um pouco para ver se o outro processo desocupa o arquivo
    msgResult = paste0("Tentativa ", t, " de salvar o arquivo ", arq, " sem sucesso\n")
  }

  cat(msgResult)

}

# Escrita de arquivo .xlsx ou .xlsm
atualizaPlanilha = function(df, arq, plan) {

  wb = xlsx::loadWorkbook(arq)
  xlsx::removeSheet(wb, sheetName=plan)
  objPlanilha = xlsx::createSheet(wb, plan)
  #addDataFrame(df, sheet=objPlanilha, startColumn=1, row.names=FALSE)
  csR <- xlsx::CellStyle(wb) +
         xlsx::Font(wb, isItalic=TRUE, color = "blue") # rowcolumns
  #cs2 <- CellStyle(wb, dataFormat = DataFormat("#,##"))
  csN <- xlsx::CellStyle(wb, dataFormat = DataFormat("#,##0.00"))
  csH <- xlsx::CellStyle(wb)         +
         xlsx::Font(wb, isBold=TRUE) +
         xlsx::Border() # header
  xlsx::addDataFrame(df, sheet=objPlanilha, startColumn=1, row.names=FALSE,
               colnamesStyle=csH,
               rownamesStyle=csR, colStyle=list('8'=csN , '9'=csN , '10'=csN,
                                                '11'=csN, '12'=csN, '13'=csN,
                                                '14'=csN, '15'=csN, '16'=csN))

  #autoSizeColumn(objPlanilha, colIndex=1:ncol(df))
  options(xlsx.date.format="dd/MM/yyyy")
  xlsx::saveWorkbook(wb, arq)
  cat("\n ..... Criado arquivo ", arq, "com ", length(df[,1]), "linhas")

}

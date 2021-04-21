######################################################
##                                                  ##
##  Área       : B3                                 ##
##  Projeto    : Geral                              ##
##  Subprojeto : Base                               ##
##                                                  ##
######################################################
##                                                  ##
##  Módulo     : Calculos                           ##
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

##############################
## Estatisticas Quant

calculaEstatisticasBase = function(df){

  probs = c(0.00, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1.0)

  media   <- mean(df, na.rm = TRUE)                                    # media
  mediana <- stats::median(df, na.rm = TRUE)                                  # mediana
  sd      <- stats::sd(df, na.rm = TRUE)                                      # DesvioPadrao
  quantis <- stats::quantile(df, probs = probs, na.rm = TRUE, names = TRUE)   # Quantis

  stats = c(sd, mediana, media, quantis)

  return(stats)

}

# Cria um DF com as estatisticas basicas para cada coluna numerica
calculaMedidas = function(df, periodo = nrow(df)){

  dfStatsP <- df                         %>%
              dplyr::select(where(is.numeric))  %>%  # Seleciona apenas variaveis numericas
              dplyr::slice_head(n   = periodo)

  # DF deve vir com apenas 1 atributo e 1 agrupameto ja' pronto
  dfStats  <- dfStatsP                %>%
              tidyr::drop_na()         %>%
              dplyr::group_modify(~ {

                  .x                                       %>%
                  purrr::map_dfc(calculaEstatisticasBase)  %>%
                  dplyr::mutate(Estatistica = c("SD", "Mediana", "Media", "Min", "1%", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "99%", "Max"))

              }) %>%

              dplyr::relocate(last_col(), .after = 1)  # Coluna com nome das Estatistica na 2a posicao

  return(dfStats)

}

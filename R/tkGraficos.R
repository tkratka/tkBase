######################################################
##                                                  ##
##  Área       : B3                                 ##
##  Projeto    : Geral                              ##
##  Subprojeto : Base                               ##
##                                                  ##
######################################################
##                                                  ##
##  Módulo     : Graficos                           ##
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

###############
## Graficos

graficoHistograma = function(plotBase, X){


    plotBase                                                 +
    ggplot2::geom_histogram(alpha = 0.3)                              +
    ggplot2::scale_y_continuous(labels = formatoCientifico)            +
    #scale_x_continuous(labels = formatoBRNumQtde)            +
    ggplot2::labs(title = paste0("Histograma de ", X),
         y     = "Frequencia")
}


graficoDensidade = function(plotBase, X){

    plotBase                                                 +
    ggplot2::geom_density(alpha = 0.3)                                +
    ggplot2::scale_y_continuous(labels = formatoCientifico)            +
    ggplot2::scale_x_continuous(labels = formatoBRNumQtde)            +

    ggplot2::labs(title = paste0("Densidade de ", X),
         y = "Densidade")

}

graficoBoxplot = function(plotBase, X, Y){

    plotBase                                                 +
    ggplot2::aes_string(y = Y)                                        +
    ggplot2::geom_boxplot(alpha = 0.3,
                 #outlier.colour="black",
                 outlier.shape = 16,
                 outlier.size  = 4,
                 notch         = TRUE
                 )                                           +

    #scale_y_continuous(labels = formatoCientifico)           +
    ggplot2::scale_x_continuous(labels = formatoBRNumQtde)            +

    ggplot2::labs(title = paste0("Boxplot de ", X),
         y = Y)

}


graficoViolino = function(plotBase, X, Y){

    plotBase                                                 +
    ggplot2::aes_string(y = Y)                                        +
    ggplot2::geom_violin(alpha = 0.3,
                draw_quantiles = c(0.25, 0.5, 0.75)

    )                                                        +

    # Adicionando media e desvio padrao
    #stat_summary(fun.data = mean_sdl, #mult = 1,
    #             geom = "pointrange", color = "red")
    # Adicionando mediana
    #stat_summary(fun.y=median, geom="point", size=2, color="red")

    #scale_y_continuous(labels = formatoCientifico)            +

    ggplot2::scale_x_continuous(labels = formatoBRNumQtde)            +

    ggplot2::labs(title = paste0("Violino de ", X),
         y = Y)

}

graficoJitter = function(plotBase, X, Y){

    plotBase                                                 +
    ggplot2::aes_string(y = Y)                                        +
    ggplot2::geom_jitter(size = 3)                                    +

    #scale_y_continuous(labels = formatoCientifico)           +
    ggplot2::scale_x_continuous(labels = formatoBRNumQtde)            +

    ggplot2::labs(title = paste0("Jitter de ", X),
         y = Y)

}




graficoLinha = function(plotBase, Y){

  plotBase                               +
    ggplot2::aes_string(y = Y)           +
    ggplot2::geom_line()                 +
    #scale_y_continuous(labels=formatoCientifico)
    ggplot2::scale_y_continuous(labels = formatoBRNum)    +
    ggplot2::labs(y = Y)



}

graficoArea = function(plotBase, Y){

  plotBase                                   +
    ggplot2::aes_string(y = Y)
    ggplot2::geom_area(
              alpha    = 0.3,
              position = position_dodge(0.3))  +

    # Removendo legendas indesejadas
    ggplot2::guides(alpha = FALSE, colour = FALSE)      +


    #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    #scale_fill_manual(values = c("#00AFBB", "#E7B800"))  +
    #scale_y_continuous(labels=formatoCientifico)
    ggplot2::scale_y_continuous(labels = formatoBRNum)  +

    ggplot2::labs(y = Y)

}




graficoTM = function(dados, area, subgrupo, fill, label, titulo = ""){

    ggplot2::ggplot(dados)                                         +
      ggplot2::aes_string(area     = get(area),
                             subgroup = get(subgrupo),
                             fill     = get(fill),
                             label    = get(label))                            +

    treemapify::geom_treemap()                                                  +
    treemapify::geom_treemap_subgroup_border(colour =  "black")                 +
    treemapify::geom_treemap_subgroup_text(place    = "centre",
                                           colour   =  "black",
                                           fontface = "italic",
                                           grow     = T,
                                           alpha = 0.6, min.size = 0)           +
    treemapify::geom_treemap_text(place    = "topleft",
                                  colour   = "white",
                                  fontface = "bold",
                                  reflow   = T, alpha = 0.8)                    +
    #labs(title = titulo)                                            +
    ggplot2::theme(legend.position = "none")

}

graficoPieDonut = function(dados, grupoPie, grupoDonut, dimensao, titulo = paste(grupoPie, "x", grupoDonut)){

  webr::PieDonut(dados,
                 aes(count    = !! dimensao,
                 pies     = !! grupoPie,
                 donuts   = !! grupoDonut),
                 title = titulo)

}

graficoPontos = function(dados, X, Y, grupo, fill = X, titulo = paste(X, "x", Y)){

  ggplot(dados) +
    ggplot2::aes_string(x      = X,
                        y      = Y,
                        fill   = FILL,
                        label  = grupo,
                        colour = X,
                        shape  = X,
                        size   = Y)        +
    ggplot2::geom_point()                  +

    #geom_text(aes(label = ifelse(get(Y) > median(get(Y)), as.character(Unidade),'')),  hjust = -0.1, vjust = 0)  +
    ggrepel::geom_label_repel(colour = "white", segment.colour = "black")         +

    # Removendo legendas indesejadas
    ggplot2::guides(label = FALSE, shape = FALSE, size = FALSE, fill = FALSE)     +
    #scale_y_continuous(labels=formatoCientifico)                           +
    ggplot2::scale_y_continuous(labels = formatoBRNum)                            +
    ggplot2::theme_classic()                                                      +
    ggplot2::theme(legend.position = "top")                                       +
    ggplot2::labs(
      #title = titulo,
      x      = X,
      y      = Y,
      #fill   = FILL,
      colour = X)

}

graficoBarra = function(plotBase, X, Y, grupo){

  #ggplot2::ggplot(dados)                                 +
  plotBase                                   +
    ggplot2::aes_string(y = Y )  + #,
                        #alpha  = grupo)                    +


    #ggplot(dados, aes(x = reorder(get(X), get(Y)), y = get(Y), alpha = get(grupo)))          +
    #geom_bar(aes(fill = get(X)), stat = 'identity', colour = "black")       +
    ggplot2::geom_bar(stat = 'identity', colour = "black", position = "dodge")       +
    # Removendo legendas indesejadas
    #ggplot2::guides(alpha = FALSE, colour = FALSE)     +

    #scale_y_continuous(labels=formatoCientifico)                           +
    ggplot2::scale_y_continuous(labels = formatoBRNum)      +
    ggplot2::scale_alpha_discrete(range = c(0.2, 1)) +

    ggplot2::labs(y = stringr::str_to_title(Y))

}





graficoDensidadeSep = function(dados, X, FILL, FACET = FILL){

  ggplot2::ggplot(dados) +
    ggplot2::aes_string(x = X)  +
    ggplot2::geom_density(aes(fill = factor(get(FILL))), alpha = 0.6) +
    ggplot2::facet_wrap(~get(FACET))                                  +
    ggplot2::scale_y_continuous(labels = formatoCientifico)            +
    ggplot2::scale_x_continuous(labels = formatoBRNumQtde)            +
    ggplot2::theme_minimal()                                          +
    ggplot2::theme(legend.position = "top")                           +
    ggplot2::labs(title = paste0("Perfil de ", X, " x ", FILL),
         y = "Densidade")
}


graficoViolino2 = function(dados, X, Y = "Qtde", FILL, FACET = FILL){

  ggplot2::ggplot(dados) +
    ggplot2::aes_string(x = reorder(X, -Y), Y) +
    ggplot2::geom_violin(aes(fill = factor(get(FILL))), alpha = 0.7)   +
    ggplot2::facet_wrap(~get(FACET))                                   +
    ggplot2::scale_y_continuous(labels = formatoBRNum)                 +
    ggplot2::theme_classic()                                           +
    ggplot2::theme(legend.position = "top")                            +
    ggplot2::labs(title = "Perfil Tempo na Atividade x UF",
         y = "Tempo")
}


#y = "n"
plotaGrafico = function(df, x, y = "n", tipoGrafico, tema, legenda, grupo, titulo = paste(y, "x", x), fill = grupo, cor = grupo, shape = grupo, linetype = grupo, label = grupo, grupo2 = grupo){


  plotBase = ggplot2::ggplot(df)                        +
             # Aesthetics
             ggplot2::aes_string(x      = paste0("reorder(",x,",", y,")"),
                                 group  = grupo,
                                 colour = grupo,
                                 shape  = shape,
                                 fill   = fill)       +

             #facet_grid() +
             #scale_alpha(range = c(0.3, 0.5))  +
             #scale_alpha(0.3)  +

             # Labels
             # Obs.: Use labs() and set the same value for all aesthetics defining the appearance of geoms.
             ggplot2::labs(title    = titulo,
                           x        = str_to_title(x),
                           fill     = "",
                           #alpha    = "",
                           color    = "",
                           linetype = "",
                           shape    = "")             +

             # Temas
             switch(tema,

                Minimal  = ggplot2::theme_minimal(),
                Classic  = ggplot2::theme_classic(),
                Light    = ggplot2::theme_light(),
                Dark     = ggplot2::theme_dark(),
                Gray     = ggplot2::theme_gray(),
                Bw       = ggplot2::theme_bw(),
                Linedraw = ggplot2::theme_linedraw(),
                Void     = ggplot2::theme_void(),
                Test     = ggplot2::theme_test()
             )                                                              +
             # Legenda
             ggplot2::theme(legend.position = switch(legenda,

                                            Acima    = "top",
                                            Abaixo   = "bottom",
                                            Ausente  = "none",
                                            Esquerda = "left",
                                            Direita  = "right"),

                   #text = element_text(size=20)
                   axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=10, face = "bold")
                  )

  #dfG <- df
  #PLOT <- plotBase


  switch(tipoGrafico,

         Linha      = graficoLinha(plotBase, y),
         Area       = graficoArea(plotBase , y),
         Histograma = graficoHistograma(plotBase, X = x),
         Densidade  = graficoDensidade(plotBase, X = x),
         Boxplot    = graficoBoxplot(plotBase, X = x, Y = grupo),
         Violino    = graficoViolino(plotBase, X = x, Y = grupo),
         Jitter     = graficoJitter(plotBase , X = x, Y = grupo),
         #Violino    = graficoViolino(plotBase, X = x, Y = y),

         Barra      = graficoBarra(plotBase, X = x, Y = y, grupo = grupo) + coord_flip(),
         Coluna     = graficoBarra(dados = df, X = x, Y = y, fill = fill, grupo = grupo, titulo = titulo),
         Treemap    = graficoTM(dados = df, area = "n", subgrupo = grupo, fill = grupo, label = label, titulo = titulo),
         PieDonut   = graficoPieDonut(dados = df, grupoPie = grupo, grupoDonut = grupo2, dimensao = "n", titulo = titulo),
         Pontos     = graficoPontos(dados = df, X = x, Y = y, grupo = grupo, titulo = titulo)

  )


}



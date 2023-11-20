#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples




# reset_functions.R

renderMap <- function(input, output, MapaBase,fill_rows,n_filas) {
  #Aqui imprimimos el mapa
  output$MapaBase <- renderPlot({
    #Opciones para plotear
    plot_type <- switch(input$opcion_bio, 
                        "Colours" = "colores",
                        "Legend" = "leyenda",
                        "Elevations" = "elevaciones",
                        "Rivers" = "rios",
                        "Clean" = "limpio"
    )

    if (plot_type == "colores") {
      #Código para dibujar el mapa sin colores
      ggplot(MapaBase) + 
        geom_sf(aes(fill = .data[[fill_rows]])) +
        scale_fill_discrete(name = "Provincias") +
        theme_void()
    } else if (plot_type == "leyenda") {
      #Código para dibujar el mapa con leyenda
      ggplot(MapaBase) + 
        geom_sf(data = MapaBase) +
        geom_sf(fill = "white") +
        geom_sf_text(aes(label = !!as.name(fill_rows)), 
                     size = 4.5, hjust = 1, maxsize = 4) + 
        scale_fill_discrete(name = "Regiones") +
        theme(axis.text = element_text(size = 8), 
              axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0))) 
      
    } #Mapa de elevaciones
    else if (plot_type == "elevaciones") {
      
      #Solicita mapa. En funcion de la localizacion, se escogera un archivo con mas o menos detalle
      MapaBase_Elevaciones<-downloadElevationsMap(MapaBase,nivel, input$seleccion_pais,input$seleccion_region, input$seleccion_provincia, input$seleccion_comarca,  input$seleccion_localidad)
      
      #Preparo los datos de reprsentacion del mapa
      #Colores para plotear
      #elevation_colours = c("#00A600", "#E6E600", "#EAB64E", "#EEB99F", "#F2F2F2")
      #elevation_colours = c("green","green" "yellow","orange", "brown")
      #Altura máxima
      max_altura <- max(MapaBase_Elevaciones$Altura)
      #Intervalos altura
      intervalos_altura <- seq(0, max_altura, length.out = 5)
      
      # Plotear el mapa utilizando los datos unidos
      ggplot() + 
        geom_raster(data = MapaBase_Elevaciones, aes(x, y, fill = Altura)) + 
        scale_fill_gradient(limits = c(0, max_altura), breaks = intervalos_altura) +
        labs(fill = input$seleccion_clima) +
        scale_fill_gradient(low = "yellow", high = "brown") +
        geom_sf(data = MapaBase, alpha = 0.5, linetype = "dotted", size = 1.5) 
      
      #scale_colour_manual(values = c("#00A600", "#E6E600", "#EAB64E", "#EEB99F", "#F2F2F2"))
    } else if (plot_type == "rios") {# Graficar mapas de rios y límites administrativos
      MapaBase_Rios <- downloadRiverMap(MapaBase, st_crs(MapaBase))
      ggplot() + 
        geom_sf(data = MapaBase) +
        geom_sf(data = MapaBase_Rios , color = "blue") 
      
    } else if (plot_type == "limpio") {
      #Código por defecto si no se selecciona ninguna opción válida
      ggplot(MapaBase) + 
        geom_sf(data = MapaBase) +
        geom_sf(fill = "white")
      
    }
  }) #Aqui acaba plotear mapa
} #Fin renderizacion imagen
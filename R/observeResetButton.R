#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples




observePaisButton <- function(session, input) {
  observeEvent(input$seleccion_pais, {
    # Escoge el nombre del país según el código
    options <- paises$name 
    values <- paises$iso3
    index_pais <- match(input$seleccion_pais, options)
    pais_seleccionado2 <- values[index_pais]
    nivel_solicitado=1
    cambia_fill_rows(nivel_solicitado) # La leyenda depende de este valor
    MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
    lockLimitDownloadCountry(pais_seleccionado2) # Deshabilita botones que no deben funcionar porque no hay
    regiones <- unique(MapaBase$NAME_1)
    render_mapa(MapaBase) #Llma a la funcion de plotear el mapa
    # Actualiza las opciones de seleccion_region
    choices <- c("region", regiones)
    updateSelectInput(session, "seleccion_region", choices = choices)
   })
}
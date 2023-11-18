#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples




# renderTittleMap
renderTittleMap <- function(input, output, seleccion_pais, seleccion_region, seleccion_provincia, seleccion_comarca, seleccion_localidad) {
  output$TittleMap <- renderText({
    browser()

    # Impresion del título ----------------------------------------------------
    title <- paste0("<strong>Country </strong> ", seleccion_pais)
    if (seleccion_region != "region") {
      title <- paste0(title, "<strong> Region </strong>", seleccion_region)
      if (seleccion_provincia != "provincia") {
        title <- paste0(title, "<strong> Provincia </strong>", seleccion_provincia)
        if (seleccion_comarca != "comarca") {
          title <- paste0(title, "<strong> Comarca </strong>", seleccion_comarca)
          if (seleccion_localidad != "localidad") {
            title <- paste0(title, "<strong> Localidad </strong>", seleccion_localidad)
          }
        }
      }
    }
    HTML(paste0("<h3 style='color: green;'>", title, "</h3>"))
  })
}

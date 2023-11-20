#' Check if map is in local
#' @param input country selected
#' @param output level of detail. 1,2,3 or 4
#' @param seleccion_pais level of detail. 1,2,3 or 4
#' @param seleccion_region level of detail. 1,2,3 or
#' @param seleccion_provincia level of detail. 1,2,3 or 4
#' @param seleccion_comarca level of detail. 1,2,3 or 4
#' @param seleccion_localidad level of detail. 1,2,3 or 4
#' @return #No returns,,. It prints in screen result of TITTLE
#' @export
#' @examples
# renderTittleMap
renderTittleMap <- function(input, output) {
  output$TittleMap <- renderText({
    
    # Impresion del título ----------------------------------------------------
    title <- paste0("<strong>Country </strong> ", input$seleccion_pais)
    if (input$seleccion_region != "region") {
      title <- paste0(title, "<strong> Region </strong>", input$seleccion_region)
      if (input$seleccion_provincia != "provincia") {
        title <- paste0(title, "<strong> Provincia </strong>", input$seleccion_provincia)
        if (input$seleccion_comarca != "comarca") {
          title <- paste0(title, "<strong> Comarca </strong>", input$seleccion_comarca)
          if (input$seleccion_localidad != "localidad") {
            title <- paste0(title, "<strong> Localidad </strong>", input$seleccion_localidad)
          }
        }
      }
    }
    HTML(paste0("<h3 style='color: green;'>", title, "</h3>"))
  })
}

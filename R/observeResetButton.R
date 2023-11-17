#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples


# observeResetButton

observeResetButton <- function(session, input) {
  observeEvent(input$reset, {
    browser()
    session$reload()
  })
}
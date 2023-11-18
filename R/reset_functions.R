#' This function reset all values and restart all maps
#'
#' @param session Contents of sesion 
#' @param input Contents of input
#' @return #No returns
#' @export
#' @examples
# reset_functions.R
observeResetButton <- function(session, input) {
  observeEvent(input$reset, {
    session$reload()
  })
}
#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples


#Funcion que cierra niveles de seleccion segun el nivel que hay
lockSelection <- function(nivel){
  shinyjs::enable("seleccion_localidad");shinyjs::enable("seleccion_comarca");shinyjs::enable("seleccion_provincia");shinyjs::enable("seleccion_region")
  #Deshabilita botones en funcion de los niveles descargados
  if (nivel==1){
    shinyjs::disable("seleccion_localidad");shinyjs::disable("seleccion_comarca");shinyjs::disable("seleccion_provincia");shinyjs::enable("seleccion_region")
  }
  if (nivel==2){
    shinyjs::disable("seleccion_localidad");shinyjs::disable("seleccion_comarca");shinyjs::enable("seleccion_provincia");shinyjs::enable("seleccion_region")
  }
  if (nivel==3){
    shinyjs::disable("seleccion_localidad");shinyjs::enable("seleccion_comarca");shinyjs::enable("seleccion_provincia");shinyjs::enable("seleccion_region")
  }
  if (nivel==4){
    shinyjs::enable("seleccion_localidad");shinyjs::enable("seleccion_comarca");shinyjs::enable("seleccion_provincia");shinyjs::enable("seleccion_region")
  }
}
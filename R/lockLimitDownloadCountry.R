#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples


#Bloquea casillas de selección para evitar solicitar descarga mapas que no existen
lockLimitDownloadCountry <- function(pais=""){
  
  url_base <- "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_"
  
  nivel_while = 1;nivel=0; 
  max_nivel = 4 # Valor máximo para el nivel
  content <- "" # Inicializar el contenido
  test_url <- paste0(url_base, pais, "_", nivel_while, ".json")
  
  response <- HEAD(test_url)
  # Comprobar si la salida contiene la cadena "1 received"
  
  while(nivel_while <= max_nivel && response$status_code == 200){
    nivel_while = nivel_while + 1
    test_url <- paste0(url_base, pais, "_", nivel_while, ".json")
    response <- HEAD(test_url)
  }
  nivel = nivel_while -1
  rm(content, test_url, response,max_nivel,nivel_while)
  lockSelection(nivel) #Deshabilita botones que no deben funcioanr porque no hay
}
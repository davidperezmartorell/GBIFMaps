#' Download Map if it not exist in local
#'
#' @param pais country selected
#' @param nivel_solicitado level of detail. 1,2,3 or 4
#' @return downloadMap Return the map
#' @export
#' @examples

# Funcion Carga Mapa ------------------------------------------------------
#Aqui revisamos si el mapa existe en local para no tener que descargarlo de internet


downloadMap <- function(pais = "", nivel_solicitado){
  # Use sys.source to load functions from mapInlocal.R
  file <- "R/mapInLocal.R"; source(file)

  
  MapaBase<-mapInLocal(pais,nivel_solicitado)

  # Si el archivo de ese mapa con ese nivel no esta en local, y esta disponible, solicitalo a internet
  if (is.null(MapaBase)){
    # Descarga el mapa desde Internet si el archivo no existe en local
    #Chequea antes de descargar
    url_base <- "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_"
    test_url <- paste0(url_base, pais, "_", nivel_solicitado, ".json")
    response <- GET(test_url, timeout(2))
    if (response$status_code == 200){
      MapaBase <- geodata::gadm(country = pais, level = nivel_solicitado, path = ".") %>% st_as_sf()
      Sys.sleep(1)
    }
  }
  return(MapaBase)  #Actualiza la info de variable MapaBase
}

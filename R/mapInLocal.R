#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples


#Este mapa solicita si el mapa esta en local
mapInLocal <- function(pais = "", nivel) {
  
  # Compose file paths for both 41 and 36 maps
  map_file41 <- paste0("inst/maps/gadm41_", pais, "_", nivel, "_pk.rds")
  map_file36 <- paste0("inst/maps/gadm36_", pais, "_", nivel, "_sp.rds")
  
  # Check if the 41 map file exists
  if (file.exists(map_file41)) { 
    # Load and return the map
    MapaBase <- readRDS(map_file41) %>% st_as_sf()
    return(MapaBase) 
  } 
  # Check if the 36 map file exists
  else if (file.exists(map_file36)) { 
    # Load and return the map
    MapaBase <- readRDS(map_file36) %>% st_as_sf()
    return(MapaBase)
  } else {
    # Inform the user that the file doesn't exist
    cat("No map file found in local directory.\n")
    return(NULL)
  }
}

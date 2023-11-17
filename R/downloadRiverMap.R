#' Check if map is in local
#'
#' @param MapaBase Map
#' @param crs_mapabase Contains the CRS of the map to their parameters filtering
#' @return MapaBase Rivers Map 
#' @export
#' @examples


#Este mapa solicita si el mapa esta en local
downloadRiverMap <- function(MapaBase, crs_mapabase){

  # Assign the coordinate reference system to MapaBase
  MapaBase <- st_set_crs(MapaBase, crs_mapabase)
  
  # Descargar datos de rios y límites administrativos
  rios <- read_sf("inst/rivers/ne_10m_rivers_lake_centerlines.dbf", encoding = "latin1", stringsAsFactors = FALSE)
  

  return(rios)
  
}

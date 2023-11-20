#' Check if map is in local
#'
#' @param MapaBase Map
#' @param crs_mapabase Contains the CRS of the map to their parameters filtering
#' @return MapaBase Rivers Map 
#' @export
#' @examples


downloadElevationsMap <- function(MapaBase,nivel, pais="", region="", provincia="", comarca="", localidad=""){
  #Mapa de elevaciones. Segun la localizacione scogida, pondra mas o menos detalle
  nivel_solicitado <- nivel
  if ((region=="region")&&(provincia=="provincia")&&(comarca=="comarca")&&(localidad=="localidad")){
    filename = 'inst/worldclim/wc2.1_10m_elev.tif'
    elevation = readGDAL(filename)
  }#Si estamos a nivel de region
  else if((region!="region")&&(provincia=="provincia")&&(comarca=="comarca")&&(localidad=="localidad")){
    filename = 'inst/worldclim/wc2.1_5m_elev.tif'
    elevation = readGDAL(filename)
  }#Si estamos a nivel de provincia
  else if((region!="region")&&(provincia!="provincia")&&(comarca=="comarca")&&(localidad=="localidad")){
    filename = 'inst/worldclim/wc2.1_2.5m_elev.tif'
    elevation = readGDAL(filename)
  }#Si estamos a nivel de comarca o localidad con maximo detalle
  else {
    #Geolocalizacion del espacio a analizar
    pais_seleccionado2<-pais
    MapaBase <- downloadMap(pais_seleccionado2, nivel_solicitado )
    # Obtener el altitud y longitud
    bbox <- st_bbox(MapaBase)
    x1 = bbox[1]
    y1 = bbox[4]
    x2 = bbox[3]
    y2 = bbox[2]
    # Obtener los valores de elevación para las coordenadas de interés
    #elevation <- elevation_30s(lat = y1, lon = x1, path = "worldclim/", clip = "none")
    elevation <- elevation_30s(country = pais_seleccionado2,lat = y1, lon = x1, path = "worldclim/", clip = "none")
    #elevation_3s(lat = y1, lon = x1, path = "worldclim/", clip = "none")
  }

  # Obtener la matriz de datos del archivo TIF
  MapaBase_Elevaciones = raster(elevation)
  
  
  #Recorta MapaBaseClima para que sea del tamaño del MapaBase
  elevaciones1  <- crop(MapaBase_Elevaciones, MapaBase)
  
  MapaBase_Elevaciones <- elevaciones1 %>% as("SpatialPixelsDataFrame") %>% as.data.frame()
  names(MapaBase_Elevaciones)[1] <- "Altura"
  
  
  # Devolver los datos climáticos descargados
  return(MapaBase_Elevaciones)
  
  
  #Temperatura (máxima, mínima, media) - códigos de datatypeid TMAX, TMIN, TAVG
  #Precipitación - código de datatypeid PRCP
  #Humedad relativa - código de datatypeid RH
  #Presión atmosférica - código de datatypeid PSXN
  #Velocidad del viento - código de datatypeid AWND
  #otros https://www.ncdc.noaa.gov/cdo-web/webservices/v2#dataTypes
  #otros https://www.ncdc.noaa.gov/cdo-web/webservices/v2
  #otros digital elevations https://data.noaa.gov/onestop/collections?q=%22digital%20elevation%22
  # Obtener la ubicación correspondiente a los parámetros de entrada
  # ubicacion <- switch(nivel,
  #                     "país" = pais,
  #                     "región" = paste(pais, region, sep = ", "),
  #                     "provincia" = paste(pais, provincia, region, sep = ", "),
  #                     "comarca" = paste(pais, comarca, provincia, region, sep = ", "),
  #                     "localidad" = paste(pais, localidad, comarca, provincia, region, sep = ", "))
  # 
  # # Descargar los datos climáticos utilizando la biblioteca RNOAA
  # datos_climaticos <- ncdc(
  #   datasetid = "GHCND",
  #   locationid = ubicacion,
  #   datatypeid = datatypeid,
  #   startdate = "2021-01-01",
  #   enddate = "2021-12-31",
  #   limit = 1000
  # )
  
  
  
}

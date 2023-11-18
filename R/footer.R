#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples

footer <- function(){
# Pie de pagina -----------------------------------------------------------
fluidRow(
  column(12, HTML("<h6> Con este programa vamos a pintar un mapa de la zona que desee, si GADM las tiene disponibles</h6>")),
  column(12, HTML("<h6> Autor David Perez Martorell davidperezmartorell@gmail.com</h6<>")),
  column(12, HTML("<h6> Patrocinado por www.ecosistemaglobal.org y Asociacion Focazul</h6>")),
  column(12, HTML("<h6> Version 1.0 Fecha 13/02/2023 En pruebas.Creación de entorno, formulario y mapa base</h6>")),
  column(12, HTML("<h6> Version 1.1 13/02/2023 Selección de nivel de detalle</h6>")),
  column(12, HTML("<h6> Version 1.2 14/02/2023 Agrego seleccion de regiones</h6>")),
  column(12, HTML("<h6> Version 1.3 15/02/2023 Optimizo seleccion de datos de paises ahora en tabla y título de mapa</h6>")),
  column(12, HTML("<h6> Version 1.4 17/02/2023 Agrego region y provincia</h6>")),
  column(12, HTML("<h6> Version 1.5 18/02/2023 Seleccion de region y detalle de provincias</h6>")),
  column(12, HTML("<h6> Version 1.6 22/02/2023 Ya aparecen los mapas y entro en las provincias</h6>")),
  column(12, HTML("<h6> Version 1.7 22/02/2023 Entro en comarcas y las muestro en plot</h6>")),                
  column(12, HTML("<h6> Version 1.7 22/02/2023 Ya entra en comarcas y localidadespendiente entrar en las comarcas</h6>")),
  column(12, HTML("<h6> Version 1.8 23/02/2023 Modifico menu en la parte superior.</h6>")),
  column(12, HTML("<h6> Version 1.9 23/02/2023 Trabajando clima. Ya aparece, hace falta usar variables</h6>")),
  column(12, HTML("<h6> Version 1.10 26/02/2023 Mejora de código interno, elimino casillas de verificación</h6>")),       
  column(12, HTML("<h6> Version 1.10 28/02/2023 Bloqueo de uso de selectores cuando no hay datos. Se corrije incidencia de ausencia de mapa</h6>")),       
  column(12, HTML("<h6> Version 2.0 12/03/2023 Renovacion de todo el codigo creando funciones, usando variables Reactivas,</h6>")),   
  column(12, HTML("<h6> Version 2.1 13/03/2023 Cambio de estilo en botones y optimizo seleccion para incorporar n capas</h6>")),   
  column(12, HTML("<h6> Version 2.2 15/03/2023 Corrijo error de mapas de localidades con detalle mas fino</h6>")),
  column(12, HTML("<h6> Version 2.2 15/03/2023 Resuelto problema de carga de mapa en niveles con mas detalle</h6>")),
  column(12, HTML("<h6> Version 2.3 19/03/2023 Cuanto mas geolocalizado, mas detalle de elevaciones y limitada la descarga y CPU</h6>")),
  column(12, HTML("<h6> Version 2.4 20/03/2023 Agrego capa de rios</h6>")),   
  column(12, HTML("<h6> pendiente filtrar mapa segun Latitud y Longitud</h6>")),
  column(12, HTML("<h6> pendiente agregar mapas de clima, estan los listados pero aun esta la carga bloqueada por seguridad de la aplicacion</h6>")),
  column(12, HTML("<h6> pendiente mejorar menu con css</h6>")),
  
  
)#Fin pie de pagina fluidRow
}


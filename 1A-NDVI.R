vegIndex <- function(R, NIR, index = "NDVI", nombre_parcela){
  # creating the NDVI
  if ((file.exists(paste("./temporal_nc/", nombre_parcela, "/", "NDVIpmask_", nombre_parcela, ".grd", sep="")))){
    # carga el archivo
    NDVIpmask <- stack(paste("./temporal_nc/", nombre_parcela, "/", "NDVIpmask_", nombre_parcela, sep=""))
  } else {
    # calculo el NDVI de la parcea a partir de las bandas raster de cada stack
    NDVIpmask <- ((NIR - R)/(NIR + R))
    NDVIpmask <- stack(NDVIpmask)
    names(NDVIpmask) <- names(NIR)
    # TODO: borrar los ultimos 4 caractes de cada layer name
    # guarda el archivo
    writeRaster(NDVIpmask, paste("./temporal_nc/", nombre_parcela, "/", "NDVIpmask_", nombre_parcela, sep=""))
  }
  

  # ahora arreglo los nombres de las capas para obtener las fechas y crear el vector de fechas
  namesa <- names(NDVIpmask)
  namesa
  fecha <- str_match(namesa,"_(.*?)T")
  fecha1 <- fecha[,2]
  fecha1
  
  # creo el vector con las fechas
  # install.packages("tidyverse")
  date_vector <- lubridate::ymd(basename(fecha1))
  date_vector
  typeof(date_vector)
  date_vector <- as.Date(date_vector)
  typeof(date_vector)
  
  # Z son los valores de tiempo. puedes especificar la lista temporal_nc, en orden, de los raster del stack. Obviamente el nº de fechas debe coincidir con el número de capas raster en el stack
  # OJO, hay que poner las fechas entre ""
  # date_vector <- c("2016-10-08","2016-10-28","2016-11-07","2016-11-17","2016-12-07","2016-12-17","2016-12-27","2017-01-16")
  # con as.Date transformo un objeto en formato fecha
  # date_vector <- as.Date(x=date_vector)
  
  NDVIpmasks <- NDVIpmask
  NDVIpmasksz <- setZ(NDVIpmasks, as.Date(date_vector))
  NDVIpmasksz <- stack(NDVIpmasksz)
  
  # TODO: necesitamos series temproales regulares. O sea que la frecuencia debe
  # ser constante. Rellenar con NA cada 5 dias pre S2B
  
  return(NDVIpmasksz)
}


# creo NDVI a partir del crop
if ((file.exists(paste("./temporal_nc/", nombre_parcela, "/", "NDVIp_", nombre_parcela, ".grd", sep="")))){
  # carga el archivo
  NDVIp <- stack(paste("./temporal_nc/", nombre_parcela, "/", "NDVIp_", nombre_parcela, sep=""))
} else {
  # calculo el NDVI de la parcea a partir de las bandas raster de cada stack
  NDVIp <- ((stackB08p - stackB04p)/(stackB08p + stackB04p))
  NDVIp <- stack(NDVIp)
  names(NDVIp) <- names(stackB04p)
  # guarda el archivo
  writeRaster(NDVIp, paste("./temporal_nc/", nombre_parcela, "/", "NDVIp_", nombre_parcela, sep=""))
}

# creating the NDVI
if ((file.exists(paste("./temporal_nc/", nombre_parcela, "/", "NDVIpmask_", nombre_parcela, ".grd", sep="")))){
  # carga el archivo
  NDVIpmask <- stack(paste("./temporal_nc/", nombre_parcela, "/", "NDVIpmask_", nombre_parcela, sep=""))
} else {
  # calculo el NDVI de la parcea a partir de las bandas raster de cada stack
  NDVIpmask <- ((stackB08pmask - stackB04pmask)/(stackB08pmask + stackB04pmask))
  NDVIpmask <- stack(NDVIpmask)
  names(NDVIpmask) <- names(stackB04pmask)
  # guarda el archivo
  writeRaster(NDVIpmask, paste("./temporal_nc/", nombre_parcela, "/", "NDVIpmask_", nombre_parcela, sep=""))
}

# dibujo el NDVI
plot(NDVIpmask[[1]])
# dibuja la parcela
lines(parcela2)

# dibuja todas las imagenes del rasterstack
library(rasterVis)
colors <- colorRampPalette(c("red","yellow","limegreen"))
levelplot(NDVIpmasksz, col.regions = colors)

# VERIFICAR SI ESTÁN ORDENADOS POR FECHA LOS RASTER (LO NORMAL ES QUE SI)
# ordeno por fecha (a ojo) los elementos del stack NDVIpmask y los guardo ordenados en NDVIpmasks
# NDVIpmasks <- stack(NDVIpmask[[5]],NDVIpmask[[8]], NDVIpmask[[6:7]],NDVIpmask[[1:4]])

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

# dibujo el stack NDVIpmasks, indicando con zlim los valores de la escala NDVI
par(mfrow=c(1,1))
plot(NDVIpmasksz[[1]], zlim=c(0.1, 0.7))
lines(parcela2)
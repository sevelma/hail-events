ARIMA <- function(vegIndex, evento){
  # install.packages("SDMTools")
  # install.packages("devtools")
  # install_github("ozjimbob/ecbtools")
  require(ecbtools)
  require(devtools)
  require(raster)
  require(rgdal) ## RGDAL depende de que esten instalados GDAL, libgdal1-dev (para tener acceso a gdal-config), y PROJ4 / $ sudo apt-get install gdal libgdal1-dev proj
  require(sf)
  
  EVI <- vegIndex
  ## para pixel by pixel : 87 momentos (r) para 345 pixels (c)
  # a partir de NDVIpmasksz
  NDVIpmaskz.TSdf <- matrix(nrow = nlayers(EVI),
                            ncol = length(values(EVI[[1]])) )
  for(i in 1:length(values(EVI[[1]]))){
    NDVIpmaskz.TSdf[,i] <- as.vector(EVI[i])
    # TODO: que no sea un bucle for, o al menos que sea un foreach()
  }
  namesa <- names(EVI)
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
  # for(i in 120:125){
  #   NDVIpmaskz.TSdf[,i] <- as.vector(NDVIpmasksz[1])
  # }
  
  # fecha de inicio del estudio en dia juliano
  tmp <- as.Date(date_vector, format = "%y%b%d")
  tmp
  format(tmp, "%j")
  
   EVI.TS <- ts(data=NDVIpmaskz.TSdf, 
               start=c(as.numeric(head(format(tmp, "%Y"), 1)), 
                       floor(as.numeric(head(format(tmp, "%j"), 1))/5)+1), 
               end=c(as.numeric(tail(format(tmp, "%Y"), 1)),
                     floor(as.numeric(tail(format(tmp, "%j"), 1))/5)+1), 
               frequency=73, class=c("mts", "ts"))
  
  #TODO: revisar valores de start y end y frecuency 
   
  ARIMAs <- list()
  for(i in 1:ncol(EVI.TS)){
    try(ARIMAs[[i]] <- decompose(EVI.TS[,i]))
  }
  
  
  
  evento <- 85 # imagen post tormenta
  random <- raster(EVI[[1]])
  
  random.v <- vector()
  for(j in 1:length(ARIMAs)){
    if(!is.null(ARIMAs[[j]])){random.v[j] <- ARIMAs[[j]]$random[evento]}
  }
  
  random.v[345] <- NA
  
  values(random) <- random.v
  plot(random)
  return(random)
}


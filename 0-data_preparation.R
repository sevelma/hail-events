dataPrep <- function(folder, aoi, output = "./temporal_nc"){
  #load packages
  require(sf)
  require(raster)
  require(rgdal)
  require(rgeos)
  require(bfast)
  require(zoo)
  require(bfastSpatial)
  require(stringr)
  require(plotly)
  
  #check directory
  getwd()
  #set working directory
  working_directory <- setwd("/media/DATOS/z/")
  working_directory
  
  #importar las parcelas
  parcela <- st_read(aoi)
  parcela[[1]]
  nombre_parcela <- as.character(parcela[[1]])
  typeof(nombre_parcela)
  
  #??? si cargo el kml me carga todas las parcelas... como las separo en R??
  # parcelakml <- st_read("relevamiento.kml")
  
  #comprobacion de geometria de los poligonos
  st_is_valid(parcela)
  
  # Crea un directorio para guardar los datos
  dir.create(paste(output, nombre_parcela, sep=""))
  
  # crea el nombre de la baldosa (tile) para guardar el archivo
  # mira a ver si puedes sustituir la función str_match con la función grep
  sentinel_tile <- as.character(list.files(path=folder, pattern = "B08.jp2$",full.names = TRUE, recursive = T)[[1]])
  sentinel_tile
  # sentinel_tile2 <- str_match(sentinel_tile,"/ExtB04B08_nc/(.*?)_")[,2]
  # sentinel_tile2
  sentinel_tile2 <- "TODO"
  
  # load or creates the list of all the B08.jp2 files in the directory
  if ((file.exists(paste(output, "/listB08_", sentinel_tile2, sep = "")))){
    # carga el archivo
    load(paste(output, "/listB08_", sentinel_tile2, sep=""))
  } else {
    # creates the list of all the B08.jp2 files in the directory
    listB08 <- list.files(path=folder, pattern = "B08.jp2$",full.names = TRUE, recursive = T)
    # guarda el archivo
    save(listB08,file=paste(output, "/listB08_", sentinel_tile2, sep = ""))
  }
  
  # crea un stack con todos los raster
  stackB08 <- stack(listB08)
  # ----------
  # NO RECOMENDADO TARDA MUCHO. TARDA MENOS EN MONTAR EL STACK DE IMAGENES.
  # # guarda el stack de raster
  # writeRaster(stackB08, paste("./temporal_nc/stackB08_", sentinel_tile2, sep=""))
  # # carga el rasterstack. 
  # stackB08 <- stack(paste("./temporal_nc/stackB08_", sentinel_tile2, sep=""))
  # ----------
  
  # load or creates the list of all the B04.jp2 files in the directory
  if ((file.exists(paste(output, "listB04_", sentinel_tile2, sep = "")))){
    # carga el archivo
    load(paste(output, "/listB04_", sentinel_tile2, sep=""))
  } else {
    # creates the list of all the B04.jp2 files in the directory
    listB04 <- list.files(path=folder, pattern = "B04.jp2$",full.names = TRUE, recursive = T)
    # guarda el archivo
    save(listB04,file=paste(output, "/listB04_", sentinel_tile2, sep = ""))
  }
  
  
  # crea un stack con todos los raster
  stackB04 <- stack(listB04)
  # ----------
  # NO RECOMENDADO TARDA MUCHO. TARDA MENOS EN MONTAR EL STACK DE IMAGENES.
  # # guarda el stack de raster
  # writeRaster(stackB04, paste("./temporal_nc/stackB04_", sentinel_tile2, sep=""))
  # # carga el rasterstack
  # stack(paste("./temporal_nc/stackB04_", sentinel_tile2, sep=""))
  # ----------
  
  # transforma la parcela de geometría sf a sp (para poder usarla con los comandos de sp)
  parcela2 <- as(parcela, "Spatial")
  # mirar al ppo para cargar la parcela como spatial directamente
  
  # transforma el sistema de coordenadas (crs) de la parcela al mismo crs que el conjunto de rasters de stackB08
  parcela2 <- spTransform(parcela2, stackB08@crs)
  
  # la función crop recorta la capa raster a partir de la vectorial, pero no píxel a píxel si no que crea el cuadrado mínimo que contendría la parcela
  # loads stack if it exists, if not stack is created
  if ((file.exists(paste(output, "/",  nombre_parcela, "/", "stackB08p_", nombre_parcela, ".grd", sep="")))){
    # carga el archivo
    stackB08p <- stack(paste(output, "/", nombre_parcela, "/", "stackB08p_", nombre_parcela, sep=""))
  } else {
    # crop all the B08.jp2 files in the stack
    stackB08p <- crop(stackB08, parcela2)
    # guarda el archivo
    writeRaster(stackB08p, paste(output, "/",  nombre_parcela, "/", "stackB08p_", nombre_parcela, sep=""))
  }
  
  # mask recorta la parcela en función del polígono vectorial
  # loads stack if it exists, if not stack is created
  if ((file.exists(paste(output, "/",  nombre_parcela, "/", "stackB08pmask_", nombre_parcela, ".grd", sep="")))){
    # carga el archivo
    stackB08pmask <- stack(paste(output, "/",  nombre_parcela, "/", "stackB08pmask_", nombre_parcela, sep=""))
  } else {
    # mask all the B08.jp2 files in the stack
    stackB08pmask <- mask(stackB08p, parcela2)
    # guarda el archivo
    writeRaster(stackB08pmask, paste(output, "/",  nombre_parcela, "/", "stackB08pmask_", nombre_parcela, sep=""))
  }
  
  # dibuja el primer elemento (primer raster) de stack de rasters
  plot(stackB08p[[1]])
  plot(stackB08pmask[[1]])
  
  # dibuja la parcela
  lines(parcela2)
  
  # la función crop recorta la capa raster a partir de la vectorial, pero no píxel a píxel si no que crea el cuadrado mínimo que contendría la parcela
  # loads stack if it exists, if not stack is created
  if ((file.exists(paste(output, "/",  nombre_parcela, "/", "stackB04p_", nombre_parcela, ".grd", sep="")))){
    # carga el archivo
    stackB04p <- stack(paste(output, "/",  nombre_parcela, "/", "stackB04p_", nombre_parcela, sep=""))
  } else {
    # crop all the B08.jp2 files in the stack
    stackB04p <- crop(stackB04, parcela2)
    # guarda el archivo
    writeRaster(stackB04p, paste(output, "/", nombre_parcela, "/", "stackB04p_", nombre_parcela, sep=""))
  }
  
  # mask recorta la parcela en función del polígono vectorial
  # loads stack if it exists, if not stack is created
  if ((file.exists(paste(output, "/",  nombre_parcela, "/", "stackB04pmask_", nombre_parcela, ".grd", sep="")))){
    # carga el archivo
    stackB04pmask <- stack(paste(output, "/",  nombre_parcela, "/", "stackB04pmask_", nombre_parcela, sep=""))
  } else {
    # mask all the B08.jp2 files in the stack
    stackB04pmask <- mask(stackB04p, parcela2)
    # guarda el archivo
    writeRaster(stackB04pmask, paste(output, "/",  nombre_parcela, "/", "stackB04pmask_", nombre_parcela, sep=""))
  }
  
  # dibuja el primer elemento (primer raster) de stack de rasters
  plot(stackB08p[[1]])
  plot(stackB08pmask[[1]])
  # dibuja la parcela
  lines(parcela2)
  
  # if we want to plot the points on each layers of the stack we need to
  # create a function and pass it to the addfun argument (see ?raster::plot)
  fun <- function() {
    lines(parcela2, col = "blue", pch = 3)
  }
  plot(stackB08pmask, addfun = fun)
  result <- list()
  result[[1]] <- stackB04pmask
  result[[2]] <- stackB08pmask
  names(result) <- c("R", "NIR")
  return(result)
}

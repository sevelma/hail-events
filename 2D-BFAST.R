
#crea la fecha para el análisis. es el día juliano (por ejemplo el 1 de septiembre es el día 244)
date_an1 <- 2018
date_an2 <- 244

# ---------------
# # NOW I CREATE THE SAMPLE POINT TO USE BFASTPIXEL. 3 OPTIONS:
# #crea un punto espacial en la parcela para poder extraer los valores del punto
# # q1 <- 593360
# # q2 <- 6312950
# # q <- SpatialPoints(cbind(q1, q2))
# 
# # usa como punto el centroide de la parcela
# q <- gCentroid(parcela2)
# q1 <- q@coords[1]
# q2 <- q@coords[2]
# 
# # writeOGR(q, ".", "filename", driver="ESRI Shapefile")
# 
# # run bfm on a pixel of known (x,y) location
# bfm <- bfmPixel(NDVIpmasksz, cell=c(q1, q2), start=c(date_an1, date_an2))
# bfm
# print(bfm$cell) # check the corresponding cell index
# 
# plot(NDVIpmasksz[[1]])
# lines(parcela2)
# points(q)
# 
# # get bfm results for a pixel of known cell index
# targcell <- print(bfm$cell)
# bfm <- bfmPixel(NDVIpmasksz, cell=targcell, start=c(date_an1, date_an2), min.thresh = 0.2) # ,interactive=TRUE)
# 
# par(mar=c(2,2,1,1))
# plot(bfm$bfm)
# print(targcell <- bfm$cell) # store cell index for follow-up analysis
# 
# # guarda el gráfico como png
# # dev.copy(png, paste("./plots/bfmPixel_", nombre_parcela, sep=""))
# # dev.off()
# # dev.new()
# 
# ## change the model parameters
# # 1. harmonic order
# bfm <- bfmPixel(NDVIpmasksz, cell=targcell, start=c(date_an1, date_an2), order=3, min.thresh = 0.2)
# plot(bfm$bfm)
# # 2. no trend
# bfm <- bfmPixel(NDVIpmasksz, cell=targcell, start=c(date_an1, date_an2), order=3, formula=response~harmon, min.thresh = 0.1)
# plot(bfm$bfm)
# # 3. trend only
# bfm <- bfmPixel(NDVIpmasksz, cell=targcell, start=c(date_an1, date_an2), formula=response~trend, min.thresh = 0.15)
# plot(bfm$bfm)
# # 4. trend only
# bfm <- bfmPixel(NDVIpmasksz, cell=targcell, start=c(date_an1, date_an2), formula=response~trend)
# plot(bfm$bfm)
# # }
# ---------------

# I create random sample points (number of points)
number_of_points <- 6
rsppoints <- spsample(parcela2 ,n=number_of_points,"random")
plot(NDVIpmasksz[[1]])
lines(parcela2)
points(rsppoints)

# creates a list to be filled by bfm values
bfm <- list(1:number_of_points)
typeof(bfm)

# run bfm on a pixel of known (x,y) location
for (i in 1:number_of_points) {bfm[[i]] <- bfmPixel(NDVIpmasksz, cell=as.numeric(c(rsppoints@coords[i,1],rsppoints@coords[i,2])), start=c(date_an1, date_an2))
}

typeof(bfm)
dev.off()
par(mfrow=c(2,3))
for (i in 1:number_of_points) {plot(bfm[[i]]$bfm, xlab = parcela2$id)
}

print(bfm$cell) # check the corresponding cell index
bfm$cell$breakpoint

# BfastSpatial

# la función crop recorta la capa raster a partir de la vectorial, pero no píxel a píxel si no que crea el cuadrado mínimo que contendría la parcela
# loads stack if it exists, if not stack is created
if ((file.exists(paste("./temporal_nc/", nombre_parcela, "/", "bfmspat_", nombre_parcela, ".grd", sep="")))){
  # carga el archivo
  bfmspat <- stack(paste("./temporal_nc/", nombre_parcela, "/", "bfmspat_", nombre_parcela, sep=""))
} else {
  # crop all the B08.jp2 files in the stack
  time1 <- Sys.time()
  bfmspat <- bfmSpatial(NDVIpmasksz,start=c(date_an1, date_an2), mc.cores = 4)
  time2 <- Sys.time()
  time2 - time1
  # guarda el archivo
  writeRaster(bfmspat, paste("./temporal_nc/", nombre_parcela, "/", "bfmspat_", nombre_parcela, sep=""))
}

plot(bfmspat)
par(mfrow=c(1,1))
plot(bfmspat[[2]], zlim=c(-0.5,0.5))
lines(parcela2)

plot((NDVIpmasksz[[70]] - bfmspat[[2]]) / NDVIpmasksz[[70]])
plot((bfmspat[[2]]) / NDVIpmasksz[[70]], zlim = c(-5,5))
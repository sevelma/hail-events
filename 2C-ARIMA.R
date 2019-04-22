# install.packages("SDMTools")
# install.packages("devtools")
# install_github("ozjimbob/ecbtools")
library(ecbtools)
library(devtools)
library(raster)
library(rgdal) ## RGDAL depende de que esten instalados GDAL, libgdal1-dev (para tener acceso a gdal-config), y PROJ4 / $ sudo apt-get install gdal libgdal1-dev proj
library(sf)
install_github("marchtaylor/sinkr")
install_github("ffilipponi/rtsa")

#set working directory
working_directory <- setwd("/media/DATOS/z") ### Esta es la carpeta donde estan los archivos, tienen que tener orden temporal y alfabetico al mismo tiempo
### En este caso el nombre del archivo es A\d\d\d\dD\d\d\d por ej A2000D049 para el DOY 49 del año 2000 (gprename  para renombrar!)

# cargo NDVI
plot(NDVIpmask)

# if we want to plot the points on each layers of the stack we need to
# create a function and pass it to the addfun argument (see ?raster::plot)
fun <- function() {
  lines(parcela2, col = "blue", pch = 3)
}
plot(NDVIpmasksz, addfun = fun, zlim=c(0.3,1))
# install.packages("rasterVis")
library(rasterVis)
colors <- colorRampPalette(c("red","yellow","limegreen"))
levelplot(NDVIpmasksz, col.regions = colors)

NDVIrts <- rts(NDVIpmasks, date_vector)
NDVIrts

rtsa.stl(NDVIrts)

NDVIpmasksz.df <- as.data.frame(NDVIpmasksz)   ### Para definir la cantidad de clusters (Hengl)
# View(head(NDVIpmasksz.df))
NDVIpmasksz.df <- na.omit(NDVIpmasksz.df)
NDVIpmasksz

# # View(head(NDVIpmasksz.df))
# wss <- (nrow(NDVIpmasksz.df)-1)*sum(apply(NDVIpmasksz.df,2,var))
# for (i in 2:12) {wss[i] <- sum(kmeans(NDVIpmasksz.df, centers=i)$withinss)}
# plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 
# kmeans.NDVIpmasksz <- kmeans(NDVIpmasksz.df, 12, iter.max=50) ### kmeans, con 12 clusters
# head(kmeans.NDVIpmasksz)
# 
clusters <- raster(NDVIpmasksz)   ## creamos un raster vacio con los mismos parametros que NDVIpmasksz
clusters <- setValues(clusters, kmeans.NDVIpmasksz$cluster) ## y le asignamos los valores de los clusters
clusters
# dev.off()
# par(mfrow=c(1,1))
# plot(clusters, col=rev(rainbow(12)))
# lines(parcela2)


library(rtsa)

NDVIpmasksz.mean <- zonal(NDVIpmasksz, clusters, fun="mean")  ### calculamos la media por cluster
NDVIp.min <- zonal(NDVIp, clusters, fun="min")
NDVIp.max <- zonal(NDVIp, clusters, fun="max")
NDVIp.sum <- zonal(NDVIp, clusters, fun="sum")

summary(NDVIp.mean)
# View(head(NDVIp.mean))

# fecha de inicio del estudio en dia juliano
tmp <- as.Date(date_vector, format = "%y%b%d")
tmp
format(tmp, "%j")

NDVIp.trasp <- t(NDVIpmasksz[,2:length(listB04)])

NDVIp.TS <- ts(data=NDVIp.trasp, start=c(2016, 9), end=c(2019, 3), frequency=36, class=c("mts", "ts"))
dev.off()
par(mfrow=c(1,1))
ts.plot(NDVIp.TS)

library("SDMTools") ## Un modulo para estadisticas zonales
clusters.df <- as.data.frame(clusters)
NDVIp.ZonalStat <- ZonalStat(NDVIp.df[,1], clusters.df) ## solo admite rasterlayers y no rasterstacks, pero en este caso sale con data.frame

## TS1 <- StructTS(NDVIp.TS[,1]) ## Este metodo separa en level/slope/sea 
## plot(TS1$fitted)

### Descomposición por medias moviles
TSdecomp1 <- decompose(NDVIp.TS[,1])
TSdecomp2 <- decompose(NDVIp.TS[,2])
TSdecomp3 <- decompose(NDVIp.TS[,3])
TSdecomp4 <- decompose(NDVIp.TS[,4])
TSdecomp5 <- decompose(NDVIp.TS[,5])
TSdecomp6 <- decompose(NDVIp.TS[,6])
TSdecomp7 <- decompose(NDVIp.TS[,7])
TSdecomp8 <- decompose(NDVIp.TS[,8])
TSdecomp8 <- decompose(NDVIp.TS[,8])
TSdecomp9 <- decompose(NDVIp.TS[,9])
TSdecomp10 <- decompose(NDVIp.TS[,10])
TSdecomp11 <- decompose(NDVIp.TS[,11])
TSdecomp12 <- decompose(NDVIpmasksz.TS[,12])

par(mfrow=c(1,1))
plot(TSdecomp1$trend)
plot(TSdecomp2$trend)
plot(TSdecomp3$trend)
plot(TSdecomp4$trend)
plot(TSdecomp5$trend)
plot(TSdecomp6$trend)
plot(TSdecomp7$trend)
plot(TSdecomp8$trend)
plot(TSdecomp9$trend)
plot(TSdecomp10$trend)
plot(TSdecomp11$trend)
plot(TSdecomp12$trend)

par(mfrow=c(4,3))
plot(TSdecomp1)
plot(TSdecomp2)
plot(TSdecomp3)
plot(TSdecomp4)
plot(TSdecomp5)
plot(TSdecomp6)
plot(TSdecomp7)
plot(TSdecomp8)
plot(TSdecomp9)
plot(TSdecomp10)
plot(TSdecomp11)
plot(TSdecomp12)



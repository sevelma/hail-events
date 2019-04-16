# #install package
# install.packages("raster")
library(raster)

pre <- raster("foo")

post <- raster("bar")

diff <- post - pre
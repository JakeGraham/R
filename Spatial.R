#install.packages("EcoGenetics")
library(EcoGenetics)
library(rgdal)
library(raster)
library(itcSegment)


######################
######################
######################    			Detrending...
######################
######################


d1 <- raster("N:\\Data02\\bcal\\Personal\\Jake\\lil_plots\\los2_10m_lilv2.tif")
plot(d1)


##
## Function that detrends a raster object using the "EcoGenetics" package 
##    inputs:
##       rastIn = the raster object to be detrended.. currently only works with one layer
##       pDeg = the degree of polynomial to be used to fit the trend
##       center = should the data be centered? 
##       scale = should the data be scaled?
##       raw = use raw and not orthogonal polynomials
##       latlong = is the data in lat long.. if so additional criteria needed... see EcoGenetics documentation.. https://rdrr.io/cran/EcoGenetics/man/eco.detrend.html
##       
##    outputs:
##       OP = output raster that has been detrended


detrendRaster <- function(rastIn, pDeg = 1, center = TRUE, scale = FALSE, raw = FALSE, latlong = FALSE){
   # convert from raster to points  
   p <- as.data.frame(rasterToPoints(rastIn))
   
   # munge for EcoGenetics
   XY <- as.data.frame(cbind(p$x, p$y))
   Z <- as.data.frame(p[,3])
   colnames(Z) <- "z"

   # perform the EcoGenetics detrending function
   OP <- eco.detrend(Z = Z, XY = XY, degree = pDeg, center = center, scale = scale, raw = raw, latlon = latlong)
   
   # create new dataframe with XY locations from original data
   DT <- p[,1:2]

   # add the detrended values (orignal - predicted)
   DT$z <- p[,3] - OP@ANALYSIS@PREDICTED[,1]

   ## convert from points back to raster...
   # set coordinates
   DTT <- DT
   coordinates(DTT) <- ~ x+y

   # coerce to SpatialPixelsDataFrame
   gridded(DTT) <- TRUE

   # coerce to raster
   OP <- raster(DTT)
  
   # return the detrended raster
   return(OP)
}



## test different order polynomial fits
dt1 <- detrendRaster(d1, 1)
dt2 <- detrendRaster(d1, 2)
dt3 <- detrendRaster(d1, 3)

## compare detrended data to original
par(mfrow = c(2,2))
plot(d1, main = "orig")
plot(dt1, main = "1st order")
plot(dt2, main = "2nd order")
plot(dt3, main = "3rd order")


######################
######################
######################    			ITC segmentation...
######################
######################

dat <- d1
values(dat) <- (getValues(dt2) + abs(min(getValues(dt2),na.rm = T))) 
s <- raster(nrow=200, ncol=200)
lil <- dat
lil@ncols <- s@ncols
lil@nrows <- s@nrows

t <- resample(dat, lil, method = 'bilinear')
plot(t)


#### now start playing with parameters....
test <- itcIMG(t, epsg = 2789, searchWinSize = 3, TRESHSeed = .3, TRESHCrown = .5, DIST = 10, th = 0, ischm = T)
crs(test) <- crs(dat)
plot(t)
plot(test, add = T)















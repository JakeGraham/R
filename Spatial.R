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

values(d1) <- getValues(d1)


str(d1)

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


##
## Function that performs segmentation of shrubs (or trees) using the R package "itcSegment" 
##    inputs:
##       rastIn = the raster object to be segmented
##       ncol = number of columns to resample to.... if no resampling is desired ncol = rastIn@ncol
##       ncol = number of rows to resample to.... if no resampling is desired nrow = rastIn@nrow 
##       The remaining inputs are the arguments for itcIMG... for descriptions see https://cran.r-project.org/web/packages/itcSegment/itcSegment.pdf
##       
##    outputs:
##       OP = list with two named "cells", OP$Raster is the resampled raster used for the segmentation, OP$ITC is the output from the itcIMG function

AnnaShrubITC <- function(rastIn, ncol, nrow, espg, searchWinSize = 3, TRESHSeed = 0.4, TRESHCrown = 0.4, DIST = 10, th = 0, ischm = T){
   
   ## get values from where they don't exist and put them into themselves.... also makes all values positive
   values(rastIn) <- (getValues(rastIn) + abs(min(getValues(rastIn),na.rm = T))) 

   ## make a "dummy" raster with the desired number of rows and columns
   s <- raster(nrow=nrow, ncol=ncol)

   ## make "template" raster
   lil <- rastIn

   ## modify rows and columns
   lil@ncols <- s@ncols
   lil@nrows <- s@nrows

   ## resample the raster (for faster computation times)
   t <- resample(rastIn, lil, method = 'bilinear')

   #### perform itc segmentation
   OPitc <- itcIMG(t, epsg = epsg, searchWinSize = searchWinSize, TRESHSeed = TRESHSeed, TRESHCrown = TRESHCrown, DIST = DIST, th = th, ischm = ischm)
   
   # create output object comprising the resampled raster and the output  SpatialPolygonsDataFrame from "itcIMG"
   OP <- list(t, OPitc)
   names(OP) <- c("Raster","ITC")

   # return output
   return(OP) 
}

test <- AnnaShrubITC(dt2, 100, 100, 2789)

# inspect output
test


# visualize
plot(test$Raster)
plot(test$ITC, add = T)


# shrub heights and crown areas
test@data$Height_m
test@data$CA_m2











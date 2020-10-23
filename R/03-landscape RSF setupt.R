#extracting used and available points for the landscape scale RSF

#install packages and set utm
libs <- c('data.table', 'raster','rgdal','sp','broom','ggplot2','dplyr')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

#import PP raster files
PPcarbon<-raster("Input/pp_QtyC.tif")
PPnitrogen<-raster("Input/pp_PctN.tif")
PPcarbon<-projectRaster(PPcarbon,crs=utm21N)
PPnitrogen<-projectRaster(PPnitrogen,crs=utm21N)

#import OMP raster files
OMPcarbon<-raster("Input/omp_QtyC.tif")
OMPnitrogen<-raster("Input/omp_PctN.tif")
OMPcarbon<-projectRaster(OMPcarbon,crs=utm21N)
OMPnitrogen<-projectRaster(OMPnitrogen,crs=utm21N)

#Import homerange scale datasheet
homerange<-readRDS("Cleaned/hrRSFsetup.rds")

## Turning OMP raster files into data.tables with all raster cell values
OMPcarbon<-as.data.frame(OMPcarbon, xy=TRUE, rownames=NULL, na.rm=TRUE)
names(OMPcarbon)<-c("X","Y","C")
OMPnitrogen<-as.data.frame(OMPnitrogen, xy=TRUE, rownames=NULL, na.rm=TRUE)
names(OMPnitrogen)<-c("X","Y","N")
OMP<-data.table(OMPcarbon$X, OMPcarbon$Y, OMPcarbon$C, OMPnitrogen$N)
names(OMP)<-c("X","Y","C","N")
OMP[, Zone:="OMP"]

## Turning PP raster files into data.tables with all raster cell values
PPcarbon<-as.data.frame(PPcarbon, xy=TRUE, rownames=NULL, na.rm=TRUE)
names(PPcarbon)<-c("X","Y","C")
PPnitrogen<-as.data.frame(PPnitrogen, xy=TRUE, rownames=NULL, na.rm=TRUE)
names(PPnitrogen)<-c("X","Y","N")
PP<-data.table(PPcarbon$X, PPcarbon$Y, PPcarbon$C, PPnitrogen$N)
names(PP)<-c("X","Y","C","N")
PP[, Zone:="PP"]

#Combining all OMP and PP raster values into one datatable
PPandOMP<-rbindlist(list(PP, OMP), use.names = TRUE, fill=TRUE)
PPandOMP[, Status:=FALSE]


#take random sample from this combined datatable, these will become the available points
  #in the landscape scale RSF
#Random sample of 20,000 points across both landscapes
samplePPandOMP<-PPandOMP[sample(1:nrow(PPandOMP), 20000, replace=FALSE),]


#since this spreadsheet lists the within home range points as the false (or available) points
  #we take those FALSE points to be our used points, and change their status to FALSE
used<-homerange[Status==FALSE]
used[,Status:=TRUE]

#combine the used home range points and the landscape sample into one datatalbe
landscapeRSF<-rbindlist(list(used, samplePPandOMP), use.names = TRUE, fill=TRUE)


#save the combined OMP and PP rasters
#export this massive datatable as an RDS 
saveRDS(PPandOMP, "Cleaned/LandscapeAvailable.rds")

#save the dataset for landscape scale RSF models
saveRDS(landscapeRSF,"Cleaned/lnRSFsetup.rds")
fwrite(landscapeRSF, "Cleaned/lnRSFsetup.csv")

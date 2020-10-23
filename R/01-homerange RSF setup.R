#extracting used and available points for the home range scale RSF

#load packages and set utm
libs <- c('data.table', 'raster','leaflet','rgdal','sp','broom','ggplot2')
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


#### Extracting carbon and nitrogen values for used OMP locs #####

#read in PP moose gps points
PP.moose.points<-readRDS("Cleaned/PP_moose_points.rds")
#creating only X Y location datatable for extraction function
PPusedpoints<-data.table(PP.moose.points$EASTING, PP.moose.points$NORTHING)
names(PPusedpoints)<-c("x1","x2")
#extracting carbon and making it datatable
PPusedcarbon<-extract(PPcarbon,PPusedpoints)
PPusedcarbon<-as.data.table(PPusedcarbon)
names(PPusedcarbon)<-c("C")
#extracting nitrogen and making it a datatable
PPusednitrogen<-extract(PPnitrogen,PPusedpoints)
PPusednitrogen<-as.data.table(PPusednitrogen)
names(PPusednitrogen)<-c("N")


#### Extracting carbon and nitrogen values for used OMP locs #####

#read in OMP moose gps points
OMP.moose.points<-readRDS("Cleaned/OMP_moose_points.rds")
#creating only X Y location datatable for extraction function
OMPusedpoints<-data.table(OMP.moose.points$EASTING, OMP.moose.points$NORTHING)
names(OMPusedpoints)<-c("x1","x2")
#extracting carbon and making it datatable
OMPusedcarbon<-extract(OMPcarbon,OMPusedpoints)
OMPusedcarbon<-as.data.table(OMPusedcarbon)
names(OMPusedcarbon)<-c("C")
#extracting nitrogen and making it a datatable
OMPusednitrogen<-extract(OMPnitrogen,OMPusedpoints)
OMPusednitrogen<-as.data.table(OMPusednitrogen)
names(OMPusednitrogen)<-c("N")


######Creating PP Used datasheet with ID, X, C, N columns
PPused<- data.table(PP.moose.points$ID, PP.moose.points$EASTING, PP.moose.points$NORTHING, PPusedcarbon$C, PPusednitrogen$N)
names(PPused)<-c("ID","X","Y","C","N")
PPused<-PPused[!is.na(C)]
PPused<-PPused[!is.na(N)]
#Creating additional column for status- this is so we can add unused points later
PPused[, Status := TRUE]
PPused[, Zone := "PP"]

######Creating OMP Used datasheet with ID, X, C, N columns
OMPused<- data.table(OMP.moose.points$ID, OMP.moose.points$EASTING, OMP.moose.points$NORTHING, OMPusedcarbon$C, OMPusednitrogen$N)
names(OMPused)<-c("ID","X","Y","C","N")
OMPused<-OMPused[!is.na(C)]
OMPused<-OMPused[!is.na(N)]
#Creating additional column for status- this is so we can add unused points later
OMPused[, Status := TRUE]
OMPused[, Zone:= "OMP"]

### Extracting carbon and nitrogen values for Available Locs ####

#read in PP moose homerange RDS file and set projection
PPmooseHR<-readRDS("Cleaned/PP_homeranges.rds")
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
proj4string(PPmooseHR) <- CRS(utm21N)

#read in OMP moose homerange RDS and set projection
OMPmooseHR<-readRDS("Cleaned/OMP_homeranges.rds")
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
proj4string(OMPmooseHR) <- CRS(utm21N)


####Lay out PP available points in a grid. keeps density of sampled points equal across HR's
PPavailablePoints <- spsample(PPmooseHR, 6000, type = 'regular')
ovrPP <- rbindlist(over(PPavailablePoints, PPmooseHR, returnList = TRUE),
                 idcol = 'ptID')
ovrPP[, ptID := as.integer(ptID)]
PPavailableCoords <- data.table(ptID = 1:length(PPavailablePoints),
                                X = PPavailablePoints@coords[,1],
                                Y = PPavailablePoints@coords[,2])
PPavailable <- merge(ovrPP, PPavailableCoords, by = 'ptID')

#naming the columns to be same as the used datatables
setnames(PPavailable, c('ptID','ID', 'area', 'X','Y'))


####Lay out OMP available points in a grid. keeps density of sampled points equal across HR's
OMPavailablePoints <- spsample(OMPmooseHR, 5375, type = 'regular')
ovrPP <- rbindlist(over(OMPavailablePoints, OMPmooseHR, returnList = TRUE),
                   idcol = 'ptID')
ovrPP[, ptID := as.integer(ptID)]
OMPavailableCoords <- data.table(ptID = 1:length(OMPavailablePoints),
                                X = OMPavailablePoints@coords[,1],
                                Y = OMPavailablePoints@coords[,2])
OMPavailable <- merge(ovrPP, OMPavailableCoords, by = 'ptID')
#naming the columns to be same as the used datatables
setnames(OMPavailable, c('ptID','ID', 'area', 'X','Y'))


#to see visualized available points
OMPtidymoose<-tidy(OMPmooseHR)
ggplot(OMPtidymoose) + 
  geom_polygon(aes(long,lat,group=group,fill=id),alpha=.5, color="black") + 
  geom_point(aes(X, Y), color="dark red", data = OMPavailable)+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())

PPtidymoose<-tidy(PPmooseHR)
ggplot(PPtidymoose) + 
  geom_polygon(aes(long,lat,group=group,fill=id),alpha=.5, color="black") + 
  geom_point(aes(X, Y), color="dark red", data = PPavailable)+
  theme_bw()

#adding on the Status column which all will be false
PPavailable[, Status := FALSE]
PPavailable[, Zone := "PP"]
OMPavailable[, Status := FALSE]
OMPavailable[, Zone := "OMP"]

#making a subset X Y datatable just for the raster::extract function
PPavailpoints<-data.table(PPavailable$X, PPavailable$Y)
names(PPavailpoints)<-c("x1","x2")
OMPavailpoints<-data.table(OMPavailable$X, OMPavailable$Y)
names(OMPavailpoints)<-c("x1","x2")

#extracting from carbon raster at all available points
PPavailcarbon<-extract(PPcarbon,PPavailpoints)
PPavailcarbon<-as.data.table(PPavailcarbon)
names(PPavailcarbon)<-c("C")
OMPavailcarbon<-extract(OMPcarbon,OMPavailpoints)
OMPavailcarbon<-as.data.table(OMPavailcarbon)
names(OMPavailcarbon)<-c("C")

#extracting from nitrogen raster at all available points
PPavailnitrogen<-extract(PPnitrogen,PPavailpoints)
PPavailnitrogen<-as.data.table(PPavailnitrogen)
names(PPavailnitrogen)<-c("N")
OMPavailnitrogen<-extract(OMPnitrogen,OMPavailpoints)
OMPavailnitrogen<-as.data.table(OMPavailnitrogen)
names(OMPavailnitrogen)<-c("N")

#adding these Carbon and nitrogen values to the existing available datatable
PPavailable[, C := PPavailcarbon$C]
PPavailable[, N := PPavailnitrogen$N]
OMPavailable[, C := OMPavailcarbon$C]
OMPavailable[, N := OMPavailnitrogen$N]

#removing all points that didnt have raster values
PPavailable<-PPavailable[!is.na(C)]
PPavailable<-PPavailable[!is.na(N)]
OMPavailable<-OMPavailable[!is.na(C)]
OMPavailable<-OMPavailable[!is.na(N)]


###### Combine the used and available datatables and save

RSF<- rbindlist(list(PPused, OMPused, PPavailable, OMPavailable), fill = TRUE, use.names = TRUE)
saveRDS(RSF,"Cleaned/hrRSFsetup.rds")
fwrite(RSF,"Cleaned/hrRSFsetup.csv")


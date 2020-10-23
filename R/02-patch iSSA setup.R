#fitting the step lengths and turn angles to gamma distribution.
#laying out available steps based on those distribution
#extracting stoich values from those unused steps

#load packages and set utm
libs<-c('lubridate','raster','amt','data.table')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

#read in rastor layers
PPcarbon<-raster("Input/pp_QtyC.tif")
PPnitrogen<-raster("Input/pp_PctN.tif")
OMPcarbon<-raster("Input/omp_QtyC.tif")
OMPnitrogen<-raster("Input/omp_PctN.tif")

#project rasters
PPcarbon<-projectRaster(PPcarbon,crs=utm21N)
OMPcarbon<-projectRaster(OMPcarbon,crs=utm21N)
PPnitrogen<-projectRaster(PPnitrogen,crs=utm21N)
OMPnitrogen<-projectRaster(OMPnitrogen,crs=utm21N)

#read in full moose gps dataframe
#this data has already been cleaned
PPmoose<-readRDS("Cleaned/PP_moose_full.rds")
OMPmoose<-readRDS("Cleaned/OMP_moose_full.rds")


###If you want to see what the track itseld looks like before making steps:
#summarize_sampling_rate(tracktest)

#function ExtractTrackCovariates will put dataset into track format,
#calculates steps, creates random steps, and extracts stoich layer. 
ExtractTrackCovariates <- function(NumbRandSteps, x.col, y.col, date.col, raster1, raster2) {
  #create track from dataset
  trk <- track(x.col, y.col, date.col) %>% 
    #function steps turns locs into steps
    steps()
    #to fix time diff problem, replace dt_ column with hours as units specified
    trk$dt_ <- difftime(trk$t2_, trk$t1_, unit='hours')
    trk<- trk %>% filter(dt_<2.5)
    trk %>%
    # creates a set number of steps from distribution of taken steps
    random_steps(n = NumbRandSteps) %>%
    #extracts the raster values at these steps
    extract_covariates(raster1) %>%
    extract_covariates(raster2)
}

#run function by moose ID
PPlocs <- PPmoose[, ExtractTrackCovariates( 10, x.col = EASTING, y.col = NORTHING, date.col = datetime, 
        raster1=PPcarbon, raster2=PPnitrogen), 
        by = ID]
OMPlocs <- OMPmoose[, ExtractTrackCovariates( 10, x.col = EASTING, y.col = NORTHING, date.col = datetime, 
        raster1=OMPcarbon, raster2=OMPnitrogen), 
        by = ID]

#to check the step time length calculations seem correct
knitr::kable(OMPlocs[, range(dt_), by = ID])
knitr::kable(PPlocs[, range(dt_), by=ID])

#transform turn angles and step lengths for logistic regression
PPlocs <- PPlocs %>% 
  mutate(cos_ta = cos(ta_), 
         log_sl = log(sl_)) 
OMPlocs <- OMPlocs %>% 
  mutate(cos_ta = cos(ta_), 
         log_sl = log(sl_)) 

PPlocs$step_id_<-as.factor(PPlocs$step_id_)
OMPlocs$step_id_<-as.factor(OMPlocs$step_id_)

#turn final product into data.table and rename C and N columns
PPlocs<-data.table(PPlocs)
PPlocs$C<-PPlocs$pp_QtyC
PPlocs$pp_QtyC<-NULL
PPlocs$N<-PPlocs$pp_PctN
PPlocs$pp_PctN<-NULL

OMPlocs<-data.table(OMPlocs)
OMPlocs$C<-OMPlocs$omp_QtyC
OMPlocs$omp_QtyC<-NULL
OMPlocs$N<-OMPlocs$omp_PctN
OMPlocs$omp_PctN<-NULL

#create new column called ind.step.id because step_id_ repeated over moose ind's
PPlocs[, ind.step.id := paste(ID, step_id_, sep = "_")]
OMPlocs[, ind.step.id := paste(ID, step_id_, sep = "_")]

#removing locs which were in blank areas of raster layer
PPlocs<-PPlocs[!is.na(C)]
PPlocs<-PPlocs[!is.na(N)]
OMPlocs<-OMPlocs[!is.na(C)]
OMPlocs<-OMPlocs[!is.na(N)]

OMPlocs<-OMPlocs[!log_sl<0]
PPlocs<-PPlocs[!log_sl<0]

###  Combine the PP and OMP step data.tables into one
locs<-rbindlist(list(PPlocs, OMPlocs), use.names = TRUE, fill = TRUE)

### Save the final step selection data sheet
saveRDS(locs, "Cleaned/iSSAsetup.rds")
fwrite(locs, "Cleaned/iSSAsetup.csv")








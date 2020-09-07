library(raster)
library(tidyverse)

source("D:/TFM Data Science/config.R")



list_bioclims <- list.files(file.path(path_bioclim), full.names = T)
raster_example <- raster(list_bioclims[1])

y_raster <- raster_example

y_data <- yFromCell(raster_example,cell=1:ncell(raster_example))
y_raster[1:ncell(raster_example)] <- abs(y_data)

background_map <- reclassify(raster_example, matrix(c(-100, 100, 0), ncol=3))
y_raster_clean <- y_raster+background_map

rm(background_map, raster_example)



pmt <- proc.time()
beginCluster()
all_bio <- brick(stack(list_bioclims))
endCluster()
proc.time() - pmt


all_bio <- addLayer(y_raster_clean, all_bio)

names(all_bio) <- c("y", sub("wc2.1_5m_", "", names(all_bio)[2:20]))
names(all_bio)
all_bio <- crop(all_bio, crop_extention)

models_list <- list.files(path_models, full.names = T)
KNN_models_list <- grep("KNN", models_list, value = T)

reg_rf <- readRDS(grep("RF", models_list, value = T))
reg_KNN_small <-readRDS(grep("small", KNN_models_list, value = T))
reg_KNN <-readRDS(grep("70K", KNN_models_list, value = T))
reg_tree <- readRDS(grep("TREE", models_list, value = T))


####### Load NPP map #######

raw_npp <- raster(file.path(path_NPP, "MOD17A3_Science_NPP_mean_00_15.tif"))
raw_npp <- crop(raw_npp,crop_extention)
clean_npp <- reclassify(raw_npp, matrix(c(32699, 100000, NA), ncol = 3))*0.1 #remove fill values, transform to g C / m / year

clean_npp_agg <- aggregate(clean_npp, fact=10, fun=mean)
plot(clean_npp_agg, main="NPP map")

npp_area <- reclassify(clean_npp_agg, matrix(c(0, 2600, 0), ncol = 3))*0.1 #remove fill values, transform to g C / m / year


###### RF prediction #######
pmt <- proc.time()
beginCluster()
RF_prediction <- raster::predict(all_bio, reg_rf_tune)
endCluster()
proc.time() - pmt

plot(RF_prediction, main="RF Prediction map")

RF_PMAE_map <- abs(clean_npp_agg - RF_prediction)/clean_npp_agg 
RF_error_map <- (clean_npp_agg - RF_prediction)

plot(RF_error_map, main="Error map")
plot(RF_error_map/clean_npp_agg, zlim=c(-50, 50))
plot(RF_PMAE_map, zlim=c(0,1000))


plot(clean_npp_agg, main="NPP map", zlim=c(0,2520))
plot(RF_prediction, main="RF Prediction map", zlim=c(0,2520))
plot(RF_error_map, main="Error map", zlim=c(-1000,2520))
plot(RF_PMAE_map, zlim=c(0,2))


hist(abs(RF_error_map), breaks=50)
mean(RF_PMAE_map)
mean(getValues(RF_PMAE_map))

writeRaster(RF_prediction, file.path(path_predictions, "RF_70K.tiff"))



####### KNN prediction ######

normalize_minmax <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

all_bio_norm <- calc(all_bio, normalize_minmax)
names(all_bio_norm) <- names(all_bio)

pmt <- proc.time()
beginCluster()
KNN_prediction <- raster::predict(all_bio_norm, reg_KNN_norm)
KNN_prediction <- exp(KNN_prediction)
endCluster()
proc.time() - pmt

plot(KNN_prediction, main="KNN Prediction map")

KNN_PMAE_map <- abs(clean_npp_agg - KNN_prediction)/clean_npp_agg 
KNN_error_map <- (clean_npp_agg - KNN_prediction)


plot(clean_npp_agg, main="NPP map", zlim=c(0,2560))
plot(KNN_prediction, main="KNN Prediction map", zlim=c(0,2560))
plot(KNN_error_map, main="Error map")#, zlim=c(-1000,2520))
plot(KNN_PMAE_map, zlim=c(0,2))

writeRaster(KNN_prediction, file.path(path_predictions, "KNN_70K.tiff"))



###### DECISION TREE prediction #######
pmt <- proc.time()
beginCluster()
TREE_prediction <- raster::predict(all_bio, reg_tree)
endCluster()
proc.time() - pmt

plot(TREE_prediction, main="D.TREE Prediction map")


###### NPP Miami Model ######
npp_miami <- raster(file.path(path_project, "NPP_miami_model","NPP_miami_model.tif"))
npp_miami <- crop(npp_miami, crop_extention)

plot(npp_miami, main="NPP Miami Model", zlim=c(0,2560))

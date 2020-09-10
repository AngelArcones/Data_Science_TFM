library(raster)
library(tidyverse)
library(randomForest)

source("D:/TFM Data Science/config.R")



#Get list of future scenarios' maps
ssp_scenarios <- list.files(file.path(path_future_scenarios))
ssp_scenarios_names <- sub("wc2.1_2.5m_bioc_MIROC6_","",ssp_scenarios)

#Load NPP map for the extent
clean_npp <- raster(file.path(path_NPP, "NPP_map_clean.tif"))
map_extent <- extent(clean_npp)
rm(clean_npp)

#Create 'y' variable map

raster_example <- raster(file.path(path_future_scenarios, ssp_scenarios[1]), band=1)

y_raster <- raster_example

y_data <- yFromCell(raster_example,cell=1:ncell(raster_example))
y_raster[1:ncell(raster_example)] <- abs(y_data)

background_map <- reclassify(raster_example, matrix(c(-100, 100, 0), ncol=3))
y_raster_clean <- y_raster+background_map
rm(background_map, raster_example)



#Load models
models_list <- list.files(path_models, full.names = T)

reg_rf_tune <- readRDS(grep("RF_train_tune_35k", models_list, value = T))

beginCluster()
for(i in 1:4){
  pmt <- proc.time()
  message(paste0(Sys.time(), ": creating brick for ", ssp_scenarios_names[i]))
  
  temp_brick <- brick(raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=1),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=2),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=3),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=4),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=5),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=6),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=7),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=8),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=9),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=10),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=11),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=12),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=13),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=14),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=15),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=16),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=17),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=18),
                      raster(file.path(path_future_scenarios, ssp_scenarios[i]), band=19))
  
  
  temp_brick <- addLayer(y_raster_clean, temp_brick)
  
  names(temp_brick) <- c("y", paste0("bio_", 1:19))
  
  temp_brick <- crop(temp_brick, map_extent)
  temp_brick <- aggregate(temp_brick, factor=2, fun=mean)
  
  message(paste0(Sys.time(), ": prediction for ", ssp_scenarios_names[i]))
  RF_prediction_future <- raster::predict(temp_brick, reg_rf_tune)
  
  
  print(proc.time() - pmt)
  
  message(paste0(Sys.time(), ": saving raster for ", ssp_scenarios_names[i]))
  writeRaster(RF_prediction_future, file.path(path_predictions, paste0("RF_pred_", ssp_scenarios_names[i])), overwrite = T)
  
  
}
endCluster()



library(raster)
library(dismo)
library(tidyverse)

source("D:/TFM Data Science/config.R")


source(file.path(path_project, "Miami_model_functions.R"))


#Load and reclassify NPP raster data (~ 1km x 1km)
raw_npp <- raster(file.path(path_NPP, "MOD17A3_Science_NPP_mean_00_15.tif"))
#plot(raw_npp)

clean_npp <- reclassify(raw_npp, matrix(c(32699, 100000, NA), ncol = 3))*0.1 #remove fill values, transform to g C / m / year

#clean_npp <- crop(clean_npp, crop_extention)
clean_npp <- aggregate(clean_npp, fact=10, fun=mean) #aggregate to 10x10km
writeRaster(clean_npp, file.path(path_NPP, "NPP_map_clean.tif"))
plot(clean_npp)


#Create sampling points
sample_points <- randomPoints(mask=clean_npp, n = 35000)
plot(clean_npp, main = "Net primary produtivity (NPP)\nas gr C / m^2 / year", sub="Dots represent sampling points")
points(sample_points, pch = ".")

###### Data frame generation #####


points_df <- as.data.frame(sample_points)


#Extract NPP data
NPP_df <- data.frame(raster::extract(clean_npp, sample_points))
colnames(NPP_df) <- "NPP"
points_df <- cbind(points_df, NPP_df)


list_bioclim <- list.files(path_bioclim, pattern = "*.tif")
#prefix <- substr(list_vars[1], 1, 9)

#Calculate Miami model values

bio_tmean <- raster(file.path(path_bioclim, list_bioclim[grep("bio_1.tif", list_bioclim)])) #Annual mean temp in ºCx10
#bio_tmean <- bio_tmean*0.1 #convert to ºC

bio_prec <- raster(file.path(path_bioclim, list_bioclim[grep("bio_12.tif", list_bioclim)])) #Annual precipitation in mm


beginCluster()
npp_tmean_miami <- calc(bio_tmean, NPP_temp)
npp_prec_miami <- calc(bio_prec, NPP_prec)

npp_brick <- brick(npp_tmean_miami, npp_prec_miami)
message("NPP finished")
endCluster()

plot(npp_brick)

beginCluster()
npp_miami <- calc(npp_brick, min)
endCluster()

plot(npp_miami)

#save data
writeRaster(npp_miami, file.path(path_project, "NPP_miami_model","NPP_miami_model.tif"))


rm(bio_tmean, bio_prec, npp_prec_miami, npp_tmean_miami, npp_brick, npp_miami)



#Extract miami model data

npp_miami <- raster(file.path(path_project, "NPP_miami_model","NPP_miami_model.tif"))

NPP_miami_df <- data.frame(raster::extract(npp_miami, sample_points))
colnames(NPP_miami_df) <- "Miami_NPP"

points_df <- cbind(points_df, NPP_miami_df)


#Get bioclim data in the table

for(i in 1:length(list_bioclim)){
  
  map_data <- raster(file.path(path_bioclim, list_bioclim[i]))
  bioclim_df <- data.frame(raster::extract(map_data, sample_points))
  #colnames(bioclim_df) <- list_vars[i] %>% sub(prefix, "", .) %>% sub(".tif", "", .)
  var_name <- strsplit(list_bioclim[i], "_") %>% unlist() %>% grep(".tif", ., value = T) %>% sub(".tif", "", .)
  colnames(bioclim_df) <- paste0("bio_", var_name)
  points_df <- cbind(points_df, bioclim_df)
  message(i, "/19 completed")
}

write.csv(points_df, file.path(path_project,"35k_data.csv"), row.names = F, )



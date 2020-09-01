library(raster)

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




pmt <- proc.time()
beginCluster()
my_prediction <- raster::predict(all_bio, reg_rf)
endCluster()
proc.time() - pmt

plot(my_prediction, main="Prediction map")


raw_npp <- raster(file.path(path_NPP, "MOD17A3_Science_NPP_mean_00_15.tif"))
clean_npp <- reclassify(raw_npp, matrix(c(32699, 100000, NA), ncol = 3))*0.1 #remove fill values, transform to g C / m / year

clean_npp_agg <- aggregate(clean_npp, fact=10, fun=mean)
plot(clean_npp_agg, main="NPP map")

RF_PMAE_map <- abs(clean_npp_agg - my_prediction)/clean_npp_agg 
RF_error_map <- (clean_npp_agg - my_prediction)

plot(RF_error_map, main="Error map")
plot(RF_error_map/clean_npp_agg, zlim=c(-50, 50))
plot(RF_PMAE_map, zlim=c(0,1000))


plot(clean_npp_agg, main="NPP map", zlim=c(0,2520))
plot(my_prediction, main="Prediction map", zlim=c(0,2520))
plot(RF_error_map, main="Error map", zlim=c(-1000,2520))
plot(RF_PMAE_map, zlim=c(0,2))


hist(abs(RF_error_map), breaks=50)
mean(RF_PMAE_map)
mean(getValues(RF_PMAE_map))

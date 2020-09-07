##########################################################
######## Config file for NPP estimation using ML #########
##########################################################

#Path to project folder

path_project <- "D:/TFM Data Science"


#Paths to data folders

path_NPP <- file.path(path_project, "NPP_tif_data")
if(!dir.exists(path_NPP)){
  dir.create(path_NPP)
}

path_bioclim <- file.path(path_project, "bioclims_2.5")
if(!dir.exists(path_bioclim)){
  dir.create(path_bioclim)
}

path_models <- file.path(path_project, "models")
if(!dir.exists(path_models)){
  dir.create(path_models)
}

path_predictions <- file.path(path_project, "predictions")
if(!dir.exists(path_predictions)){
  dir.create(path_predictions)
}

#Set seed
seed <- 42

#Extention of the cropped area
crop_extention <- extent(-10, 4.5, 35.6, 44) #Xmin, Xmax, Ymin, Ymax

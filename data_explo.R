##########################################
#           Data Exploration             #
##########################################

library(tidyverse)
library(Boruta)
library(party)
library(corrplot)
library(psych)


source("D:/TFM Data Science/config.R")



points_df  <- read.csv(file.path(path_project, "first_data.csv"), header = T)


#Number of NAs
points_df %>% is.na() %>% sum()

#Number of points with NPP = 0 or <=1
points_df %>% filter(NPP==0) %>% nrow()
points_df %>% filter(NPP<=1) %>% nrow()

#Prepare the main data frame
df_modelo <- points_df %>% select(-x, -Miami_NPP) %>% filter(NPP > 1)
df_modelo$y <- abs(df_modelo$y)
df_modelo <- na.omit(df_modelo)


######### Data exploration ###########

#### CORRELATIONS ####

df_modelo %>% cor() %>% corrplot(method="circle", type="upper")
df_modelo %>% cor() %>% corrplot(method="color", type="upper",addCoef.col = "black",diag = F)


temp_df <- df_modelo %>% select("NPP", c(paste0("bio_",1:11)))
prec_df <- df_modelo %>% select("NPP", c(paste0("bio_",12:19)))

#Correlatons between temperature variables

temp_df %>% cor() %>% corrplot(method="number", type = "upper")



#bio_3 is bio_2/bio_7
#bio_7 is bio_5_bio6
#bio_1 is highly correlated with everything
#bio_11 is practically  the same as bio_6

temp_df %>% select(-bio_1, -bio_3, -bio_7, -bio_11) %>% cor() %>% corrplot(method="number", type = "upper")
temp_df %>% select(-bio_1, -bio_3, -bio_7, -bio_11) %>% pairs.panels(method = "pearson",
                                                                     density = TRUE, ellipses = TRUE)
#Correlatons between precipitation variables
prec_df %>% cor() %>% corrplot(method="number", type = "upper")




######### Variable selection #########

### Boruta method ###

boruta_output <- Boruta(NPP~., data=df_modelo_norm, doTrace=2)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

### RF method ###
cpt <- proc.time()
rf1 <- cforest(NPP~., data=df_modelo_norm, control=cforest_unbiased(mtry=2,ntree=50))
proc.time() - cpt


cpt <- proc.time()
varimp(rf1, conditional=TRUE)
proc.time() - cpt
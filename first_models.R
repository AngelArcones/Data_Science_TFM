library(raster)
library(caret)
library(tidyverse)
library(randomForest)


source("D:/TFM Data Science/config.R")


points_df  <- read.csv(file.path(path_project, "35k_data.csv"), header = T)


#Prepare the main data frame
df_modelo <- points_df %>% dplyr::select(-x, -Miami_NPP) %>% filter(NPP > 1)
df_modelo$y <- abs(df_modelo$y)
df_modelo <- na.omit(df_modelo)




#Create sampling index for train-test split
set.seed(seed)
sample <- sample.int(n = nrow(points_df), size = floor(.80*nrow(points_df)), replace = F)


###### Miami Model evaluation #########

NPP_eval <- points_df %>% dplyr::select(NPP, Miami_NPP) %>% filter(NPP > 1 & Miami_NPP > 1) %>% na.omit() 

Miami_PMAE <- mean(abs(NPP_eval$NPP - NPP_eval$Miami_NPP)/NPP_eval$NPP)

results_miami <- data.frame("Model"="Miami model",
                             "PMAE Train"=NA,
                             "PMAE Test"=mean(abs(NPP_eval$NPP - NPP_eval$Miami_NPP)/NPP_eval$NPP),
                             "RMSE Train"=NA,
                             "RMSE Test"=sqrt(mean((NPP_eval$NPP - NPP_eval$Miami_NPP)^2)))


hist_MIAMI <- abs(NPP_eval$NPP - NPP_eval$Miami_NPP)/NPP_eval$NPP
hist(hist_MIAMI[hist_MIAMI<6]*100, breaks =200, xlim = c(0,600), xlab = "% de desviación", main = "Desviación de las estimas del Miami Model respecto a NPP real")
abline(v=mean(hist_MIAMI)*100, lty="dashed")
abline(v=median(hist_MIAMI)*100, col="red")

#hist(hist_MIAMI[hist_MIAMI<6 & hist_MIAMI >-6]*100, breaks =200, xlim = c(-600,600), xlab = "% de desviación", main = "Desviación de las estimas del Miami Model respecto a NPP real")
hist(hist_MIAMI*100, breaks =2000, xlim = c(-1000,600), xlab = "% de desviación", main = "Desviación de las estimas del Miami Model respecto a NPP real")



######## NORMALIZATION ##########


df_modelo_norm <- df_modelo

normalize_minmax <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}


for(i in 1:ncol(df_modelo_norm)){
  if(names(df_modelo_norm)[i] != "NPP"){
    df_modelo_norm[,i] <- normalize_minmax(df_modelo_norm[,i])
  } else{
    df_modelo_norm[,i] <- log(df_modelo_norm[,i])
  }
}




###################################################################
##################        MODELS            #######################
###################################################################

###### Naive model (median) #######
df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])


results_median <- data.frame("Model"="Naive (median)",
                          "PMAE Train"=mean(abs(df_train$NPP - median(df_train$NPP))/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - median(df_test$NPP))/df_test$NPP),
                          "RMSE Train"=sqrt(mean((df_train$NPP - median(df_train$NPP))^2)),
                          "RMSE Test"=sqrt(mean((df_test$NPP - median(df_test$NPP))^2)))




###### Linear model (1 var) #######
df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

reg_lm <- lm(NPP~bio_12, data=df_train)
reg_lm


results_lm <- data.frame("Model"="LM (1 var)",
                          "PMAE Train"=mean(abs(df_train$NPP - predict(reg_lm, df_train))/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - predict(reg_lm, df_test))/df_test$NPP),
                         "RMSE Train"=sqrt(mean((df_train$NPP - predict(reg_lm, df_train))^2)),
                         "RMSE Test"=sqrt(mean((df_test$NPP - predict(reg_lm, df_test))^2)))

###### Linear model (multivar) #######
df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

reg_lm_mv <- lm(NPP~., data=df_train)
reg_lm_mv


results_lm_mv <- data.frame("Model"="Multivar LM",
                         "PMAE Train"=mean(abs(df_train$NPP - predict(reg_lm_mv, df_train))/df_train$NPP),
                         "PMAE Test"=mean(abs(df_test$NPP - predict(reg_lm_mv, df_test))/df_test$NPP),
                         "RMSE Train"=sqrt(mean((df_train$NPP - predict(reg_lm_mv, df_train))^2)),
                         "RMSE Test"=sqrt(mean((df_test$NPP - predict(reg_lm_mv, df_test))^2)))

###### Linear model (multivar with transformation) #######
df_train_norm <- na.omit(df_modelo_norm[sample, ])
df_test_norm  <- na.omit(df_modelo_norm[-sample, ])

reg_lm_mv_t <- lm(NPP~., data=df_train_norm)
reg_lm_mv_t


results_lm_mv_t <- data.frame("Model"="Multivar LM (trans)",
                            "PMAE Train"=mean(abs(df_train$NPP - exp(predict(reg_lm_mv_t, df_train_norm)))/df_train$NPP),
                            "PMAE Test"=mean(abs(df_test$NPP - exp(predict(reg_lm_mv_t, df_test_norm)))/df_test$NPP),
                            "RMSE Train"=sqrt(mean((df_train$NPP - exp(predict(reg_lm_mv_t, df_train_norm)))^2)),
                            "RMSE Test"=sqrt(mean((df_test$NPP - exp(predict(reg_lm_mv_t, df_test_norm)))^2)))


###### KNN ######

df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

pmt <- proc.time()
reg_KNN <- caret::train(NPP~.,
                 data = df_train,
                 method = "knn",
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats=10), # Seleccionar metodo de resampling (#aqui: repeated 10-fold CV)
                 tuneGrid = expand.grid(k = seq(2,10))) # Generar la malla de valores de parámetros para buscar optimo
proc.time() - pmt


reg_KNN #ver resultados
plot(reg_KNN) #visualizacion gráfica


results_KNN <- data.frame("Model"="KNN",
                          "PMAE Train"=mean(abs(df_train$NPP - predict(reg_KNN, df_train))/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - predict(reg_KNN, df_test))/df_test$NPP),
                          "RMSE Train"=sqrt(mean((df_train$NPP - predict(reg_KNN, df_train))^2)),
                          "RMSE Test"=sqrt(mean((df_test$NPP - predict(reg_KNN, df_test))^2)))


###### KNN transformed ######

#sample <- sample.int(n = nrow(points_df), size = floor(.80*nrow(points_df)), replace = F)
df_train_norm <- na.omit(df_modelo_norm[sample, ])
df_test_norm  <- na.omit(df_modelo_norm[-sample, ])

pmt <- proc.time()
beginCluster()
reg_KNN_norm <- train(NPP~.,
                 data = df_train_norm,
                 method = "knn",
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats=10), # Seleccionar metodo de resampling (#aqui: repeated 10-fold CV)
                 tuneGrid = expand.grid(k = seq(2,10))) # Generar la malla de valores de parámetros para buscar optimo
endCluster()
proc.time() - pmt


reg_KNN_norm #ver resultados
plot(reg_KNN_norm) #visualizacion gráfica


#Comparison

pred_log_train <- exp(predict(reg_KNN_norm, df_train_norm))
pred_log_test <- exp(predict(reg_KNN_norm, df_test_norm))


results_KNN_trans <- data.frame("Model"="KNN transformed",
                          "PMAE Train"=mean(abs(df_train$NPP - pred_log_train)/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - pred_log_test)/df_test$NPP),
                          "RMSE Train"=sqrt(mean((df_train$NPP - pred_log_train)^2)),
                          "RMSE Test"=sqrt(mean((df_test$NPP - pred_log_test)^2)))



saveRDS(reg_KNN_norm, file.path(path_models, "KNN_norm_train_35k.rds"))
PMAE_conv <- mean(abs(NPP_test - pred_log)/NPP_test)



hist(NPP_test - pred_log, breaks=100, xlab = "% de desviacion", main = "Desviación (en %) de la prediccion\nrespecto al dato real")

hist((abs(NPP_test - pred_log)/NPP_test)*100, xlim=c(0,600), breaks=500, xlab = "% de desviacion", main = "Desviación de las estimas del modelo KNN respecto al NPP real")
abline(v=PMAE_conv*100, lty="dashed")
median(abs(NPP_test - pred_log)/NPP_test)
abline(v=median(abs(NPP_test - pred_log)/NPP_test)*100, col = "red")




###### Regression Tree  CARET######

df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

pmt <- proc.time()
reg_tree <- train(NPP~., data = df_train,
                  method = "rpart",
                  trControl = trainControl("cv", number = 20),
                  tuneLength = 50)
proc.time() - pmt


reg_tree
plot(reg_tree)
plot(reg_tree$finalModel)
text(reg_tree$finalModel, use.n=TRUE, all=TRUE, cex=.6)

#Results
results_tree_car <- data.frame("Model"="Regression Tree",
                          "PMAE Train"=mean(abs(df_train$NPP - predict(reg_tree, df_train))/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - predict(reg_tree, df_test))/df_test$NPP),
                          "RMSE Train"=sqrt(mean((df_train$NPP - predict(reg_tree, df_train))^2)),
                          "RMSE Test"=sqrt(mean((df_test$NPP - predict(reg_tree, df_test))^2)))

hist((abs(NPP_test - NPP_pred_tree)/NPP_test)*100, xlim=c(0,600), breaks=500, xlab = "% de desviacion", main = "Desviación de las estimas del modelo KNN respecto al NPP real")
abline(v=PMAE_conv*100, lty="dashed")
median(abs(NPP_test - NPP_pred_tree)/NPP_test)
abline(v=median(abs(NPP_test - NPP_pred_tree)/NPP_test)*100, col = "red")

saveRDS(reg_tree, file.path(path_models, "TREE_train_35k.rds"))


####### regresion using randomForest #############

df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

reg_rf <- randomForest(NPP~., data=df_train)
reg_rf


plot(reg_rf)
varImpPlot(reg_rf)


#Results
results_tree_rf <- data.frame("Model"="RandomForest",
                               "PMAE Train"=mean(abs(df_train$NPP - predict(reg_rf, df_train))/df_train$NPP),
                               "PMAE Test"=mean(abs(df_test$NPP - predict(reg_rf, df_test))/df_test$NPP),
                              "RMSE Train"=sqrt(mean((df_train$NPP - predict(reg_rf, df_train))^2)),
                              "RMSE Test"=sqrt(mean((df_test$NPP - predict(reg_rf, df_test))^2)))



saveRDS(reg_rf, file.path(path_models, "RF_train_35k.rds"))


####### Random Forest with tunning ##########


df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])


# Algorithm Tune (tuneRF)
pmt <- proc.time()
set.seed(seed)
bestmtry <- tuneRF(df_train[,!names(df_train)=="NPP"], df_train$NPP, stepFactor=1.5, improve=1e-5, ntree=500)
proc.time() - pmt
print(bestmtry) # 7 o 10



reg_rf_tune <- randomForest(NPP~., data=df_train, mtry=7)
reg_rf_tune

#Results
results_tree_rf_tune <- data.frame("Model"="RandomForest with tunning",
                              "PMAE Train"=mean(abs(df_train$NPP - predict(reg_rf_tune, df_train))/df_train$NPP),
                              "PMAE Test"=mean(abs(df_test$NPP - predict(reg_rf_tune, df_test))/df_test$NPP),
                              "RMSE Train"=sqrt(mean((df_train$NPP - predict(reg_rf_tune, df_train))^2)),
                              "RMSE Test"=sqrt(mean((df_test$NPP - predict(reg_rf_tune, df_test))^2)))




saveRDS(reg_rf_tune, file.path(path_models, "RF_train_tune_35k.rds"))
# pmt <- proc.time()
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
# set.seed(42)
# mtry <- sqrt(ncol(df_train))
# rf_random <- train(NPP~., data=df_train, method="rf", tuneLength=15, trControl=control)
# proc.time() - pmt
# print(rf_random)
# plot(rf_random)

#Combined Results

all_results <- rbind(results_miami,
                     results_median,
                     results_lm,
                     results_lm_mv,
                     results_lm_mv_t,
                     results_KNN,
                     results_KNN_trans,
                     results_tree_car,
                     results_tree_rf,
                     results_tree_rf_tune)

write.csv(all_results, file.path(path_models, "Summary_all_models_metrics.csv"))

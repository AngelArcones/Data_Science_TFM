library(caret)
library(tidyverse)
library(rpart)
library(randomForest)
library(Boruta)
library(party)
library(xgboost)


source("D:/TFM Data Science/config.R")


points_df  <- read.csv(file.path(path_project, "69k_data.csv"), header = T)


#Prepare the main data frame
df_modelo <- points_df %>% select(-x, -Miami_NPP) %>% filter(NPP > 1)
df_modelo$y <- abs(df_modelo$y)
df_modelo <- na.omit(df_modelo)




#Create sampling index for train-test split
sample <- sample.int(n = nrow(points_df), size = floor(.80*nrow(points_df)), replace = F)


###### Miami Model evaluation #########

NPP_eval <- points_df %>% select(NPP, Miami_NPP) %>% filter(NPP > 1 & Miami_NPP > 1) %>% na.omit() 

Miami_PMAE <- mean(abs(NPP_eval$NPP - NPP_eval$Miami_NPP)/NPP_eval$NPP)

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
                          "PMAE Test"=mean(abs(df_test$NPP - median(df_test$NPP))/df_test$NPP))




###### Linear model (1 var) #######
df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

reg_lm <- lm(NPP~bio_12, data=df_train)
reg_lm


results_lm <- data.frame("Model"="LM (1 var)",
                          "PMAE Train"=mean(abs(df_train$NPP - predict(reg_lm, df_train))/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - predict(reg_lm, df_test))/df_test$NPP))

###### Linear model (multivar) #######
df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

reg_lm_mv <- lm(NPP~., data=df_train)
reg_lm_mv


results_lm_mv <- data.frame("Model"="Multivar LM",
                         "PMAE Train"=mean(abs(df_train$NPP - predict(reg_lm_mv, df_train))/df_train$NPP),
                         "PMAE Test"=mean(abs(df_test$NPP - predict(reg_lm_mv, df_test))/df_test$NPP))

###### Linear model (multivar with transformation) #######
df_train_norm <- na.omit(df_modelo_norm[sample, ])
df_test_norm  <- na.omit(df_modelo_norm[-sample, ])

reg_lm_mv_t <- lm(NPP~., data=df_train_norm)
reg_lm_mv_t


results_lm_mv_t <- data.frame("Model"="Multivar LM (trans)",
                            "PMAE Train"=mean(abs(df_train$NPP - exp(predict(reg_lm_mv_t, df_train_norm)))/df_train$NPP),
                            "PMAE Test"=mean(abs(df_test$NPP - exp(predict(reg_lm_mv_t, df_test_norm)))/df_test$NPP))


###### KNN ######

df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

pmt <- proc.time()
reg_KNN <- caret::train(NPP~.,
                 data = df_train,
                 method = "knn",
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats=10), # Seleccionar metodo de resampling (#aqui: repeated 10-fold CV)
                 tuneGrid = expand.grid(k = seq(3,10))) # Generar la malla de valores de parámetros para buscar optimo
proc.time() - pmt


reg_KNN #ver resultados
plot(reg_KNN) #visualizacion gráfica


NPP_pred_train <- predict(reg_KNN, df_train)
NPP_pred_test <- predict(reg_KNN, df_test)

pred_MAE <- mean(abs(na.omit(df_test$NPP) - NPP_pred))

results_KNN <- data.frame("Model"="KNN",
                          "PMAE Train"=mean(abs(df_train$NPP - NPP_pred_train)/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - NPP_pred_test)/df_test$NPP))
#pred_PMAE_KNN <- mean(abs(df_test$NPP - NPP_pred)/df_test$NPP)

###### KNN transformed ######

#sample <- sample.int(n = nrow(points_df), size = floor(.80*nrow(points_df)), replace = F)
df_train_norm <- na.omit(df_modelo_norm[sample, ])
df_test_norm  <- na.omit(df_modelo_norm[-sample, ])

pmt <- proc.time()
reg_KNN_norm <- train(NPP~.,
                 data = df_train_norm,
                 method = "knn",
                 trControl = trainControl(method = "repeatedcv", number = 5, repeats=10), # Seleccionar metodo de resampling (#aqui: repeated 10-fold CV)
                 tuneGrid = expand.grid(k = seq(3,10))) # Generar la malla de valores de parámetros para buscar optimo
proc.time() - pmt


reg_KNN_norm #ver resultados
plot(reg_KNN_norm) #visualizacion gráfica


NPP_pred_norm_train <- predict(reg_KNN_norm, df_train_norm)
NPP_pred_norm_test <- predict(reg_KNN_norm, df_test_norm)

#pred_MAE_norm <- mean(abs(na.omit(df_test_norm$NPP) - NPP_pred_norm))


#pred_PMAE_norm <- mean(abs(df_test_norm$NPP - NPP_pred_norm)/df_test_norm$NPP)


#Comparison

pred_log_train <- exp(NPP_pred_norm_train)
pred_log_test <- exp(NPP_pred_norm_test)


results_KNN_trans <- data.frame("Model"="KNN transformed",
                          "PMAE Train"=mean(abs(df_train$NPP - pred_log_train)/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - pred_log_test)/df_test$NPP))


PMAE_conv <- mean(abs(NPP_test - pred_log)/NPP_test)



hist(NPP_test - pred_log, breaks=100, xlab = "% de desviacion", main = "Desviación (en %) de la prediccion\nrespecto al dato real")

hist((abs(NPP_test - pred_log)/NPP_test)*100, xlim=c(0,600), breaks=500, xlab = "% de desviacion", main = "Desviación de las estimas del modelo KNN respecto al NPP real")
abline(v=PMAE_conv*100, lty="dashed")
median(abs(NPP_test - pred_log)/NPP_test)
abline(v=median(abs(NPP_test - pred_log)/NPP_test)*100, col = "red")




###### Regression Tree  CARET######

pmt <- proc.time()
reg_tree <- train(NPP~., data = df_train,
                  method = "rpart",
                  trControl = trainControl("cv", number = 10),
                  tuneLength = 39
)
proc.time() - pmt


reg_tree
plot(reg_tree)
plot(reg_tree$finalModel)
text(reg_tree, use.n=TRUE, all=TRUE, cex=.8)

NPP_pred_tree <- predict(reg_tree, df_test)

pred_MAE_tree <- mean(abs(na.omit(df_test$NPP) - NPP_pred_tree))

#Results
results_tree_car <- data.frame("Model"="Reg Tree Caret",
                          "PMAE Train"=mean(abs(df_train$NPP - predict(reg_tree, df_train))/df_train$NPP),
                          "PMAE Test"=mean(abs(df_test$NPP - predict(reg_tree, df_test))/df_test$NPP))

pred_PMAE_tree <- mean(abs(df_test$NPP - NPP_pred_tree)/df_test$NPP)

hist(NPP_test - NPP_pred_tree, breaks=100, xlab = "% de desviacion", main = "Desviación (en %) de la prediccion\nrespecto al dato real")

hist((abs(NPP_test - NPP_pred_tree)/NPP_test)*100, xlim=c(0,600), breaks=500, xlab = "% de desviacion", main = "Desviación de las estimas del modelo KNN respecto al NPP real")
abline(v=PMAE_conv*100, lty="dashed")
median(abs(NPP_test - NPP_pred_tree)/NPP_test)
abline(v=median(abs(NPP_test - NPP_pred_tree)/NPP_test)*100, col = "red")

######## Regression tree rpart normalized ##########

reg_tree_2 <- rpart(NPP~., method="anova", data = df_train_norm)

printcp(reg_tree_2)
summary(reg_tree_2)

plot(reg_tree_2)
text(reg_tree_2, use.n=TRUE, all=TRUE, cex=.8)

NPP_pred_tree2 <- predict(reg_tree_2, df_test_norm)

pred_MAE_tree2 <- mean(abs(na.omit(df_test$NPP) - NPP_pred_tree2))


pred_PMAE_tree2 <- mean(abs(df_test_norm$NPP - NPP_pred_tree2)/df_test_norm$NPP)

pred_log_tree2 <- exp(NPP_pred_tree2)


#Results
results_tree_rpart <- data.frame("Model"="Reg Tree transf",
                               "PMAE Train"=mean(abs(df_train$NPP - exp(predict(reg_tree_2, df_train_norm)))/df_train$NPP),
                               "PMAE Test"=mean(abs(df_test$NPP - exp(predict(reg_tree_2, df_test_norm)))/df_test$NPP))
PMAE_conv_tree2 <- mean(abs(NPP_test - pred_log_tree2)/NPP_test)

hist(NPP_test - pred_log_tree2, breaks=100, xlab = "% de desviacion", main = "Desviación (en %) de la prediccion\nrespecto al dato real")

hist((abs(NPP_test - pred_log_tree2)/NPP_test)*100, xlim=c(0,600), breaks=500, xlab = "% de desviacion", main = "Desviación de las estimas del modelo KNN respecto al NPP real")
abline(v=PMAE_conv*100, lty="dashed")
median(abs(NPP_test - pred_log_tree2)/NPP_test)
abline(v=median(abs(NPP_test - pred_log_tree2)/NPP_test)*100, col = "red")



####### regresion using randomForest #############

df_train <- na.omit(df_modelo[sample, ])
df_test  <- na.omit(df_modelo[-sample, ])

reg_rf <- randomForest(NPP~., data=df_train)
reg_rf


plot(reg_rf)
varImpPlot(reg_rf)

NPP_pred_rf <- predict(reg_rf, df_test)

pred_MAE_rf <- mean(abs(na.omit(df_test$NPP) - NPP_pred_rf))


#Results
results_tree_rf <- data.frame("Model"="RandomForest",
                               "PMAE Train"=mean(abs(df_train$NPP - predict(reg_rf, df_train))/df_train$NPP),
                               "PMAE Test"=mean(abs(df_test$NPP - predict(reg_rf, df_test))/df_test$NPP))

pred_PMAE_rf <- mean(abs(df_test$NPP - NPP_pred_rf)/df_test$NPP)

hist(NPP_test - NPP_pred_rf, breaks=100, xlab = "% de desviacion", main = "Desviación (en %) de la prediccion\nrespecto al dato real")

hist((abs(NPP_test - NPP_pred_rf)/NPP_test)*100, xlim=c(0,600), breaks=500, xlab = "% de desviacion", main = "Desviación de las estimas del modelo KNN respecto al NPP real")
abline(v=PMAE_conv*100, lty="dashed")
median(abs(NPP_test - NPP_pred_rf)/NPP_test)
abline(v=median(abs(NPP_test - NPP_pred_rf)/NPP_test)*100, col = "red")

saveRDS(reg_rf, file.path(path_models, "RF_train_70K.rds"))

#Combined Results

all_results <- rbind(results_median,
                     results_lm,
                     results_lm_mv,
                     results_lm_mv_t,
                     results_KNN,
                     results_KNN_trans,
                     results_tree_car,
                     results_tree_rpart,
                     results_tree_rf)

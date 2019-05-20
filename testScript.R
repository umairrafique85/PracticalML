df_trainData <- read.csv("pml-training.csv")
df_testData <- read.csv("pml-testing.csv")

colMeans(is.na(df_trainData))

varsToDrop <- names(colMeans(is.na(df_trainData)))[colMeans(is.na(df_trainData)) > 0.9]
library(caret)

df_trainData %>% 
  select(-varsToDrop) -> df_trainData
df_testData %>% 
  select(-varsToDrop) -> df_testData

vars_nzv <- nearZeroVar(df_trainData, saveMetrics = TRUE)$nzv

df_trainData <- df_trainData[, vars_nzv==FALSE]
df_testData <- df_testData[, vars_nzv==FALSE]

df_trainData <- df_trainData[,-1]
df_testData <- df_testData[,-1]

inTrain <- createDataPartition(df_trainData$classe, p = 0.7, list = FALSE)
train <- df_trainData[inTrain, ]
test <- df_trainData[-inTrain, ]


mdl_rf <- train(classe ~ ., method = "rf", data = train)
mdl_rf
confusionMatrix(test$classe, predict(mdl_rf, test))
save(mdl_rf, file = "mdl_rf.RData")

mdl_gbm <- train(classe ~ ., method = "gbm", data = train)
mdl_gbm
confusionMatrix(test$classe, predict(mdl_gbm, test))

confusionMatrix(test$classe, predict(mdl_xgb, test))
save(mdl_xgb, file = "mdl_xgb.RData")
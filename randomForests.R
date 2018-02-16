fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 6,
  ## repeated ten times
  repeats = 10)

rfDF <- cbind(clean$df[,selVars], 'classe' = clean$classe)
modFitAll <-
  train(classe ~ ., data = rfDF, method = "rf", trControl = fitControl)

importance <- varImp(modFitAll, scale=FALSE)
plot(importance)


pred <- predict(modFit, rfDF)

confusionMatrix(pred, rfDF$classe)

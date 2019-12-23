library('caret')

ranFor <- function(heart) {

  # Splitting the dataset to training and testing subsets
  heart <- as.data.frame(heart)
  to_train <- createDataPartition(y = heart$disease, p = .75, list = F)
  training <- heart[to_train,]
  testing <- heart[-to_train,]
  # Factoring the disease variable
  training[training$disease==1, 14] = 'No'
  training[training$disease==2, 14] = 'Yes'
  testing[testing$disease==1, 14] = 'No'
  testing[testing$disease==2, 14] = 'Yes'
  # Training
  ctrl <- trainControl(method = 'repeatedcv',
                       number = 10,
                       repeats =3,
                       summaryFunction = multiClassSummary,
                       classProbs = T)
  
  random_Forest <- train(disease ~ sex+chestPainType+exerAngina+slope+oldpeak+coloredVessels+thal, data = training, method = 'rf',
                      trControl = ctrl,
                      preProcess = c('center', 'scale'),
                      metric = 'Accuracy',
                      tuneGrid = expand.grid(.mtry=c(1:3)))
  predict.rf <- predict(random_Forest, newdata = testing[-14])
  predict.rf[predict.rf==1] = 'No'
  predict.rf[predict.rf==2] = 'Yes'
  cm <- confusionMatrix(data = predict.rf, as.factor(testing$disease))
  return(list(cm$table, cm$byClass, cm$overall))
}


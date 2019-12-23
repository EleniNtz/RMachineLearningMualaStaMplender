# Loading necessary libraries
library('caret')

clSVM <- function(heart) {
# Parsing the dataset and naming the columns
#heart <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat',
#                    sep=' ')
#colnames(heart) <- c('age', 'sex', 'chestPainType','restBloodPress', 'serumChol', 'fastBloodSug','restECG', 'maxHR', 'exerAngina', 'oldpeak', 'slope', 'coloredVessels', 'thal', 'disease')

# Splitting the dataset to training and testing subsets
heart <- as.data.frame(heart)
to_train <- createDataPartition(y = heart$disease, p = .75, list = F)
training <- heart[to_train,]
testing <- heart[-to_train,]
#return(dim(training))
# Factoring the disease variable
training$disease <- factor(training$disease)
testing$disease <- factor(testing$disease)
# Training
ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
grid <- expand.grid(C = c( .01, .05, .1, .25, .5, .75, 1,1.25, 1.5, 1.75, 2.5))
svm_Linear <- train(disease ~
                    +sex
                    +exerAngina 
                    +coloredVessels
                    +chestPainType
                    +restECG
                    +age
                    , data = training, method = 'svmLinear',
                    trControl = ctrl,
                    preProcess = c('center', 'scale'),
                    tuneGrid = grid,
                    tuneLength = 10)

pred <- predict(svm_Linear,newdata = testing)
cm <- confusionMatrix(pred, testing$disease)

return(list(cm$table, cm$byClass, cm$overall))
}

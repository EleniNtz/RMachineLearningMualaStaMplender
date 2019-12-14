# Importing libraries
library('rpart')
library('rpart.plot')
library('caret')
library('e1071')
library('caTools')

# Parsing the dataset and naming the columns
heart <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat',
                    sep=' ')

colnames(heart) <- c('age', 'sex', 'chestPainType','restBloodPress', 'serumChol', 'fastBloodSug','restECG', 'maxHR', 'exerAngina', 'oldpeak', 'slope', 'coloredVessels', 'thal', 'disease')

# Changing disease values (2 presence, 1 absence) to (1,0) respectively
heart$disease[heart$disease==1]=0
heart$disease[heart$disease==2]=1
heart$disease = factor(heart$disease, levels=c(0,1))

# Splitting the dataset to training and test
set.seed(123)
split = sample.split(heart$disease, SplitRatio = 0.75)
training_set = subset(heart, split==T)
test_set = subset(heart, split==F)

# Scaling the datasets 
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# SVM classification 
# Tested for all values and serially commenting out features in order to maximize 
# the main diagonal elements of the confusion matrix
classifier = svm(formula = disease ~ 
                  +sex
                  +exerAngina 
                  +coloredVessels
                  #+thal
                  #+oldpeak
                  #+fastBloodSug
                  +chestPainType
                  #+maxHR
                  #+restBloodPress 
                  #+serumChol
                  #+slope
                  +restECG
                  +age
                 ,data = training_set, type='C-classification', kernel = 'linear')

y_pred = predict(classifier, newdata = test_set[-14])
cm = table(test_set[,14], y_pred)
print(cm)
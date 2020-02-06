
############ PCA ##################
stats = read.csv('Titanictest.csv')
sum(is.na(stats))
stats$Age = ifelse(is.na(stats$Age),
                   ave(stats$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                   stats$Age)


#Encoding/changing categorical values as factors

stats$Sex = as.numeric(factor(stats$Sex,
                              levels = c('male','female'),
                              labels = c(0 , 1)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(stats$Survived, SplitRatio = 0.8)
training_set = subset(stats, split == TRUE)
test_set = subset(stats, split == FALSE)

# Feature Scaling
training_set[-5] = scale(training_set[-5])
test_set[-5] = scale(test_set[-5])

# Applying LDA
library(MASS)
lda = lda(formula = Survived ~ ., data = training_set)
training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(4 , 1)]
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(4 , 1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-2])
y_pred
# Making the Confusion Matrix
cm = table(test_set[, 2], y_pred)
cm
#Accuracy
Accuracy=(cm[1]+cm[4])/(cm[1]+cm[4]+cm[3]+cm[2])
Accuracy

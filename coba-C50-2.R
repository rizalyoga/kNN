getwd()
setwd("~/Documents/DataScience/")
getwd()

##import dataset
malware_dataset = read.csv("Datasetfull.csv")

##convert atribut family dari char -> factor
malware_dataset$Family <- as.factor(malware_dataset$Family)
str(malware_dataset$Family)

##split data
library(caTools)
split = sample.split(malware_dataset$Family, SplitRatio = 0.60)
training_set = subset(malware_dataset[-1], split == TRUE)
test_set = subset(malware_dataset[-1], split == FALSE)

##split data
#library(caTools)
#split = sample.split(malware_dataset$Family, SplitRatio = 0.60)
#training_set = subset(malware_dataset[-1], split == TRUE)
#test_set = subset(malware_dataset[-1], split == FALSE)

##training data
library(C50)
malw_model = C5.0(formula = Family ~ ., data = training_set )
malw_model
plot(malw_model)
summary(malw_model)

##training data dengan 10x percobaan
library(AppliedPredictiveModeling)
malw_model = C5.0(formula = Family ~ ., data = training_set, trials=10 )
summary(malw_model)
malw_model

malw_modelPred <- predict(malw_model,test_set)
postResample(malw_modelPred,test_set$Family)

##prediksi
pred = predict(malw_model,test_set[-919], type = 'class')
pred

table(pred, test_set$Family)
summary(pred)
plot(pred)
plot(test_set$Family)

##akurasi
#accuracy <- sum( pred != test_set[919] ) / length( pred )
#accuracy <- sum( pred == test_set$Family )
#accuracy <- mean( pred == test_set$Family )
#accuracy

##akurasi rmse with library
library(Metrics)
rmse(test_set$Family, pred)
##akurasi rmse manual
sqrt(mean((test_set$Family - pred)^2))

##akurasi with confusion metrics
library(caret)
#expected <- factor(c(1, 1, 0, 1, 0, 0, 1, 0, 0, 0))
#predicted <- factor(c(1, 0, 0, 1, 0, 0, 1, 1, 1, 0))
expected <- test_set$Family
predicted <- pred
results <- confusionMatrix(data=predicted, reference=expected)
print(results)



#Packages
library(readr)
library(caTools)
library(caret)
library(party)
library(rpart)
library(rpart.plot)
library(class)
library(e1071)

df = read_csv("diabetes.csv")
View(df)

#Remove NaN values
df = na.omit(df)

target_data = sample.split(df$Outcome, SplitRatio = 0.75)
View(target_data)

#Training data
train_data = subset(df, target_data==TRUE)
View(train_data)

#Test data
test_data = subset(df, target_data==FALSE)
View(test_data)

#Decision Tree classifier
tree = ctree(Outcome~., train_data)
plot(tree)

fit <- rpart(Outcome~., data = train_data, method="class")
rpart.plot(fit)

#Predictions
preds = predict(fit, train_data, type="class")
table(preds)
table(train_data$Outcome)

#Accuracy
dtc_error = sqrt((mean(preds != test_data$Outcome))^2)
print(paste('Accuracy =', 1-dtc_error))

#KNN Classifier
knn_model = knn(train_data, test_data, cl = train_data$Outcome, k = 15)
cm = table(test_data$Outcome, knn_model)
cm

#KNN Model Accuracy
error = sqrt((mean(knn_model != test_data$Outcome)^2))
print(paste('Accuracy =', 1-error))
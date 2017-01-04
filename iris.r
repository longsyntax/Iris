library(caret)
irisdata <- iris

# --- Exploring features ---
summary(irisdata)
head(irisdata)
summary(irisData$Species)
plot(irisData$Species, irisData$Sepal.Length)
plot(irisData$Species, irisData$Sepal.Width)
plot(irisData$Species, irisData$Petal.Length)
plot(irisData$Species, irisData$Petal.Width)

# --- 80/20 training/test split --- 
splitIndex <- createDataPartition(irisData$Species, p = 0.8, list = FALSE)
testSet = irisData[-splitIndex,]
trainingSet = irisData[splitIndex,]

# 1 --- Testing LDA ---
lda_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSet, method = "lda", metric = "Accuracy", trControl = trainControl(method = "cv", number = 5))
lda_model

# 2 --- Testing CART ---
cart_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSet, method = "rpart", metric = "Accuracy", trControl = trainControl(method = "cv", number = 5))
cart_model

# 3 --- Testing KNN ---
knn_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSet, method = "knn", metric = "Accuracy", trControl = trainControl(method = "cv", number = 5))
knn_model

# 4 --- Testing SVM ---
svm_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSet, method = "svmRadial", metric = "Accuracy", trControl = trainControl(method = "cv", number = 5))
svm_model

# 5 --- Testing RF ---
rf_model <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSet, method = "rf", metric = "Accuracy", trControl = trainControl(method = "cv", number = 5))
rf_model

# --- Comparing models --- 
allModels = resamples(list(LDA = lda_model, CART = cart_model, KNN = knn_model, SVM = svm_model, RF = rf_model))
summary(allModels)

# --- Choosing LDA for prediction on testSet ---
prediction <- predict(lda_model, irisData)
confusionMatrix(prediction, irisData$Species)



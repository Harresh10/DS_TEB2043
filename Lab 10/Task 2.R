library(e1071)
library(caTools)
library(class)
data(ChickWeight)

split <- sample.split(ChickWeight$Diet, SplitRatio = 0.7)
train_cl <- subset(ChickWeight, split == TRUE)
test_cl <- subset(ChickWeight, split == FALSE)

train_scale <- scale(train_cl[, 1:2])
test_scale <- scale(test_cl[, 1:2])

classifier_knn3 <- knn(train = train_scale, test = test_scale, cl = train_cl$Diet, k = 3)
misClassError3 <- mean(classifier_knn3 != test_cl$Diet)
print(paste('Accuracy K=3:', 1-misClassError3))

classifier_knn5 <- knn(train = train_scale, test = test_scale, cl = train_cl$Diet, k = 5)
misClassError5 <- mean(classifier_knn5 != test_cl$Diet)
print(paste('Accuracy K=5:', 1-misClassError5))

classifier_knn7 <- knn(train = train_scale, test = test_scale, cl = train_cl$Diet, k = 7)
misClassError7 <- mean(classifier_knn7 != test_cl$Diet)
print(paste('Accuracy K=7:', 1-misClassError7))

classifier_knn15 <- knn(train = train_scale, test = test_scale, cl = train_cl$Diet, k = 15)
misClassError15 <- mean(classifier_knn15 != test_cl$Diet)
print(paste('Accuracy K=15:', 1-misClassError15))

# k=15 has the highest accuracy (optimal k value)
cm <- table(test_cl$Diet, classifier_knn15) 
print(cm)
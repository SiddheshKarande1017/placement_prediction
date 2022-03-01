library('caret')
p<-read.csv("D:\\data science\\lab\\Placement_Data_Full_Class1.csv")
str(p)
head(p)
p$gender <- ifelse(p$gender=="M", 1, 0)
p$workex <- ifelse(p$workex=="Yes", 1, 0)
p$status <- ifelse(p$status=="Placed", 1, 0)
set.seed(575)
sub <- createDataPartition(y = p$status, p= 0.8, list = FALSE)
#print(sub)
plot(p)
cor(p[, unlist(lapply(p, is.numeric))])
cov(p[, unlist(lapply(p, is.numeric))])
training <- p[sub,]
testing <- p[-sub,]
#str(training)
dim(training); 
dim(testing);
anyNA(p)
summary(p)
training[["status"]] = factor(training[["status"]])
tr <- trainControl(method = "repeatedcv", number = 10, repeats = 3)#caret package
svm_Linear <- train(status ~., data = training, method = "svmLinear",
                    trControl=tr,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
#t<-table(testing$status, test_pred)
#ac1<-sum(diag(t))/sum(t)
#cat("\naccuracy of classifier is :",ac1)
print(confusionMatrix(table(test_pred, testing$status)))


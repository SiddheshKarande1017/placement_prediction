library('caTools')
f<-read.csv("D:\\data science\\lab\\Placement_Data_Full_Class1.csv",header=T, stringsAsFactors=T)
set.seed(59)
split <- sample.split(f, SplitRatio = 0.5)
train_reg <- subset(f, split == "TRUE")
test_reg <- subset(f, split == "FALSE")
str(f)

logistic_model <- glm(status ~., 
                      data = train_reg, 
                      family = "binomial")
logistic_model
summary(logistic_model)
predict_reg <- predict(logistic_model, 
                       test_reg, type = "response")
predict_reg  
predict_reg <- ifelse(predict_reg >0.5, 1, 0)
test_reg$status <- ifelse(test_reg$status=="Placed", 1, 0)
t<-table(test_reg$status, predict_reg)
print(t)
ac1<-sum(diag(t))/sum(t)
cat("\naccuracy of classifier is :",ac1)
Re<-t[1,1]/sum(t[1:2,1])
cat("\nRecall of classifier is :",Re)
Spe<-t[2,2]/sum(t[,2])
cat("\nSpecificity of classifier is :",Spe)
pre<-t[1,1]/sum(t[1,])
cat("\nPrecision of classifer is :",pre)
 

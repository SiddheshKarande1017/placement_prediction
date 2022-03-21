setwd("D:/DS_Course_project")
f<-read.csv("Placement_Data_Full_Class.csv")
library(class)
head(f)
f$workex<-as.factor(f$workex)
f$workex<-as.numeric(f$workex)
f$status<-as.factor(f$status)
workex_mod<-ifelse(f$workex=="Yes",1,0)
cbind(f,workex_mod)
f1<-data.frame(f$ssc_p,f$hsc_p,f$degree_p,f$etest_p,f$mba_p)
x<-f
n2<-function(b){
  (b-min(b))/(max(b)-min(b))
}
f1<-as.data.frame(lapply(f1,n2)) #normalize the data
#f1<-cbind(f1,f$workex)
set.seed(6969)
f1<-f1[order(runif(215)),]
#170 35 
f_train<-f1[1:170,]
f_test<-f1[171:215,]
f_train_label<-f[1:170,14]
f_test_label<-f[171:215,14]
p<-knn(f_train,f_test,f_train_label,k=5)
t<-table(actual=f_test_label,predicted=p)
print(t)
cat("The value of accuracy is ")
print(((t[1]+t[4])/(t[1]+t[2]+t[3]+t[4]))*100)
cat("The sensitivity is : ")
print((t[4]/(t[4]+t[2]))*100)
cat("The specificity is ")
print((t[1]/(t[1]+t[3]))*100)

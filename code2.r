data<-read.csv("LoanStats3d.csv",skip=1)

data <- read_csv("Downloads/LoanStats3d.csv", skip = 1)

grep("revol_bal_joint", colnames(data))

##only individual
data2<-subset(data,application_type=="Individual"& grade!="")

data2<-subset(data2,select=c(loan_amnt,int_rate,grade,dti,open_acc,inq_last_6mths,acc_open_past_24mths,emp_length,annual_inc,revol_bal))

##clean data
int<-as.character(data2$int_rate)
int2<-gsub("%", "", int)
data2$int_rate<-as.numeric(int2)

ggplot(data2,aes(grade))+geom_bar()


sapply(data2,FUN = function(x) sum(is.na(x)))

data3 <- data2
data3$grade <-as.factor(data3$grade)
data3$grade2 <-as.numeric(data3$grade)
##linear regression
b<-lm(grade2~loan_amnt+dti+emp_length+open_acc+inq_last_6mths+acc_open_past_24mths+annual_inc,data3)
summary(b)

b<-lm(grade2~openn_acc+dti+inq_last_6mths+acc_open_past_24mths,data3)
summary(b)

ir.pca <- prcomp(data3,
                 center = TRUE,
                 scale. = TRUE) 


##choose loan_amnt, int_rate,acc_open_past_24mths

##divided into train/test set
n=nrow(data2)
t=sample(1:n,size=(0.9*n),replace=FALSE,prob=NULL)
train=data2[t,]
test=data2[-t,]

##logistic model
library(nnet)
b<-multinom(grade~open_acc+dti+inq_last_6mths+acc_open_past_24mths,data=train)

fitted.results <- predict(b,newdata=test,type='probs')

p_grade<-rep(0,nrow(test))
fitted.results <-cbind(fitted.results,p_grade)
pro<-rep(0,nrow(test))
fitted.results <-cbind(fitted.results,pro)

for(i in 1:nrow(test)){
  for( j in 1:7){
    if (fitted.results[i,j]>fitted.results[i,9]){
      fitted.results[i,9]=fitted.results[i,j]
      fitted.results[i,8]=j
    }
    
  }
}

fitted.results2<-as.data.frame(fitted.results)
t_result<-cbind(test,fitted.results2$p_grade)
t_result$grade2<-as.numeric(as.factor(t_result$grade))
t_result$re<-ifelse(t_result$grade2==t_result[,11],0,1)
sum(t_result$re)/nrow(t_result)

##0.01466987
ac<-rep(0,10)

ac[1]=1-sum(t_result$re)/nrow(t_result)

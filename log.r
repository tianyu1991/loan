grep("total_il_high_credit_limit", colnames(data))
##only individual
data2<-subset(data,application_type=="Individual"& grade!="")
data2<-subset(data2,select=c(loan_amnt,int_rate,grade,dti,open_acc,inq_last_6mths,acc_open_past_24mths))

int<-as.character(data2$int_rate)
int2<-gsub("%", "", int)
data2$int_rate<-as.numeric(int2)

apply(data2,function(x) sum(is.na(x)))

b<-lm(grade2~loan_amnt+int_rate+inq_last_6mths+acc_open_past_24mths+annual_inc,data4)
summary(b)
##choose varaible loan_amnt, int_rate,acc_open_past_24mths

n=nrow(data2)
t1=sample(1:n,size=(0.9*n),replace=FALSE,prob=NULL)
train=data2[t1,]
test=data2[-t1,]

grade2<-as.numeric(data2$grade)

library(nnet)
b<-multinom(grade~loan_amnt+int_rate+acc_open_past_24mths,data=train)
summary(b)

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

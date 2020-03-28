library(readxl)
Data <- read_excel(".../Data.xlsx", 
                     +     sheet = "Sheet4")
View(Data)      
Data$team2 [ Data$DIAB_P > 10.00|Data$OBE_P>30] = 1
Data$team2 [ Data$DIAB_P <= 10.00&Data$OBE_P<=30] = 0
library(e1071)
set.seed(1234)
df<-Data[-1]
train<-sample(nrow(df),0.7*nrow(df))
df.train<-df[train,]
df.validate<-df[-train,]
table(df.train$team2)

table(df.validate$team2)

fit.svm<-svm(team2~.,data=df.train)
fit.svm

#Number of Support Vectors:  28

svm.pred<-predict(fit.svm,na.omit(df.validate))
svm.fact<-factor(svm.pred>.5,levels=c(FALSE,TRUE),labels=c("0","1"))
svm.pref<-table(df.validate$team2,svm.fact,dnn=c("Actual","Predicted"))
svm.pref
#####
        #Predicted
#Actual 0         1
#0      4         1
#1      0        11
#####

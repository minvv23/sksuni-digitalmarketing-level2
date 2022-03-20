#+ Week 7 Factor analysis----------------------------------
#' # Week 7 

#' show current directory
getwd() 

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls()) #remove all the objects

#+ Week 7.1 logistic regression: test sample----------------------------------
#' # Week 7.1  logistic regression: test sample

#' import data 
hr.df<-read.csv("L6_hr.csv")

#' show the first 10 rows of the data 
head(hr.df,10)

#' show the structure of the data 
str(hr.df)

colnames(hr.df)
colnames(hr.df)[colnames(hr.df)=="average_montly_hours"]<-"average_monthly_hours"

mean(is.na(hr.df))

#' scatter plots with y=respond
library(ggplot2, ggthemes)

ggplot(hr.df,aes(x=average_monthly_hours, y=left)) + geom_point() +
  labs(x="average monthly hours", y="Left") + theme_bw()

#' box plots to show which variables may have effects on employees' decision to leave
## make sure to convert left variable to a factor variable
hr.df$left<-as.factor(hr.df$left)
ggplot(hr.df, aes(x=left, y=satisfaction_level)) + geom_boxplot() + theme_bw()
ggplot(hr.df, aes(x=left, y=last_evaluation)) + geom_boxplot() + theme_bw()
ggplot(hr.df, aes(x=left, y=average_monthly_hours)) + geom_boxplot() + theme_bw()


#' compare average of variables between those who left and did not leave
aggregate(number_project~left, data=hr.df, mean)
aggregate(time_spend_company~left, data=hr.df, mean)

#' two-way tables
t_salary<-table(hr.df$salary,hr.df$left, dnn=c("salary", "left"))
round(prop.table(t_salary,1),3)

t_promotion<-table(hr.df$promotion_last_5years,hr.df$left, dnn=c("promotion", "left"))
round(prop.table(t_promotion,1),3)

t_accident<-table(hr.df$Work_accident,hr.df$left, dnn=c("accident", "left"))
round(prop.table(t_accident,1),3)

t_department<-table(hr.df$Department,hr.df$left, dnn=c("department", "left"))
round(prop.table(t_department,1),3)

#' bar graph to make comparisons
## stacked 
ggplot(hr.df, aes(x=Department, fill=left)) + geom_bar(stat="count")
## side-by-side
ggplot(hr.df, aes(x=Department, fill=left)) + geom_bar(stat="count",position=position_dodge())

#' logistic regression analysis



##' partitioning data into train and test sample
###' generate random numbers 
table(hr.df$left)
3571/11428
prop.table(table(hr.df$left))
hr.stay<-hr.df[hr.df$left==0,]
hr.leave<-hr.df[hr.df$left==1,]
set.seed(7)
sample <- sample(c(TRUE, FALSE), nrow(hr.stay), replace=TRUE, prob=c(0.312,0.688))
hr.stay.sample <- hr.stay[sample, ]
hr.df.balanced <- rbind(hr.stay.sample, hr.leave)

sample1 <- sample(c(TRUE, FALSE), nrow(hr.df.balanced), replace=TRUE, prob=c(0.75,0.25))
hr.train<-hr.df.balanced[sample1,]
hr.test<-hr.df.balanced[!sample1,]
#n <- dim(hr.df)[1] # get the total number of observations or employees in the data
#train_ind <- runif(n)<0.75 # index numbers of random rows or employees
#head(runif(n)<0.75)

## split the data into train and test samples
#train<-hr.df[train_ind, ]
#test<-hr.df[!train_ind, ]

#' validating your partition
prop.table(table(hr.train$left))
prop.table(table(hr.test$left))

##' Model 1 

model0<-glm(left ~ average_monthly_hours , family="binomial", data = train)

#' show regression table 
library(stargazer)
stargazer(model0, type="text")
confusion_matrix(model0)

model1<-glm(left ~ satisfaction_level + average_monthly_hours + salary, family="binomial", data = hr.train)
stargazer(model1, type="text")
round(exp(model1$coefficients),3)

#' calculate confusion matrix 
install.packages("regclass")
library(regclass)
prediction_table1<-confusion_matrix(model1)
round(prop.table(prediction_table1[1:2,1:2],1),3)

model2<-glm(left ~ satisfaction_level + average_monthly_hours + promotion_last_5years + salary, family="binomial", data = train)
stargazer(model2, type="text")
prediction_table2<-confusion_matrix(model2)
round(prop.table(prediction_table2[1:2,1:2],1),3)


#' logistic regression: model 2
model3<-glm(left ~ satisfaction_level + average_monthly_hours + promotion_last_5years + salary + Department, family="binomial", data = train)
stargazer(model0, model1, model2, model3, type="text", out="text")
prediction_table3<-confusion_matrix(model3)
round(prop.table(prediction_table3[1:2,1:2],1),3)


#' show predicted values
#install.packages("dplyr")
#probabilities<-predict(model2, newdata=test, type="response")
#predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
#mean(predicted.classes == test$left)

#a<-data.table(round(predict(lr2,newdata=dt2, type="response" ),3),keep.rownames = T)
#setnames(a,"V1","prob")
#a[, rn:= row.names(a)]
#a[, Visit := ifelse(prob >=0.5,1,0)]
#setcolorder(a,c("rn","prob","Visit"))

install.packages("caret")
library(caret)
install.packages("InformationValue")
library(InformationValue)

#' use model to predict probability of default
predicted <- predict(model3, test, type="response")
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$left, predicted)[1]


#' create confusion matrix
confusionMatrix(test$left, predicted, threshold=0.5)
confusionMatrix(test$left, predicted, threshold=optimal)

#' calculate sensitivity
sensitivity(test$left, predicted, threshold=optimal)

#' calculate specificity
specificity(test$left, predicted, threshold=optimal)

#' calculate total misclassification error rate
misClassError(test$left, predicted, threshold=0.5)
1-misClassError(test$left, predicted, threshold=optimal)

#' end
rm(list=ls())
#+ Week 6 ----------------------------------
#' # Week 6

#' show current directory
getwd() 

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls()) #remove all the objects

#+ 6.1  Omitted variable problem ------
#' ## 6.1 Omitted variable problem

#' import data 
banking.df<-read.csv("L5_banking.csv")

#' show the first 10 rows of the data 
head(banking.df, 10)

#' show the structure of the data 
str(banking.df)

#' ## data cleaning

#' data cleaning 1: convert district into a factor variable
banking.df$district<-as.factor(banking.df$district)

#' data cleaning 2: change name of variable
colnames(banking.df)
colnames(banking.df)[colnames(banking.df)=="inc"]<-"income"
colnames(banking.df)

#' data cleaning 3: change values of variable
banking.df$income2 <- ifelse(banking.df$income==1,"Less than $15,000",
                             ifelse(banking.df$income==2, "$15,000-$19,999",
                                    ifelse(banking.df$income ==3, "$20,000 - $29,000",
                                           ifelse(banking.df$income ==4, "$30,000- $39,999",
                                                  ifelse(banking.df$income==5,"$40,000 -$40,999",
                                                         ifelse(banking.df$income==6,"$50,000- $74,999",
                                                                ifelse(banking.df$income==7,"$75,000-99,999", 
                                                                       ifelse(banking.df$income==8,"$100,000 - $124,999",
                                                                              ifelse(banking.df$income==9,"$125,000 and more",NA)))))))))

banking.df$age2 <- ifelse(banking.df$age==1,"15 years or younger",
                          ifelse(banking.df$age==2, "15 - 24 years",
                                 ifelse(banking.df$age ==3, "25 - 34 years",
                                        ifelse(banking.df$age ==4, "35 - 44 years",
                                               ifelse(banking.df$age==5,"45 - 54 years",
                                                      ifelse(banking.df$age==6,"55 - 64 years",
                                                             ifelse(banking.df$age==7,"65 years or older",NA)))))))

#' # data cleaning 4: missing values
is.na(banking.df$income)
is.na(banking.df$age)
#' compute the fraction of observations with 
mean(is.na(banking.df$income))
mean(is.na(banking.df$age))
#' omit the observations with missing values
banking.df<-na.omit(banking.df)


#+ Alan's tasks ----
#' ## Alan's tasks

#' calculate the counts, mean, sd of "profit" by "online"
aggregate(profit~online, data=banking.df, mean)

#' t test 
t.test(profit ~ online, banking.df)
t.test(profit ~ online, banking.df, equal.var=F)

#' anova test 
model1<-lm(profit ~ online, banking.df) 
summary(model1)
stargazer(model1, type="text")

#' relationship between profitability and income
aggregate(profit~income, banking.df, mean)

#' linear regression 
reg1<-lm(profit ~ online + income, banking.df)
#' summary the results of the regression
summary(reg1)
#' quick view the results of the regression 
library(stargazer)
stargazer(reg1, type="text")

#' relationship between age and profitability
aggregate(profit~age, banking.df, mean)

#' linear regression 
reg2<-lm(profit ~ online + income + age, banking.df)
summary(reg2)
stargazer(reg1,reg2,type="text")
stargazer(reg1,reg2,type="html", out = "w2t.html")


#' cross-table by age and profits
aggregate(banking.df$age, banking.df$profit, dnn=c("Age", "Profit"))
print(table2)
round(prop.table(table2,1),3)

table<-table(banking.df$age, banking.df$online, dnn=c("age", "online"))
round(prop.table(table,1),3)
#+ 6.2  logistic regression analysis ------
#' ## 6.2 logistic regression analysis

#' clear the environment
rm(list=ls())

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
set.seed(7)
n <- dim(hr.df)[1] # get the total number of observations or employees in the data
train_ind <- runif(n)<0.75 # index numbers of random rows or employees
head(runif(n)<0.75)
## split the data into train and test samples
hr.df_train<-hr.df[train_ind, ]
hr.df_test<-hr.df[!train_ind, ]

#' validating your partition
prop.table(table(hr.df_train$left))
prop.table(table(hr.df_test$left))

##' Model 1 

model0<-glm(left ~ average_monthly_hours , family="binomial", data = hr.df_train)

#' show regression table 
library(stargazer)
stargazer(model0, type="text")
confusion_matrix(model0)

model1<-glm(left ~ satisfaction_level + average_monthly_hours + salary, family="binomial", data = hr.df_train)
stargazer(model1, type="text")

#' calculate confusion matrix 
install.packages("regclass")
library(regclass)
prediction_table1<-confusion_matrix(model1)
round(prop.table(prediction_table1[1:2,1:2],1),3)

model2<-glm(left ~ satisfaction_level + average_monthly_hours + promotion_last_5years + salary, family="binomial", data = hr.df_train)
stargazer(model2, type="text")
prediction_table2<-confusion_matrix(model2)
round(prop.table(prediction_table2[1:2,1:2],1),3)


#' logistic regression: model 2
model3<-glm(left ~ satisfaction_level + average_monthly_hours + promotion_last_5years + salary + Department, family="binomial", data = hr.df_train)
stargazer(model0, model1, model2, model3, type="text", out="text")
prediction_table3<-confusion_matrix(model3)
round(prop.table(prediction_table3[1:2,1:2],1),3)



#' end 
rm(list=ls())

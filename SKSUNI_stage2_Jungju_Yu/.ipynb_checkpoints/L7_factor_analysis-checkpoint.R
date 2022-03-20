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
library(stargazer)
library(regclass)


#' convert left variable to a factor variable
hr.df$left<-as.factor(hr.df$left)

#' logistic regression analysis

##' partitioning data into train and test sample
###' generate random numbers 
set.seed(7)
sample <- sample(c(TRUE, FALSE), nrow(hr.df), replace=TRUE, prob=c(0.75,0.25))
train <- hr.df[sample, ]
test <- hr.df[!sample, ]

#n <- dim(hr.df)[1] # get the total number of observations or employees in the data
#train_ind <- runif(n)<0.75 # index numbers of random rows or employees
#head(runif(n)<0.75)

## split the data into train and test samples
#train<-hr.df[train_ind, ]
#test<-hr.df[!train_ind, ]

#' validating your partition
round(prop.table(table(train$left)),3)
round(prop.table(table(test$left)),3)

##' Model 1 

model0<-glm(left ~ average_monthly_hours , family="binomial", data = train)
stargazer(model0, type="text")

model1<-glm(left ~ satisfaction_level + average_monthly_hours + salary, family="binomial", data = train)
stargazer(model1, type="text")
#' show exponents of coefficients for interpretations using odds ratio
round(exp(model1$coefficients),3)

model2<-glm(left ~ satisfaction_level + average_monthly_hours + promotion_last_5years + salary, family="binomial", data = train)
stargazer(model2, type="text")

model3<-glm(left ~ satisfaction_level + average_monthly_hours + promotion_last_5years + salary + Department, family="binomial", data = train)
stargazer(model3, type="text")

#' install.packages("caret")
library(caret)
#' install.packages("InformationValue")
library(InformationValue)

#use model to predict probability of default
predicted <- predict(model3, test, type="response")
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$left, predicted)[1]


#create confusion matrix
confusionMatrix(test$left, predicted, threshold=0.5)
confusionMatrix(test$left, predicted, threshold=optimal)

#calculate sensitivity
sensitivity(test$left, predicted, threshold=optimal)

#calculate specificity
specificity(test$left, predicted, threshold=optimal)

#calculate total overall prediction accuracy
misClassError(test$left, predicted, threshold=0.5)
1-misClassError(test$left, predicted, threshold=optimal)



#+ Week 7.2 factor analysis----------------------------------
#' # Week 7.2  factor analysis

# clear the environment
rm(list=ls())
# read the dataset
data<-read.csv("L7_factor_analysis.csv")

head(data, 10)
summary(data)
str(data)

#install.packages("psych")
library(psych)

#' check whether the data is suitable for factor analysis 
library(corrplot)
## correlation table
cor(data)
## correlation plot
corrplot(cor(data), method="number")

#' factor analysis
library(GPArotation)
library(ggplot2)
library(ggthemes)

#' determining the number of factors using principal component analysis
#' scale the variables
summary(data)
data.sc <- data.frame(scale(data))
#' run a pca
summary(data.sc)
data.pc <- prcomp(data.sc)
summary(data.pc)
data.pc
#' scree plot
plot(data.pc, type="l")

#' running initial factor analysis 
fa<-fa(data, rotate = "none")
#' producing a screeplot
n_factors <- length(fa$e.values)
scree <- data.frame(Factor_n =  as.factor(1:n_factors), Eigenvalue = fa$e.values)
colnames(scree)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() + xlab("Number of factors") + ylab("Eigenvalue") + labs( title = "Scree Plot")

fa_unrotated<- fa(data.sc, nfactors = 2, fm="pa", rotate = "none")
print(fa_unrotated,digits = 3)

#' loading plot for interpretation
fa_loading<-data.frame(fa_unrotated$loadings[1:7, 1:2])

#' scatter plot by scores of factor 1 and 2
ggplot(fa_loading,aes(x=PA1, y=PA2)) + geom_point() +  geom_text(label=rownames(fa_loading), nudge_x=0.1) + 
  labs(x="Factor 1", y="Factor 2") + geom_hline(yintercept=0) + geom_vline(xintercept = 0) 

#' factor analysis with rotation
fa_rotated<- fa(data.sc, nfactors = 2, fm="pa", rotate = "varimax") #' factor analysis by PCA 
print(fa_rotated,digits = 3)
#' loading plot for interpretation
fa_loading_rotated<-data.frame(fa_rotated$loadings[1:7, 1:2])

#' scatter plot by scores of factor 1 and 2
ggplot(fa_loading_rotated,aes(x=PA1, y=PA2)) + geom_point() +  geom_text(label=rownames(fa_loading), nudge_x=0.1) + 
  labs(x="Factor 1", y="Factor 2") + geom_hline(yintercept=0) + geom_vline(xintercept = 0) 


#' save scores to a data.frame format
round(fa_rotated$weights,3)
round(fa_rotated$scores,3)
#scores<-data.frame(round(fa_rotate$scores,3),keep.rownames = T) 
scores<-data.frame(round(fa_rotated$scores,3)) 

#' scatter plot by scores of factor 1 and 2
ggplot(scores,aes(x=PA1, y=PA2)) + geom_point() +labs(x="Intelligence", y="Leadership skills") + 
  geom_hline(yintercept=0) + geom_vline(xintercept = 0)


#' end 
rm(list=ls())

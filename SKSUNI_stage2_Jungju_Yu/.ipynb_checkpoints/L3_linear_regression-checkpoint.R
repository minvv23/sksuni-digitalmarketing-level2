#+ Week 3 --------
#' # Week 3 

#' show current directory
getwd() 

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls()) #remove all the objects
#rm(dt)
#control + l to clear the console

#'import data and name it as "email.df" 
email.df<-read.csv("L2_email_campaign.csv")
#summary provides a summary of a vector or a dataset

summary(email.df)

#' View the whole dataset in a separate window.
View(email.df) 

#' View the whole dataset in a console.
print(email.df)

#' show the structure of dataset
str(email.df)

#' show the variable names
colnames(email.df)

#+ 3.1  Describing and analyzing relationship between two variables ------
#' ## 3.1 Describing and analyzing relationship between two variables

#'correlation between two variables
cor(email.df$age,email.df$totdol)
cor.test(email.df$age,email.df$totdol)
cor(email.df$numpurchase,email.df$totdol)

#' scartter plot: age and spend 
#' R has a basic plot function
plot(email.df$age, email.df$totdol, xlab="age", ylab="total dollars spent")

#' we can product a better scatter plot using ggplot
#' install.packages("ggplot2") if you have not done it already
library(ggplot2, ggthemes)
#'geom_point creates scatterplots
ggplot(email.df,aes(x=age, y=totdol)) + geom_point() +
  labs(x="age", y="total dollars spend") + theme_bw()

ggplot(email.df,aes(x=numpurchase, y=totdol)) + geom_point() +
  labs(x="total number of purchases", y="totoal dollars spent") + theme_bw()


#' Q1: Do consumers who respond to emails spend the same account as those who do not? 
# Aggregate: use a formula!
t1 <- aggregate(totdol ~ respond, data=email.df, mean)
View(t1)
aggregate(totdol ~ respond + gender, data=email.df, mean)

#' independent-samples t test
#' run a t-test without assuming the equal variances
t.test(totdol~respond, email.df, var.equal=F)
#' see what happens if equal variances are not assumed
t.test(totdol~respond, email.df, var.equal=T)
#' check whether variance of the two samples are the same
var.test(totdol~respond,email.df)

x<-seq(70, 100, length=30)
y<-seq(75, 100, length=30)
t.test(x,y)
t.test(x,y,paired=T)

#' Q2: Do consumers of a different gender respond to email campaigns with the same rate? 
#' describing the relationship in a two-way table
round(prop.table(table(email.df$respond, email.df$gender),2),3)

t2 <- table(email.df$respond, email.df$gender,
            dnn = c("respond","gender"))
t2

round(prop.table(t2,2),3)

#' chi-square test for a relationship between two categorical variables
chisq.test(email.df$respond, email.df$gender) 
chisq.test(email.df$respond, email.df$gender, correct = F) 

#+ 3.2  Linear regression ------
#' ## 3.2 Linear regression

rm(list=ls())
#' import data 
meddi.df<-read.csv("L3_meddicorp.csv")

#' show the dataset
View(meddi.df)

#' show the structure of the dataset 
str(meddi.df) 
is.factor(meddi.df$REGION)
#' REGION should be a factor variable 
meddi.df$REGION <- as.factor(meddi.df$REGION)

summary(meddi.df)

#' scatter plot 
library(ggplot2, ggthemes)
ggplot(meddi.df,aes(x=BONUS,y=SALES)) + geom_point() + theme_bw() 




#' model 1 ----------------------------------------
#' unstandardized regression 
model1<-lm(SALES ~ BONUS + ADV, meddi.df) 
summary(model1)

# stargazer is a very popular package for export beautiful results
#install.packages("stargazer")
library(stargazer)
stargazer(model1, type = "text")


#' model 2 ---------------------------------------------
model2<-lm(SALES ~ BONUS + ADV + MKTSHR + COMPET, meddi.df) 
#' summary the result
summary(model2)
#'quick view the result 
stargazer(model2, type="text")

#' compare model 1 and model 2 
stargazer(model1,model2, type="text")

#' model 3 -------------------------------------
model3<-lm(SALES ~ BONUS + ADV + LASTYEARBONUS, meddi.df) 


#' summary the result
summary(model3)

#'quick view the result 
stargazer(model3, type="text")

#' compare model 1, model 2, and model 3 
stargazer(model1,model2,model3, type="text")

#' correlation matrix for all variables in the data
cor(meddi.df)
#' correlation between BONUS and LASTYEARBONUS
cor(meddi.df$BONUS, meddi.df$LASTYEARBONUS)

#' visualizing correlation
install.packages("corrplot")
library(corrplot)
corrplot.mixed(corr=cor(meddi.df[, -c(1)]), upper="ellipse")
#' correlation between BONUS and LASTYEARBONUS
#' first, find the column numbers of the two variables using which() function
which(colnames(meddi.df)==c("BONUS","LASTYEARBONUS"))
cor(meddi.df[, c(3,6)])


#' model 4
model4<-lm(SALES ~ BONUS + ADV + factor(REGION, levels = c(1,2,3)), meddi.df)
model4b<-lm(SALES ~ BONUS + ADV + factor(REGION, levels = c(2,1,3)), meddi.df) 
model4c<-lm(SALES ~ BONUS + ADV + factor(REGION, levels = c(3,1,2)), meddi.df) 

#' compare different models 
stargazer(model4,model4b,model4c, type="text")
stargazer(model1,model2,model3,model4, type="text")

#' end
rm(list=ls())

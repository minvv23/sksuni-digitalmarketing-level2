#+ Week 5 ----------------------------------
#' # Week 5

#+ 5.0  Debugging from last time ------
#' ## 5.0 Debugging from last time

#+ 5.1  Linear regression ------
#' ## 5.1 Linear regression

#' show current directory
getwd() 

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls())

#' import data 
meddi.df<-read.csv("L3_meddicorp.csv")

#' show the dataset
View(meddi.df)

#' show the structure of the dataset 
str(meddi.df)

#' REGION variable must be converted into a factor variable. Begin by replacing values with each region 
meddi.df['REGION'][meddi.df['REGION'] == 1]<-"South"
meddi.df['REGION'][meddi.df['REGION'] == 2]<-"West"
meddi.df['REGION'][meddi.df['REGION'] == 3]<-"Midwest"
is.factor(meddi.df$REGION)
meddi.df$REGION <- as.factor(meddi.df$REGION)

summary(meddi.df)

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
cor.test(meddi.df$BONUS, meddi.df$LASTYEARBONUS)


#' model 4: adding a factor variable REGION
#' create dummy variables for each region
meddi.df$DSouth <- ifelse(meddi.df$REGION=="South",1,0)
meddi.df$DWest <- ifelse(meddi.df$REGION=="West",1,0)
meddi.df$DMidwest <- ifelse(meddi.df$REGION=="Midwest",1,0)

#' run a regression analysis with all the dummy variables (R will drop one dummy variable)
model4<-lm(SALES ~ BONUS + ADV + DSouth + DWest + DMidwest, meddi.df)
summary(model4)
stargazer(model4, type="text")

#' manually omit one dummy variable (DWest) and run a regression analysis 
model4b<-lm(SALES ~ BONUS + ADV + DSouth + DMidwest, meddi.df)
stargazer(model4b, type="text")

#' R is smart! We can just run a regression by adding the factor variable REGION
model4c<-lm(SALES ~ BONUS + ADV + REGION, meddi.df)
stargazer(model4c, type="text")


#' compare different models 

stargazer(model4,model4b,model4c, type="text")
stargazer(model1,model2,model3,model4, type="text")


#+ 5.2  Omitted variable problem ------
#' ## 5.2 Omitted variable problem


#' clear the environment
rm(list=ls())

#' import data 
banking.df<-read.csv("L5_banking.csv")

#' show the first 10 rows of the data 
head(banking.df,10)

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


#+ 5.2.1 Alan's tasks ----
#' ## 5.2.1 Alan's tasks

#' calculate the counts, mean, sd of "profit" by "online"
aggregate(profit~online, data=banking.df, mean)

#' t test 
t.test(profit ~ online, banking.df)
t.test(profit ~ online, banking.df, equal.var=F)

#' anova test 
model1<-lm(profit ~ online, banking.df) 
summary(model1)
stargazer(model1, type="text")

anova<-aov(profit~online, banking.df)
summary(anova)

#' relationship between profitability and income
aggregate(profit~income, banking.df, mean)

#' linear regression 
reg1<-lm(profit ~ online + income, banking.df)
#' summary the results of the regression
summary(reg1)
#' quick view the results of the regression 
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


#+ 5.2.2 Non-linear transformation -----
#' ## 5.2.2 Non-linear transformation


dt2<-read.csv("data_w4_nonlinear_transformation.csv")

head(dt2,10)
str(dt2)
colnames(dt2)

ggplot(dt2,aes(x=Temperature,y=Yield)) + geom_point() +theme_bw()

reg3<-lm(Yield ~ Temperature, dt2)
stargazer(reg3,type="text")

dt2[, TempSq := Temperature^2]
reg4<-lm(Yield ~ Temperature + TempSq, dt2)
stargazer(reg4,type="text")

rm(list=ls())
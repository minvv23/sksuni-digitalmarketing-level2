
#' Useful keys in RStudio 

#' Ctrl + L: clear the screen 
#' Ctrl + R : run the command for macos
#' Ctrl + Enter: run the command for windows

#+ Week 2  ----
#' # Week 2: Describing the data vs. drawing inferences from the data
#' show current directory
getwd()

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls()) #remove all the objects
#control + l to clear the console

#+ 2.1 Describing the data ---------------
#' ## 2.1 Describing the data ---------------


#'import data and name it as "email.df" 
email.df<-read.csv("L2_email_campaign.csv")

#' View the whole dataset in a separate window.
View(email.df)

#' View the whole dataset in a console.
print(email.df)

#' show the structure of dataset
str(email.df)

#' show the variable names
colnames(email.df)
#names(email.df) <- c("id","respond","gender","age","numberpurchase","totaldollars","region","addresstype")


#summary provides a summary of a vector or a dataset
summary(email.df)
#using pysch package
install.packages("psych")
library("psych")
describe(email.df)

#' show the first or last 10 rows of the dataset
head(email.df,10) 
tail(email.df,10) 


#' Q1: What % of customers responded to the campaign 

#' use table to count by respond
table(email.df$respond)
respond.table <- table(email.df$respond)
respond.table

with(email.df, table(respond))

#' count % by the respond
prop.table(respond.table)
# or, using "with"
with(email.df, prop.table(table(respond)))

#' Q2: Who are the customers who responded to the campaign?

#' crosstabulation between respond and gender variable
t1 <- table(email.df$respond, email.df$gender)
t1

#' calculate the proportion by column 
prop.table(t1)
prop.table(t1,2)
#' keep 2 digits
round(prop.table(t1,2),2)

#' calculate the proportion by row 
prop.table(t1,1)
round(prop.table(t1,1),2) 

#' alternatively, use "with" function and show variable names
with(email.df, table(respond, gender))
with(email.df, round(prop.table(table(respond, gender),2),2))

#' Q3: How much do customers spend to buy the company's products?

#' statistical description of the variable "totdol"
summary(email.df$totdol)
sd(email.df$totdol)
var(email.df$totdol)

#' histogram visually shows the entire distribution

#' histogram using a base function
hist(email.df$totdol)
hist(email.df$totdol, main="Histogram for Totdol", xlab="total dollars spent", ylab="counts")
hist(email.df$totdol, main="Histogram for Totdol", xlab="total dollars spent", ylab="counts", breaks=30)
hist(email.df$totdol, main="Histogram for Totdol", xlab="total dollars spent", ylab="counts", breaks=30, col="lightblue")

#' histogram using ggplot2
install.packages("ggplot2") 
#' ggplot2 is a very popular package in data visualization
library(ggplot2)

install.packages("ggthemes") 
#' ggtheme contains extra themes, scales, and geoms, and functions for ggplot2
library(ggthemes)

ggplot(email.df, aes(x=totdol)) + geom_histogram() #default number of bins=30
ggplot(email.df, aes(x=totdol)) + geom_histogram(bins=10) #set number of bins=10
ggplot(email.df, aes(x=totdol)) + geom_histogram(binwidth=300) #set width of each bin=300
h1<-ggplot(email.df, aes(x=totdol)) + geom_histogram(fill="steelblue", binwidth=300) #change the bin color and save histogram as an object
h1
h1+theme_bw() #use the saved object and change the background theme
h1+ labs(x="total dollars spent",y="counts") + theme_bw() # add new labels




#+ 2.2 Questions about one variable----
#' ## 2.2 Questions about one variable

#' Q1: Test a mean 

#' double-tailed test
t.test(email.df$totdol, mu=400) 

#' one-tailed test: less that 400
t.test(email.df$totdol, mu=400, alternative ="greater") 

#' one-tailed test: greater that 400
t.test(email.df$totdol, mu=400, alternative ="less") 

#' Q2: Test a proportion

#' count the values by the variable "respond"
email.df[,.N,by="respond"]

#'calculate the proportion of the values of "respond"
prop.table(respond.table)

#'binom test 
binom.test(136,1000,0.15) #two-tailed
binom.test(136,1000,0.15, alternative = "greater") #one-tailed
binom.test(100,1000,0.15, alternative = "less") #one-tailed


#+ 2.3 Questions about a relationship between two variables----
#' ## 2.3 Questions about a relationship between two variables
colnames(email.df)

#' scartter plot: age and spend 
#' R has a basic plot function
plot(email.df$age, email.df$totdol, xlab="age", ylab="totdol")

#' we can product a better scatter plot using ggplot
library(ggplot2)
#'geom_point creates scatterplots
ggplot(email.df,aes(x=age, y=totdol)) + geom_point() +
  labs(x="age", y="total dollars spend") + theme_bw()

ggplot(email.df,aes(x=numpurchase, y=totdol)) + geom_point() +
  labs(x="total number of purchases", y="totoal dollars spent") + theme_bw()


#' Q1: Do consumers who respond to emails spend the same account as those who do not? 
# apply a function by group
by(email.df$totdol, email.df$respond, mean)
by(email.df$totdol, list(email.df$respond, email.df$gender), mean)
# Aggregate: use a formula!
aggregate(totdol ~ respond, data=email.df, mean)
aggregate(totdol ~ respond + gender, data=email.df, mean)

aggregate(email.df$totdol, by=list(respond=email.df$respond), mean)

#' independent t test
t.test(totdol~respond,email.df)

#' Q2: Do consumers of a different gender respond to email campaigns with the same rate? 
#' describing the relationship in a two-way table
round(prop.table(table(email.df$respond, email.df$gender),2),3)
with(email.df, round(prop.table(table(respond, gender),2),3))

#' chi-square test for a relationship between two categorical variables
chisq.test(email.df$respond, email.df$gender, correct = F) 

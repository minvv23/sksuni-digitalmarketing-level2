#+ Week 9 ----- Perceptual mapping

#' show current directory
getwd() 

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls()) #remove all the objects

#' # Week 9 
#' importa data 
data<-read.csv("L9_perceptual_mapping.csv")
head(data,10)
str(data)


#' select numeric variables from the dataset
#' remove Resp and Car and CarNo variables 
colnames(data)
data.num<-data[,3:8]

#' quickly visualize the data 
library(ggplot2)
library(GGally)
ggpairs(data.num)

#' check whether the data is suitable for factor analysis 
library(corrplot)
## correlation table
cor(data.num)
## correlation plot
corrplot(cor(data.num), method="number")


#' test whether the data is suitable for factor analysis 
library(psych)
#' KMO test
KMO(data.num)
#' Bartlet test
cortest.bartlett(data.num)

#' Step 1: principle component analysis to decide the number of factors. 
pc <- principal(data.num, nfactors = 6, rotate="none") 
#' show the pca result 
print(pc, digits=3)
#' SS loadings indiciate the eigen values

#' plot scree plot 
plot(pc$values, type="b") 
#' Note: values are eigenvalues; b stands for both point and line. 
#' Based on the eigenvalues, conclude two factors


#' Step 2: factor analysis by PCA 
#' factor analysis with two factors 
fa<-factanal(data.num, factors=2)
print(fa)
#' the following is a beautiful plot 
load.fa<-data.frame(fa$loadings[,1:2])
print(round(load.fa,3))
ggplot(load.fa,aes(x=Factor1, y=Factor2)) + geom_point(color="red") + 
  geom_text(label=rownames(load.fa), nudge_x=0.1) + 
  labs(x="Factor 1", y="Factor 2") + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + theme_bw()



#' use varimax rotation for better interpretation
# install.packages("GPArotation")
library(GPArotation)
fa_rotated<- factanal(data.num, factors =2, rotation = "varimax", scores = "regression") 
print(fa_rotated,digits = 3)

#' the following is a beautiful plot 
load.fa_rotated<-data.frame(fa_rotated$loadings[,1:2])
ggplot(load.fa_rotated,aes(x=Factor1, y=Factor2)) + geom_point(color="red") + 
  geom_text(label=rownames(load.fa_rotated), nudge_x=0.1) + 
  labs(x="Factor 1", y="Factor 2") + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + theme_bw()


#' save scores to a data.table format
scores<-data.frame(round(fa_rotated$scores,3)) 
head(scores,10)

#' compute the average of factor scores across respondents by car brands
data.scores<-cbind(data, scores)
head(data.scores,10)
avg.scores<-aggregate(Factor1~Car, data.scores, mean)
print(avg.scores)
avg.f2scores<-aggregate(Factor2~Car, data.scores, mean)
print(avg.f2scores)
avg.scores$Factor2<-avg.f2scores$Factor2
print(avg.scores)

#' plot scatter point 
ggplot(avg.scores,aes(x=Factor1, y=Factor2)) + geom_point(color="red") + 
  geom_text(label=avg.scores$Car, nudge_x=0.2, nudge_y=-0.1) + 
  labs(x="Fashion", y="Prestige") + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + theme_bw()

#' end 
rm(list=ls())




#+ Additional exercise for factor analysis  -------------------------------------------------
#' Additional exercise for factor analysis

data<-read.csv("L7_college.csv")

library(psych)
#' subset a dataset without the first two variables and name it "fa"
data1<-data[,-c("V1","Private")] 

#' check whether the data is suitable for factor analysis 
#'  KMO test 
KMO(data1) 
#' Bartlet test
print(cortest.bartlett(data1)) 

#' scree plot by PCA
pca<-principal(data1,nfactors = 17, rotate = "none")
print(pca, digits=3)
plot(pca$values, type="b")

#' factor analysis
library(GPArotation)
library(ggplot2)

#' factor analysis without rotation 
fa<- fa(data1, nfactors = 4, rotate = "none", fm="pa", scores = "regression") #' factor analysis by PCA 
print(fa,digits = 3)

#' factor analysis with rotation
fa2<- fa(data1, nfactors = 4, rotate = "varimax", fm="pa", scores = "regression") #' factor analysis by PCA 
print(fa2,digits = 3)

#' factor analysis diagram
#' arrow direction towards variables 
fa.diagram(fa2, digits = 3) 


#' save scores to a data.table format
scores<-data.table(round(fa2$scores,3),keep.rownames = T) 

#' scatter plot by scores of factor 1 and 2

ggplot(scores,aes(x=PA1, y=PA2)) + geom_point(color="darkblue") + 
  labs(x="REGR factor score 1 for analysis 2", 
       y="REGR factor score 1 for analysis 2") + 
  geom_hline(yintercept=0) + geom_vline(xintercept = 0) +
  theme_bw()

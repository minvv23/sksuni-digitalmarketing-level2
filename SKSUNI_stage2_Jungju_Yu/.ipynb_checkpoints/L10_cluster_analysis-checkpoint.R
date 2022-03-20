#+ Week 10 Cluster analysis --------------------
#' # Week 10 Cluster analysis

#' show current directory
getwd() 

#' set work directory 
setwd("/Users/jungju/Dropbox/SK mySUNI/rcodes and data")
rm(list=ls()) #remove all the objects

#' import data 
hotel.df<-read.csv("L10_cluster_analysis.csv")
head(hotel.df,10)
str(hotel.df)
colnames(hotel.df)

## Data cleaning
mean(is.na(hotel.df))
#' change the name to avoid unnecessary errors.
colnames(hotel.df)[colnames(hotel.df)=="Vacation."]<-"VacationMoney" 


## Performing cluster analysis

#' clustering analysis: Hierarchical cluster 
install.packages("cluster")

#' install rtools40 from https://cran.rstudio.com/bin/windows/Rtools/
library(cluster)
library(ggplot2)

#' data preparation before cluster analysis
mydata<-hotel.df[-c(1)]

#' standardize the dataset 
mydata.sc<-scale(mydata)
summary(mydata.sc)
# Note: mydata.sc is a matrix not a data.frame

#' calculate distance
dmatrix<-daisy(mydata.sc, metric = "euclidean",stand=T)
print(dmatrix)

#' cluster analysis 
hc<-agnes(mydata.sc, metric = "euclidean", stand = T, method = "ward")
print(hc)
#' calculate distance
hc$diss

#' plotting dendrogram (clustering tree) for hierarchical clustering
pltree(hc, cex=1,hang=-1)


#' set 3 cluster
clusN<-cutree(hc, k=3)
clusN

#' group the observations
mydata$clusterN<-clusN
hotel.df$clusterN<-clusN

print(mydata)

#' visualize three clusters
pltree(hc,cex=1, hang=-1)
rect.hclust(hc, k=3, border ="red")


## Interpreting results of hierarchical cluster analysis

#' compare values among different clusters
colnames(mydata)
aggregate(cbind(Income,TravelAttitude,VacationImp, HHSize, HHHeadAge, VacationMoney)~clusterN, data=mydata, mean)

#' compute the number of observations in each cluster
table(mydata$clusterN)

#' principal component analysis to produce a cluster plot
library(psych)
pc <- principal(mydata.sc, nfactors = 6, rotate="none") 
#' show the pca result 
print(pc, digits=3)
#' cluster plot
clusplot(mydata.sc, mydata$clusterN, color=T, shade=T, labels=3, lines=0, main="Hierarchical cluster plot")


#' calculate the visit by group
round(aggregate(Visit~clusterN, data=hotel.df, mean),3)


#' k-means cluster analysis
set.seed(96743)
hotel.K<-kmeans(mydata.sc, centers=3)
print(hotel.K)
hotel.K$cluster
mydata$clusterK<-hotel.K$cluster
print(mydata)

aggregate(cbind(Income,TravelAttitude,VacationImp, HHSize, HHHeadAge, VacationMoney)~clusterK, data=mydata, mean)

clusplot(mydata.sc, mydata$clusterK, color=T, shade=T, labels=3, lines=0, main="K-means cluster plot")

#' end
rm(list=ls())




#+  In-class exercise for cluster analysis---------------------------------------

rm(list=ls())

getwd()
setwd("/Users/jungju/Dropbox/Teaching (Selective Sync Conflict 1)/rcodes")

college.df<-read.csv("L10_college.csv")

colnames(college.df)
mean(is.na(college.df))

#' data preparation before cluster analysis 
#' subset a data
college.df2<-college.df[,-(1:2)]
#' standardize the dataset 
college.sc<-data.frame(scale(college.df2))
#' Note: dt3 is a matrix not a data.table 


##' clustering analysis: Hierarchical cluster 
library(cluster)
library(ggplot2)

#' cluster analysis 
hc<-agnes(college.sc, metric = "euclidean", stand = T, method = "ward")

#' calculate distance
dmatrix<-daisy(college.sc, metric = "euclidean",stand=T)
dmatrix

#' show the dendrogram 
pltree(hc,cex=0.6, hang=-1)

#' show agglomeration schedule
summary(hc)
hc$merge
as<-data.frame(hc$merge, hc$height)
as
setnames(as,1:3,c("cluster1","cluster2","height"))
print(as,digits=3)

#' set 3 cluster
clusN<-cutree(as.hclust(hc), k=3)  
#' group the observations
college.df$clusterN<-clusN

pltree(hc,cex=0.6, hang=-1)
rect.hclust(hc, k=3, border =1:3)

#' compare values among different clusters
colnames(college.df)
aggregate(cbind(Private, Apps,Accept, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergrad, Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, perc.alumni, Expend, Grad.Rate)~clusterN, data=college.df, mean)
table(college.df$Private, college.df$clusterN)
table(college.df$clusterN)

#' principal component analysis to produce a cluster plot
pc <- principal(college.sc, nfactors = 17, rotate="none") 
#' show the pca result 
print(pc, digits=3)
#' cluster plot
clusplot(college.sc, college.df$clusterN, color=T, shade=T, labels=3, lines=0, main="Hierarchical cluster plot")

college.df[484,1]

#' end 
rm(list=ls())

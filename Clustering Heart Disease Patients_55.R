#Installing Packages
install.packages("caret")
install.packages("dbscan")
install.packages("tidyverse")
install.packages("cluster")
install.packages("dendextend")
install.packages("corrplot")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("gridExtra")
install.packages("factoextra")
library(caret)
library(dbscan)
library(tidyverse)
library(cluster)
library(dendextend)
library(corrplot)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(factoextra)
library(fpc)
#import data from the file
Heartdiseas<-read.table("Heartdiseas.txt",sep=",",header =TRUE)
heart= Heartdiseas
str(heart)
head(heart)
#take summary about the data
summary(heart)
# check outliers 
boxplot(heart)
#show the value of outlier
boxplot.stats(heart$trestbps)$out
boxplot.stats(heart$chol)$out
boxplot.stats(heart$thalach)$out
boxplot.stats(heart$oldpeak)$out
# make outliers = N/A
outliers <- boxplot(heart$trestbps, plot = FALSE)$out
heart[heart$trestbps %in% outliers, "trestbps"] = NA
outliers <- boxplot(heart$chol, plot = FALSE)$out
heart[heart$chol %in% outliers, "chol"] = NA
outliers <- boxplot(heart$thalach, plot = FALSE)$out
heart[heart$thalach %in% outliers, "thalach"] = NA
outliers <- boxplot(heart$oldpeak, plot = FALSE)$out
heart[heart$oldpeak %in% outliers, "oldpeak"] = NA
heart<- na.omit(heart)
#check redundancy
heart <- unique( heart[,c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope") ] )
boxplot(heart)
str(heart)
###############
#some statistic
###############
###[1]summary about the data##
summary(heart)
###[2]correlation###
heartScale= scale(heart)
HRT_matrix <- data.matrix(heartScale, rownames.force = NA)
M <- cor(HRT_matrix)
corrplot(M, method = "number", number.cex = 0.70, order="hclust")
str(heart)
###[3]variance and standard deviation ###
a<-sd(heart$age)
a
a<-var(heart$age)
a
b<-sd(heart$sex)
b
b<-var(heart$sex)
b
c<-sd(heart$cp)
c
c<-var(heart$cp)
c
d<-sd(heart$trestbps)
d
d<-var(heart$trestbps)
d
e<-sd(heart$chol)
e
e<-var(heart$chol)
e
f<-sd(heart$fbs)
f
f<-var(heart$fbs)
f
g<-sd(heart$restecg)
g
g<-var(heart$restecg)
g
h<-sd(heart$thalach)
h
h<-var(heart$thalach)
h
i<-sd(heart$exang)
i
i<-var(heart$exang)
i
j<-sd(heart$oldpeak)
j
j<-var(heart$oldpeak)
j
k<-sd(heart$slope)
k
k<-var(heart$slope)
k
###############################pie chart
###[1] pie chart for sex###
#convert the numeric to qualitative
q<-as.factor(heart$sex)
gender<-table(q)
#calculate the percentage to the piechart
prc=round(gender/sum(gender)*100)
names=c("Women","Men")
#makes the label for the pie chart
newlabel= paste(names," are ",prc,"%",sep="")
pie(gender,labels = newlabel,main= "the sex of patient",col=rainbow(3))
#the barchart
barplot(gender,xlab="the catogery of sex",ylab ="the category number",main="barchart for sex of patient",names.arg = names,col="BLUE",border ="RED",ylim = c(0,210) )
#the pie chart show women is more then men by 36% consider(men=0, women=1)

###[2] pie chart for cp ###
#convert the numeric to qualitative
r<-as.factor(heart$cp)
catOfData<-table(r)
catOfData
#calculate the percentage to the piechart
prc1=round(catOfData/sum(catOfData)*100)
names=c("Asymptomatic","Atypical Angina","Non-Anginal Pain"," Typical Angina")
#makes the label for the pie chart
newlabel= paste(names," is ",prc1,"%",sep="")
pie(catOfData,labels = newlabel,main= "the cp of the patient",col=rainbow(4))
#the barchart
barplot(catOfData,xlab="the catogery of cp",ylab ="the category number",main="barchart for cp of patient",col="BLUE",border ="RED",density=10,ylim = c(0,200) )

###[3] pie chart for fbs ###
#convert the numeric to qualitative
r<-as.factor(heart$fbs)
catOfData<-table(r)
catOfData
#calculate the percentage to the piechart
prc2=round(catOfData/sum(catOfData)*100)
names=c("< 120 mg/dl","> 120 mg/dl")
#makes the label for the pie chart
newlabel= paste(names," ",prc2,"%",sep="")
pie(catOfData,labels = newlabel,main= "the fbs of the patient",col=rainbow(3))
#the barchart
barplot(catOfData,xlab="the catogery of fbs",ylab ="the category number",main="barchart for fbs of patient", density=10,col="BLUE",border ="RED",ylim = c(0,300) )

###[4] pie chart for exange ###
r<-as.factor(heart$exang)
catOfData<-table(r)
#calculate the percentage to the piechart
prc1=round(catOfData/sum(catOfData)*100)
names=c("Exercise induced angina ","dont has induced angina ")
#makes the label for the pie chart
newlabel= paste(names,"-",prc1,"%",sep="")
pie(catOfData,labels = newlabel,main= "the exange of the patient",col=rainbow(3))
#make the bar chart for exange
barplot(catOfData,xlab="the catogery of exange",ylab ="the category number",main="barchart for exange of patient",names.arg = names,col="BLUE",border ="RED",ylim = c(0,250) )

###[5] pie chart for restecg ###
r<-as.factor(heart$restecg)
catOfData<-table(r)
#calculate the percentage to the piechart
prc1=round(catOfData/sum(catOfData)*100)
names=c("ventricular hypertrophy ","normal","having ST-T wave abnormality ")
#makes the label for the pie chart
newlabel= paste(names,"-",prc1,"%",sep="")
pie(catOfData,labels = newlabel,main= "the exange of the patient",col=rainbow(3))
#the barchart
barplot(catOfData,xlab="the catogery of restecg",ylab ="the category number",main="barchart for restecg of patient",names.arg = names,col="BLUE",border ="RED",ylim = c(0,180) )

###[6] pie chart for slope ###
r<-as.factor(heart$slope)
f<-table(r)
#calculate the percentage to the piechart
prc1=round(f/sum(f)*100)
names=c("downsloping","flat","upsloping")
#makes the label for the pie chart
newlabel= paste(names,"-",prc1,"%",sep="")
pie(f,labels = newlabel,main= "the slope of the patient",col=rainbow(3))
#the barchart
barplot(f,xlab="the catogery of slope",ylab ="the category number",main="barchart for slope of patient",density=10,col="green",border ="RED",ylim = c(0,180) )
###############################################################################################################
#Relations in data
###############################################################################################################
###############
#graphs
###############
mydata.cor = cor(heart)
mydata.cor = cor(heart, method = c("spearman"))
corrplot(mydata.cor)
#plot scatter betwean age and thalac
t<-heart$thalach
q<-heart$age
ggplot(heart, aes(x = q , y = t)) +                 # aesthetic mapping
  geom_point() +                                   # add data points
  stat_smooth(method = lm) +                       # add trend line
  xlim(0, 75) +                                        # change plotting limits of x-axis 
  labs(x = "age", y = "the highest heart rate")
#histogram betwean cp and maximum heart rate 
t<-heart$thalach
q<-heart$cp
p<-ggplot(heart, aes(x=t)) +
  geom_histogram(color="white", fill="red")# Separate columns according to cp
p +   facet_wrap(q,ncol=4)
# bar chart betwean exang and cp
t<-heart$exang
counts <- table(q, t)
text(0.42, 7500, labels = "cp with exang")
barplot(counts, legend = rownames(counts), col = rainbow(4), xlab = "exang", beside = F,
        args.legend = list(x = "topleft"))
#histogram betwean exang and maximum heart rate 
t<-heart$thalach
q<-heart$exang
p<-ggplot(heart, aes(x=t)) +
  geom_histogram(color="white", fill="red")# Separate columns according to cp
p +   facet_wrap(q,ncol=2)
#plot scatter betwean oldpeak and thalac
t<-heart$thalach
q<-heart$oldpeak
ggplot(heart, aes(x = q , y = t)) +                 # aesthetic mapping
  geom_point() +                                   # add data points
  stat_smooth(method = lm) +                       # add trend line
  xlim(0, 1) +                                        # change plotting limits of x-axis 
  labs(x = "age", y = "the highest heart rate")
#histogram betwean slope and maximum heart rate 
t<-heart$thalach
q<-heart$slope
p<-ggplot(heart, aes(x=t)) +
  geom_histogram(color="white", fill="red")# Separate columns according to cp
p +   facet_wrap(q,ncol=3)
#histogram betwean slope and oldpeak
t<-heart$oldpeak
q<-heart$slope
p<-ggplot(heart, aes(x=t)) +
  geom_histogram(color="white", fill="red")# Separate columns according to cp
p +   facet_wrap(q,ncol=3)

###############################################################################################################
###############################################################################################################

###create histogram
#create histogram to age in our data
q<-heart$age
hist(q,main="age histogram",xlab="range of ages",ylab="frequency",xlim = c(20,80),ylim=c(1,77),col ="BLUE",border = "BLACK" )

#create histogram to trestpbs in our data
q<-heart$trestbps
hist(q,main="trestbps histogram",xlab="range of trestbps",ylab="frequency",xlim = c(90,200),ylim=c(1,77),col ="tan3",border = "BLACK" )

#create histogram to chol in our data
q<-heart$chol
hist(q,main="chol histogram",xlab="range of chol",ylab="frequency",xlim = c(100,600),ylim=c(1,145),col ="darkgray",border = "BLACK" )

#create histogram to thalach in our data
q<-heart$thalach
hist(q,main="thalach histogram",xlab="range of thalach",ylab="frequency",xlim = c(65,202),ylim=c(1,60),col ="tomato3",border = "BLACK" )

#create histogram to oldpeak in our data
q<-heart$oldpeak
hist(q,main="oldpeak histogram",xlab="range of oldpeak",ylab="frequency",xlim = c(0,6.4),ylim=c(1,150),col ="#7CE3D8",border = "BLACK" )

############################################################################################################################################

#normalize data to use it in unsupervisor learning
process <- preProcess(as.data.frame(heart), method=c("range"))
norm_scale <- predict(process, as.data.frame(heart))
##########################################################################################################################################
#[1] first method to find optimal value of clusters (K) to do the clustering techniques
wss=numeric(15)   
for(i in 1:15) wss[i]=sum(kmeans(heart,i)$withinss)
plot(1:15,wss,type='b',xlab='number of cluster',ylab='within grp sum square')
#[2] second method to find optimal value of clusters (K) to do the clustering techniques
x <- fviz_nbclust(heartScale, FUNcluster = kmeans, method = "silhouette") + theme_bw() 
grid.arrange(arrangeGrob(x, ncol=1, nrow=1), heights=c(10,1), widths=c(10,1))
#According to the results, the optimal number of clusters is 2 ,3 for Kmeans and hierarchical clustering
##########################################################################################################################################
#[a]KMEANS CLUSTERING
##########################################################################################################################################
#[1]first cluster without number k=2
km1 =kmeans(heart,2)
autoplot(km1,heart,frame=TRUE)
#[2]second cluster sihouette plot k=2 
cl_kmeans2 <- eclust(heart, k=2, FUNcluster="kmeans", hc_metric="pearson", graph=FALSE)
cl_kmeans3 <- eclust(heart, k=3, FUNcluster="kmeans", hc_metric="pearson", graph=FALSE)
a1 <- fviz_silhouette(cl_kmeans2)
grid.arrange(arrangeGrob(a1, ncol=1, nrow=1), heights=c(10,1), widths=c(10,1))
#[3]third cluster  k=3
km2 =kmeans(heart,3)
autoplot(km2,heart,frame=TRUE)
#[4]fourth cluster sihouette plot k=3
a2 <- fviz_silhouette(cl_kmeans3)
grid.arrange(arrangeGrob(a2, ncol=1, nrow=1), heights=c(10,1), widths=c(10,1))
#[6]mix veiw 
grid.arrange(a1,a2)
##########################################################################################################################################
#[b]K-Med CLUSTERING
##########################################################################################################################################
#load data
dFrame <- heart
#scale each variable to have a mean of 0 and sd of 1
dFrame <- scale(dFrame)
#Number of Clusters is 2
#[1]perform k-medoids clustering with k = 2 clusters
kmed2 <- pam(dFrame, k = 2)
#plot results of final k-medoids model k=2
autoplot(kmed2,frame=TRUE)
#add cluster assignment to original data
#[2]perform k-medoids clustering with k = 3 clusters
kmed3 <- pam(dFrame, k = 3)
#view results
kmed3
#plot results of final k-medoids model k=3
autoplot(kmed3,frame=TRUE)
##########################################################################################################################################
#[c]DBSCAN CLUSTERING
##########################################################################################################################################
process <- preProcess(as.data.frame(heart), method=c("range"))
norm_scale <- predict(process, as.data.frame(heart))

# Fitting DBScan clustering Model 
# to training dataset
Dbscan_cl <- dbscan(norm_scale, eps = 0.45, MinPts = 5)
Dbscan_cl
# Checking cluster
Dbscan_cl$cluster
# Table
#table(Dbscan_cl$cluster, norm_scale)
# Plotting Cluster
plot(Dbscan_cl, heart, main = "DBScan")
d=heart[,c("id","age","trestbps","chol","thalach","oldpeak")]
plot(Dbscan_cl, d, main = "DBScan")
##########################################################################################################################################
#[d]DIVISIVE HIERARCHICAL CLUSTERING
##########################################################################################################################################
DH <- diana(heartScale)
DH$dc
# Plot obtained dendrogram
# Finding distance matrix
mtcars=heart
distance_mat <- dist(mtcars, method = 'euclidean')
distance_mat
# Fitting Hierarchical clustering Model
# to training dataset
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl
pltree(DH, cex = 0.6, hang = -1,main = "Dendrogram of data") 
plot(Hierar_cl, cex = 0.3)
##########################################################################################################################################
#[e]AGGLOMERATIVE HIERARCHICAL CLUSTERING
##########################################################################################################################################
###########################
d <- dist(heartScale, method = "euclidean")
AG1 <- hclust(d, method = "single" )
plot(AG1, cex = 0.6, hang = -1)
#=============================================================================
#[2]AVERAGE LINKAGE
d <- dist(heartScale, method = "euclidean")
AG2 <- hclust(d, method = "average" )
plot(AG2, cex = 0.6, hang = -1)
#=============================================================================
#[3]complete LINKAGE
d <- dist(heartScale, method = "euclidean")
AG3 <- hclust(d, method = "complete" )
plot(AG3, cex = 0.6, hang = -1)
#=============================================================================
#[4]WARD LINKAGE
d <- dist(heartScale, method = "euclidean")
AG4 <- hclust(d, method = "ward.D" )
plot(AG4, cex = 0.6, hang = -1)
#=============================================================================


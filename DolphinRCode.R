#Start Code
#you need to have the xtrain data to do this
library(reticulate)
np <- import("numpy")
matTrain <- np$load("DOCC10_train/DOCC10_Xtrain.npy")
myData1 <- matTrain[1,1:100]
myFreqData1 <- (200000*(myData1/2))/2
secData1 <- 1/(myData1*1000)
secData1
myFreqData1

counts <- table(myFreqData1)
barplot(counts, main="Click Distribution",
xlab="Number of Clicks")

#Bin Frequency Table and Graph
binFreqTable <- function(myFreqData1, bins) {
   freq = hist(myFreqData1, breaks=bins, include.lowest=TRUE, plot=FALSE)
    ranges = paste(head(freq$breaks,-1), freq$breaks[-1], sep=" - ")
    return(data.frame(range = ranges, frequency = freq$counts))
 }
binFreqTable(myFreqData1,10)

#sort and abs to handle neg/pos frequency values
mydataA <- sort(abs(myFreqData1))

#Calculates root mean square (RMS) amplitude in overlapping frames, providing an envelope of
#RMS amplitude as a measure of sound intensity
install.packages("soundgen")
library(soundgen)
getRMS(
    myFreqData1,
    samplingRate = 200,
    windowLength = 50,
    step = NULL,
    overlap = 75,
    killDC = FALSE,
    scale = 100,
    normalize = TRUE,
    windowDC = 200,
    plot = TRUE,
    xlab = "Time, ms",
    ylab = "",
    type = "b",
    col = "blue",
    lwd = 2,
)

#Histograms of Freq vs. Time or Sample (Cluster) Numbers vs. Freq.

hist(myFreqData1, main = "Dolphin Click Set A", xlab="Time", ylab = "Frequency", col = "darkmagenta",freq=FALSE)
# colorful plot with colors by group showing clusters between echolocation clicks


plot(myFreqData1, timeData1, pch = 15, col = rainbow(10))

#Fourier Frequency Function - that I could not get to work

dt <- 0.01 #s
 n <- T/dt
 F <- 1/dt
 df <- 1/T
 freq <- 5 #Hz
 t <- seq(0,T,by=dt)
 #create time series data
 t<- posData1
 y <- 10*sin(2*pi*freq*t) +4* sin(2*pi*20*t)
#Frequency Array
 f <- 1:length(t)/T
#Fourier Transform Work
 Y <- fft(y)
 mag <- sqrt(Re(Y)^2+Im(Y)^2)*2/n
 phase <- atan(Im(Y)/Re(Y))
 Yr <- Re(Y)
 Yi <- Im(Y)
#plotting

 #inside # does not run for me -Dylan
layout(matrix(c(1,2),2,1,byrow=TRUE))


plot(f[1:length(f)/2],mag[1:length(f)/2],type="l",xlab= "Frequency (kHz)",ylab= "Amplitudes") 
grid(NULL,NULL, col = "lightgray", lty = "dotted",lwd = 1) 

#plots frequency data as a linear curve; not much different histogram just shows a linear line instead of histogram
 fade(
    myFreqData1,
    fadeIn = 1000,
    fadeOut = 1000,
    samplingRate = NULL,
    shape = c("lin", "exp", "log", "cos", "logistic")[1],
    steepness = 1,
    plot = TRUE
)








#Kmeans alt method
mydata = myFreqData1
 mydata = as.data.frame(unclass(mydata))
 dim(mydata)
[1] 101   1
summary(mydata)
 unclass(mydata)   
#remove NAs
 myDataClean = na.omit(mydata)
 dim(myDataClean)
 scaled_data = as.matrix(scale(myDataClean))
 #k mean clusters
 kmm = kmeans(scaled_data,3,nstart = 50, iter.max = 15)
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# convert frequency data to music notes

##############where did posdata1 and timedata4 come from
s = HzToSemitones(posData1, ref = 0.5109875)
notesDict$note[1 + round(s)]

#getting rid of Infinity values for time data
timeData4[timeData4==-Inf]<- 0
###########################################
#turns clicks into audio tones
fart(
glottis = c(50, 200),
pitch = 65,
temperature = 0.25,
sylLen = 600,
rolloff = -10,
samplingRate = 16000,
play = TRUE
plot = FALSE
)







































#continues exploratory analisis of the data to look for additional helpful information

mydata2 = myFreqData1
mydata2 = as.data.frame(unclass(mydata))
 plot(1:k.max, wss,
+      type="b", pch = 19, frame = FALSE, 
+      xlab="Number of clusters K",
+      ylab="Total within-clusters sum of squares")
library(cluster)

#could not get next to work 
#clusplot(myDataClean, fit$cluster, color=TRUE, shade=TRUE,
#+    labels=2, lines=0))
install.packages("pvclust")

library(pvclust)
#next would not work either because n>=2 objects to cluster
 #fit <- pvclust(myDataClean, method.hclust="ward",
#+    method.dist="euclidean")

fit <- kmeans(mydata2, 5) 
aggregate(mydata2,by=list(fit$cluster),FUN=mean)
 mydata2 <- data.frame(mydata2, fit$cluster)
 d <- dist(mydata2, method = "euclidean")
fit <- hclust(d, method="ward")
 plot(fit)
groups <- cutree(fit, k=5) 
rect.hclust(fit, k=5, border="red")
library(pvclust)

fit <- pvclust(mydata2, method.hclust="ward",
+                method.dist="euclidean")

#Model based approaches assume a variety of data models and apply maximum likelihood estimation and Bayes criteria to identify the most likely model and number of cluster
install.packages("mclust")
library(mclust)
fit <- Mclust(mydata2)
plot(fit)


#my attempt to get get a nice kmeans clustering graph to show groupings of points thus far but am stuck on fvizcluster
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
library(tidyverse)
library(cluster)
library(factoextra)
test2<-myFreqData1
test2=as.data.frame(unclass(test2))
dim(test2)
test2C=na.omit(test2)
test2C<-scale(test2C)
head(test2C)
k<-kmeans(test2C,5,nstart=25)
str(k)
# error about not having an x and y becuase of how limited data is fvis_cluster(k,data=test2C)
#trying a bunch of methods to see withiness and to get some visuals to see if anything interesting about the clusters shows up
myDataClean <- scale(myDataClean)
wss <- (nrow(myDataClean)-1)*sum(apply(myDataClean,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(myDataClean,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
fit <- kmeans(myDataClean, 5)
aggregate(myDataClean,by=list(fit$cluster),FUN=mean)
myDataClean <- data.frame(myDataClean, fit$cluster)
dis <- dist(myDataClean, method = "euclidean")
fit <- hclust(dis, method="ward")
plot(fit) 

fit <- Mclust(myDataClean)
plot(fit) # plot results
summary(fit)
clusplot(myDataClean, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
library(fpc)
plotcluster(myDataClean, fit$cluster)











#Read the data into R then labelled the dataset
my_data = read.table("nci.data.txt", header = FALSE, sep = "", dec = ".")
labels = read.table("label.txt", header = FALSE, sep = "", dec = ".")
names(my_data) = labels[,1]

my_data = t(my_data)
a = dim(my_data)

#############################Single-linkage#####################################

#Commented code to show labels in list but, because of duplicate labels I used 
#numbers. Uncomment to change list clusters shown as numbers to label values.

#The number list number showsat which point clusters join.
#The top of the dendrogram is shown as the last list number for each list
#If a new cluster joins it will be shown as below:
#e.g 
# [1] 10,11
# [2] 10,11,12


source('Single.R')
SLM = Single()

# for(i in 1:length(SLM)){
#   for(j in 1:length(SLM[[i]])){
#     SLM[[i]][j] = labels[SLM[[i]][j],1]
#   }
# }

#############################Complete-linkage###################################

source('Complete.R')
CLM = Complete()

# for(i in 1:length(CLM)){
#   for(j in 1:length(CLM[[i]])){
#     CLM[[i]][j] = labels[CLM[[i]][j],1]
#   }
# }

#############################Average-linkage####################################

source('Average.R')
ALM = Average()

# for(i in 1:length(ALM)){
#   for(j in 1:length(ALM[[i]])){
#     ALM[[i]][j] = labels[ALM[[i]][j],1]
#   }
# }

#############################Centroid-linkage###################################

source('Centroid.R')
CDLM = Centroid()

# for(i in 1:length(CDLM)){
#   for(j in 1:length(CDLM[[i]])){
#     CDLM[[i]][j] = labels[CDLM[[i]][j],1]
#   }
# }

#############################K-means Clustering#################################


#Used each of these K means commented out K means not chosen

#km.out <- kmeans(my_data, 2, nstart=20)
#plot(my_data, col=(km.out$cluster+1), main="K-Means Clustering Results
#+ with K=2", xlab="", ylab="", pch=20, cex=2)

# km.out <- kmeans(my_data, 3, nstart=20)
# plot(my_data, col=(km.out$cluster+1), main="K-Means Clustering Results
# with K=3", xlab="", ylab="", pch=20, cex=2)

km.out <- kmeans(my_data, 4, nstart=20)
plot(my_data, col=(km.out$cluster+1), main="K-Means Clustering Results
with K=4", xlab="", ylab="", pch=20, cex=2)

#km.out <- kmeans(my_data, 5, nstart=20)
#plot(my_data, col=(km.out$cluster+1), main="K-Means Clustering Results
#with K=5", xlab="", ylab="", pch=20, cex=2)

#km.out <- kmeans(my_data, 6, nstart=20)
#plot(my_data, col=(km.out$cluster+1), main="K-Means Clustering Results
#with K=6", xlab="", ylab="", pch=20, cex=2)

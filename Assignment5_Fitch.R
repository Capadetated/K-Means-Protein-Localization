# This program contains all the R code used to explore the ECOLI dataset for assignment 5
# Data 630 Assignment 5
# Ted Fitch
# Last updated 03AUG21

# Section 1 - Introduction /////
# Install packages
install.packages("cluster")
library("cluster")

# Section 2 - Read the CSV file /////
# Change the file path to the file location on your hard drive.
setwd("C:/Users/soari/Documents/Assignments/Data Analytics/UMGC/Summer 2021 Data 630/Assignment 5")
ECOLI <- read.csv("ecoli.csv")

# Begin EDA
str(ECOLI)
summary(ECOLI)
ECOLI$class<-factor(ECOLI$class)
ECOLI$chg<-factor(ECOLI$chg)
ECOLI$lip<-factor(ECOLI$lip)
summary(ECOLI)
head(ECOLI)

#table(ECOLI$Class)

# Make plots
# barplot of class distribution
x<- ECOLI$class
x
cnt <- table(x)
cnt
barplot (cnt,main="Distribution of Classes",
         xlab="Classes",
         ylab="Count",
         border="brown",
         col="brown", space =1.0,beside=TRUE,)

# return to numeric type
ECOLI$chg<-as.numeric(ECOLI$chg)
ECOLI$lip<-as.numeric(ECOLI$lip)
str(ECOLI)

# Section 3 - Data Preprocessing /////
# Remove class variable
# ECOLI dataset has no class variable
newECOLI<-ECOLI
newECOLI$class<-NULL

#head(newECOLI)
# No further preprocessing

# Section 4 - Model 1 /////__________________________________
# Section 4.1 - model prep
#make sure that the result is reproducible
set.seed(1234)

#scale the variables
newECOLI[1:7]<-scale(newECOLI[1:7])
summary(newECOLI)

# Compute best cluster number using elbow method:
# Function to compute total within-cluster sum of squares:
install.packages("factoextra")
library("factoextra")
fviz_nbclust(newECOLI, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("The Elbow Method: Finding Ideal Cluster Number")

# Compute best cluster number using the gap statistic:
gap_stat <- clusGap(newECOLI, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")


#Run the method and store the result in kc variable
kc<-kmeans(newECOLI, 4)

#output the result
print(kc)
kc$centers
kc$totss
kc$iter
kc$betweenss
kc$tot.withinss

# Section 4.2 - cluster to class evaluation /////
# Raw Numbers in each cluster:
table(ECOLI$class, kc$cluster)
# Percentages in each cluster:
100 * round(prop.table(table(ECOLI$class, kc$cluster)), digits = 2)

# Section 4.3 - cluster plot /////
clusplot(newECOLI, kc$cluster, color=TRUE, shade=TRUE)

# Section 4.4 - Anomaly detection /////
# Find centers
centers <- kc$centers[kc$cluster, ]
head(centers, 15)
# Find distances
distances <- sqrt(rowSums((newECOLI - centers)^2))
distances
# Find all outliers (> 3SD from mean)
OTLR <- mean(distances) + 3*sd(distances)# Statistical definition of outliers (> 3SD above / below mean)
View(distances)
ALLOTLR <- which(distances > OTLR)
ALLOTLR
ECOLI[ALLOTLR,]
# Find top 5 outliers (based on highest distances)
outliers <- order(distances, decreasing=T)[1:5]
outliers
ECOLI[outliers,]

# Section 5 - Model 2 /////__________________________________
# Use 8 clusters
# Section 5.1 - model prep
RECOLI <- ECOLI
#make sure that the result is reproducible
set.seed(1234)
#scale the variables
RECOLI[1:7]<-scale(RECOLI[1:7])
summary(RECOLI)
# Remove class
XECOLI <- RECOLI
RECOLI$class<-NULL
summary(RECOLI)
str(RECOLI)

#Run the method and store the result in kc variable
kc<-kmeans(RECOLI, 8)

#output the result
print(kc)
kc$centers
kc$totss
kc$iter
kc$betweenss
kc$withinss

# Section 5.2 - cluster to class evaluation /////
# Raw Numbers in each cluster:
table(XECOLI$class, kc$cluster)
# Percentages in each cluster:
100 * round(prop.table(table(XECOLI$class, kc$cluster)), digits = 2)

# Section 5.3 - cluster plot /////
clusplot(RECOLI, kc$cluster, color=TRUE, shade=TRUE)


# Section 6 - Model 3 /////__________________________________
# Section 6.1 - model prep
# Which cluster were they a part of?
kc$cluster[c(183,223,224,252,275,280,281,282,283,284)]

# Remove outliers:
RECOLI <- ECOLI[-c(183,223,224,252,275,280,281,282,283,284), ]
str(RECOLI)

#make sure that the result is reproducible
set.seed(1234)
#scale the variables
RECOLI[1:7]<-scale(RECOLI[1:7])
summary(RECOLI)
# Remove two binary variables
XECOLI <- RECOLI
RECOLI$lip<-NULL
RECOLI$chg<-NULL
RECOLI$class<-NULL
summary(RECOLI)
str(RECOLI)

#Run the method and store the result in kc variable
kc<-kmeans(RECOLI, 8)

#output the result
print(kc)
kc$centers
kc$totss
kc$iter
kc$betweenss
kc$withinss

# Section 6.2 - cluster to class evaluation /////
# Raw Numbers in each cluster:
table(XECOLI$class, kc$cluster)
# Percentages in each cluster:
100 * round(prop.table(table(XECOLI$class, kc$cluster)), digits = 2)

# Section 6.3 - cluster plot /////
clusplot(RECOLI, kc$cluster, color=TRUE, shade=TRUE)



# End of script
getwd()
setwd("../Desktop/Data_Mining/Assignment_2")
getwd()
cardata<-read.table("car.data",header = FALSE,sep = "," )
names(cardata)<- c("Buying","Maintenance","Doors","Persons","lug_boot","safety","Class_Value")
data.frame(cardata)
DF <- data.matrix(cardata)
dim(DF)
library(SnowballC)
library(lsa)
#Question 1: Cars with highest similarities
car_sim <- cosine(DF)
dim(car_sim)
car_sim <- cosine(t(DF))
dim(car_sim)
 
max(car_sim)
#Question 2: Cars with highest dissimilarities
car_dissim <- 1-cosine(DF)
dim(car_dissim)
car_dissim <- 1-cosine(t(DF))
dim(car_dissim)
max(car_dissim)
getwd()
cardata<-read.table("car.data",header = "FALSE",sep = ",")
names(cardata)<- c("Buying","Maintenance","Doors","Persons","lug_boot","safety","Class_Value")
data.frame(cardata)
a=as.matrix(cardata)
a

#Question 1: Cars with highest similarities

b = matrix(nrow =1728 , ncol = 1728)

count1 = function(ii,ij){
  count2 = 0
  for (j in (1:7)) {
    if(a[ii,j] == a[ij,j]){
      count2 = count2 + 1 #(count2 - 1) for dissimilarity Q1 and Q2
    }
  }
  return(count2)
}
for (i in 1:1728){
  for (j in (1:1728)){
    if(i==j) {
      b[i,j] = 0
    }
    
    else{
      count3 = count1(i,j)
      b[i,j] = count3 / 7
    }
  }
}
b
max(b)
#Question 2: Cars with highest dissimilarities

c = matrix(nrow =1728 , ncol = 1728)

count1 = function(ii,ij){
  count2 = 0
  for (j in (1:7)) {
    if(a[ii,j] == a[ij,j]){
      count2 = count2 + 1 #(count2 - 1) for dissimilarity Q1 and Q2
    }
  }
  return(count2)
}
for (i in 1:1728){
  for (j in (1:1728)){
    if(i==j) {
      c[i,j] = -0.0000001
    }
    
    else{
      count3 = count1(i,j)
      c[i,j] = 1-(count3 / 7)
    }
  }
}
c
max(c)

#Question 4:
data.frame(cardata)
VG <- cardata[cardata$Class_Value=="vgood",]

m=as.matrix(VG)
m

n = matrix(nrow =65 , ncol = 65)

count1 = function(ii,ij){
  count2 = 0
  for (j in (1:7)) {
    if(m[ii,j] == m[ij,j]){
      count2 = count2 + 1 #(count2 - 1) for dissimilarity Q1 and Q2
    }
  }
  return(count2)
}
for (i in 1:65){
  for (j in (1:65)){
    if(i==j) {
      n[i,j] = -0.0000001
    }
    
    else{
      count3 = count1(i,j)
      n[i,j] = 1-(count3 / 7)
    }
  }
}
n
max(n)



#Question 3:
COR_Data <- data.matrix(cardata)
DF_Corr<- as.data.frame(COR_Data)
#Significance Level: 0.01
cor.test(DF_Corr$Persons,DF_Corr$Class_Value,method="pearson",conf.level = 0.99)

#Significance Level: 0.05 
cor.test(DF_Corr$Persons,DF_Corr$Class_Value,method="pearson",conf.level = 0.95)

#Significance Level: 0.1
cor.test(DF_Corr$Persons,DF_Corr$Class_Value,method="pearson",conf.level = 0.90)

#Scatter PLot
plot(DF_Corr$Persons,DF_Corr$Class_Value,xlab = "Persons",ylab = "Class_Value")


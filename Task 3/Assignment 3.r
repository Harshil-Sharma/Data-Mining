install.packages("arules")
install.packages("splitstackshape")
library(arules)
library(splitstackshape)
rm(list=ls(all=TRUE))
mydata <-read.delim(file.choose(), header = FALSE)
mydata$v2<-c(seq(1:246))
mydata<-mydata[,c(2,1)]
H<-cSplit(mydata,"V1"," ","long")
write.table(H, file="forests.csv", sep=",", col.names = FALSE, row.names = FALSE)
HS<-read.csv(file="forests.csv",header =FALSE,sep=",")
trans = read.transactions(file="forests.csv", rm.duplicates= FALSE, format="single",sep=",",cols =c(1,2))
inspect(trans)
support<-0.50
library(arules)
#question 1
fp <- apriori(trans, parameter = list(target = "frequent itemsets", supp=support, minlen=1), control = list(verbose = FALSE))
sets_order_supp <- DATAFRAME(sort(fp, by="support", decreasing = F))
View(sets_order_supp)
#question 2
maximal <-is.maximal(fp)
inspect(fp[maximal])
#question 3
rules <- apriori(trans,parameter = list(sup = 0.40, conf = 0.7,target="rules"))
inspect(rules)
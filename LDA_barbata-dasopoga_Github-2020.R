library(MASS)
library(klaR)
library(ade4)

#######################################################################################################
#Importing the data file with the two species U. barbata and U. dasopoga (n=216 in total)
BarbaDaso<-read.table("Add the path to the place you registered the file\\barbata-dasopoga.txt", header=T)
#For the variable names to be be used
attach(BarbaDaso)
#This is to list the variable names for control
names(BarbaDaso)

#Linear discriminant analysis without cross validation (CV=F). 
#The brackets [,5:14] indicate that we will work with variables n°5 to 14
#The discriminant factor is the variable ESPECE
lcBarbaDaso <-lda(BarbaDaso[,4:10], grouping=Species, CV=F)
lcBarbaDaso

#Compute the coordinates on the discriminant axis and the a posterior probabilities 
plcBarbaDaso <- predict(lcBarbaDaso)
plcBarbaDaso
#Gives the assignment of each individual to an a posteriri class
plcBarbaDaso$class

#Displays the confusion table: the lines correspond to the apriori classification whereas the columns coprrespond t
#o the a posteriori classification 
table(BarbaDaso$Species,plcBarbaDaso$class)

#Identification of the best discriminant variables
disc<-stepclass(BarbaDaso[,4:10], Species, improvement = 0.01, method = "lda")
disc

###############################################################################################################
#Import of the dataset with the doubtful individuals (with a suffix PC) 
BarbaDasoPC<-read.table("Add the path to the place you registered the file\\barbata-PC-dasopoga-PC.txt", header=T)
#For the variable names to be be used
attach(BarbaDasoPC)
#This is to list the variable names for control
names(BarbaDasoPC)

#Comparison of doubtful individuals using the former discriminant analysis
plctest <- predict(lcBarbaDaso, BarbaDasoPC[,4:10])
plctest
#Displays the confusion table: the lines correspond to the apriori classification whereas the columns coprrespond t
#o the a posteriori classification 
table(BarbaDasoPC$Species,plctest$class)

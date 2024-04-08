Banana<-data.frame(Banana)
Banana
response<-cbind(Banana$TS,Banana$Elasticity)
response
Time<-as.factor(Banana$Time)
Time
manova.fit<-manova(response~Time)
manova.fit

summary (manova.fit, "Wilks")
summary (manova.fit, "Hotelling-Lawley")
summary (manova.fit, "Pillai")
summary (manova.fit, "Roy")

install.packages("mvnormtest")
library("mvnormtest")
response_transpose<-t(response)
response_transpose
mshapiro.test(response_transpose)

my_mahalanobis<-data.frame(response)
my_mahalanobis
p<-ncol(my_mahalanobis)
p
my_mahalanobis$distance<-mahalanobis(my_mahalanobis,
                                     center = colMeans(my_mahalanobis),
                                     cov(my_mahalanobis))
my_mahalanobis$p_value<-pchisq(my_mahalanobis$distance,,df=p-1,lower.tail = FALSE)
colnames(my_mahalanobis)<-c("TS","Elasticity","Mahal_distance","p-value")
print(my_mahalanobis)

cor.test(response[,1],response[,2],method = "pearson")

install.packages("heplots")
library("heplots")
boxM(Y=Banana[,c("TS","Elasticity")],group=Banana$Time)

#Author: Kubam Ivo
#Date: 08/11/2019
#Course: Topics In Advance Modelling Techniqes

install.packages("CAMAN")
install.packages("sas7bdat")
require("sas7bdat")
require("CAMAN")

#Reading in sas dataset;
headache<-read.sas7bdat("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Advanced Modelling Techniques\\Project\\acuamt2019.sas7bdat")
head(headache)
headache$sex<-as.factor(headache$sex)
headache$group<-as.factor(headache$group)
headache$migraine<-as.factor(headache$migraine)
headache$frequency<-as.integer(headache$frequency)
str(headache)
head(headache)
max(headache$frequency)
min(headache$frequency)
max(headache2$Count)

#Creating a dataset with counts of headache and frequency from the headache dataset;
#Unique counts of headache;

headache2<-as.data.frame(table(as.integer(headache$frequency)))
colnames(headache2)<-c("Count","freq")
headache2$Count<-as.numeric(headache2$Count)-1


str(headache2)
head(headache2)

write.csv(headache2,file = "E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Advanced Modelling Techniques\\Project\\headache2.csv")

#Exploring the Headache Dataset
summary(headache)
par(mfrow=c(1,3))
hist(headache$frequency,xlab = "Number of headaches",plot = TRUE,breaks = 10,main="Observed",probability = T,xlim = c(0,30))
hist(rpois(200,c(0.143,2.817,8.164,16.156)),xlab = "Number of headaches",main="Finite mixture k=5",breaks = 10,probability = T,ylim = c(0,0.2),xlim = c(0,30))
hist(rpois(1000,2.232),xlab="Number of headaches",breaks = 10,main="Single Poisson",probability = T,ylim = c(0,0.35))



#Estimating the number of support points using NPMLE
#Phase 1 and 2 combined
npml1<-mixalg(obs =headache2$Count,weights = headache2$freq,family = "poisson",data=headache2,acc = 10^(-6),numiter=500000,startk = 50)
npml1
summary(npml1)

##Classification##

class<-round(cbind(headache2,npml1@prob,npml1@classification),digits=4)

class_tot<-NULL

for (x in 1:6){
  total=0
  for(i in 1:29){
    if(npml1@classification[i]==x){
      total=total+headache2$freq[i]
    }
  } 
  class_tot[x]<-c(total)
}

component<-c(1,2,3,4,5,6)
num_headache<-class_tot
prop_comp<-class_tot/401
Proir_prob<-c(0,0.2577944,0.05650109,0.2792075,0.2482094,0.1582169)
sum(num_headache)
class_table<-cbind(component,num_headache,prop_comp,Proir_prob)

headache

##INFORMAL CHECK FOR SUBPOPULATION DETECTED BY GROUP#####

age_uniq<-unique(headache$age)
lambda_values<-c(1:length(unique(headache$age)))

for(i in unique(headache$age)){
  b<-1
  lambda_values[b]<-exp(3.1600+0.001126*i+0.2095 -0.1328 -0.6490) 
  b<-b+1
}

plot(lambda_values~age_uniq)
plot

predicted<-read.sas7bdat("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Advanced Modelling Techniques\\Project\\out_pred.sas7bdat")


for(i in 1:nrow(predicted)){
  if (predicted$Class[i]==1)
  {predicted$fitted[i]<-predicted$Pred_1[i]}
  else if (predicted$Class[i]==2)
  {predicted$fitted[i]<-predicted$Pred_2[i]}
  else if (predicted$Class[i]==3)
  {predicted$fitted[i]<-predicted$Pred_3[i]}
  else if (predicted$Class[i]==4)
  {predicted$fitted[i]<-predicted$Pred_4[i]}
  else if (predicted$Class[i]==5)
  {predicted$fitted[i]<-predicted$Pred_5[i]}
  else if (predicted$Class[i]==6)
  {predicted$fitted[i]<-predicted$Pred_6[i]}
}
head(predicted)
write.csv(predicted,"E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Advanced Modelling Techniques\\Project\\predicted.csv")

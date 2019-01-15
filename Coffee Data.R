library(ggplot2)
library(dplyr)
library(broom)


#Assign Values
Age<-Final_Coffee$Age
Beans<-Final_Coffee$Beans
Value<-Final_Coffee$Value
Online<-Final_Coffee$Online
D_Online<-Final_Coffee$D_Online

#Plot Initial Data
ggplot(data=Final_Coffee,aes(x=Final_Coffee$Age,y=Final_Coffee$Online))+
  geom_jitter(width = 0,height=.05,alpha=.5)

#Logistic Model
model1<-glm(as.factor(Online)~Age+Value,data=Final_Coffee,family=binomial)

#Model Summary
summary(model1)

#Plot With Line
ggplot(data=Final_Coffee,aes(x=Final_Coffee$Age,y=D_Online))+
  geom_jitter(width = 0,height=.05,alpha=.5)+
  geom_smooth(method="glm",se=0,color="red",method.args=list(family="binomial"))+
  ggtitle("Logistic Regression of Age and Willingness to Purchase Coffee Online")+
  ylab("Online Purchase (1=Yes,0=No)")+
  xlab("Age")+
  theme(plot.title = element_text(face="bold"))

#Probability Scale
probability_scale<-model1 %>%
  augment(type.predict="response")%>%
  mutate(Y_hat=.fitted)

#Assign Scale Values
SC_Age<-probability_scale$Age
SC_Online <- probability_scale$as.factor.Online.
SC_Value<-probability_scale$Value
Y_hat<-probability_scale$Y_hat

#Plot Scale
ggplot(probability_scale,aes(x=SC_Age,y=Y_hat))+
  geom_point()+
  geom_line()+
  scale_y_continuous("Probability of Purchasing Coffee Online",limits=c(0,1))

#Prediction Function
Coffee_Prediction<-function(x,y){predict(model1,data.frame(Age=x,Value=y),type="response")}







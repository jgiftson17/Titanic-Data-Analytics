library(dplyr)
library(ggplot2)
View(train)
train %>% filter(Pclass==1, Survived==1) %>% 
  group_by(Sex) %>% 
  summarize(count=n())

train %>% filter(Pclass==3, Survived==1) %>% 
  group_by(Sex) %>% 
  summarise(count=n())

train %>% filter(Pclass==1, Age>15, Sex=="male") %>% 
  group_by(Survived) %>% 
  summarise(count=n())

train %>% filter(Pclass==1, Age<15, Sex=="male") %>% 
  group_by(Survived) %>% 
  summarise(count=n())

ggplot(train, aes(factor(Survived),Age))+geom_boxplot()

ggplot(train, aes(x=Age)) + geom_histogram(aes(fill = factor(Survived)))

train$Survived=as.factor(train$Survived)
ggplot(train,aes(Survived,SibSp)) +geom_bar(stat = "identity")

ggplot(train,aes(factor(Pclass),Age,fill=factor(Survived))+geom_bar(stat = "identity")+facet_grid(~Sex)
       
ggplot(train,aes(factor(Pclass),Age,fill=factor(Survived)))+geom_bar(stat="identity")+facet_grid(~Sex)       

ggplot(train,aes(Age,fill=factor(Survived)))+
  geom_histogram(binwidth = 10,position = position_dodge(width = 10))+facet_grid(~Pclass)

m1=glm(Survived~Age+Sex+Pclass,family="binomial",train)
p1=predict(m1,train,type = "response")
p2=ifelse(p1>0.5,1,0)
table(train$Survived,p2)



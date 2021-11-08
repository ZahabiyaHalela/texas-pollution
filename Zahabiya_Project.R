getwd()
setwd("C:/Users/zrjha/Desktop")
tp <-read.csv("texas_pollution.csv")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

library("dplyr")
library("ggplot2")
library("tidyr")

dim(tp)
str(tp)
levels(tp$product.description)
names(tp)

#CLEANING DATA 

tp2 <- unite(tp, factorysite, company, site, sep = ",")
tp2
head(tp2)

tp3 <- gather(tp2, "Emissions", "TPY", c(co,voc, pm10, pm2.5, nox, so2))
tp3
tp4<-tp3%>%
  arrange(product.description)

str(tp4)
dim(tp4)

write.csv(tp4, "texas_pollutionC.csv")
tp4$Emissions<-as.factor(tp4$Emissions)
str(tp4)
levels(tp4$Emissions)

any(is.na(tp4))
sum(is.na(tp4))

#ANALYSIS

ggplot(tp4, aes(x=product.description, y=TPY, fill = Emissions)) +
  geom_bar(stat="identity")----- #GRAPH1
  
#shows that crude releases highest amount of pollutant with Voc being a major contributor
  
ggplot(tp4, aes(x=(product.description), y=TPY)) + stat_summary(fun.y="mean", geom="point" , color="purple")
#Graph2
#outliers
tp4%>%
  filter(TPY > 00.000) %>% 
  select(Emissions,county,TPY,product.description) %>% 
  group_by(product.description) %>%
  summarise(mean=mean(TPY))%>%
  arrange(desc(mean))

tp4%>%
  filter(TPY > 00.000) %>% 
  select(Emissions,county,TPY,product.description) %>% 
  group_by(Emissions) %>%
  summarise(mean=mean(TPY))%>%
  arrange(desc(mean))
#synthetic fibres and plastic highest amount of pollutant in the air 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#PLASTIC INDUSTRY

levels(tp4$product.description)
plastic<-filter(tp4,product.description == "PLASTICS MATERIALS AND SYNTHETIC RESINS")

dim(plastic)

plasticA<-filter(tp4,product.description == "PLASTICS MATERIALS AND SYNTHETIC RESINS", Emissions =="co")
dim(plasticA) #companies?

#major contributor to pollution in plastic ?

plastic1<-plastic%>%
  filter(TPY > 00.000) %>% 
  select(Emissions,county,TPY,product.description) %>% 
  group_by(Emissions) %>%
  summarise(mean=mean(TPY))%>%
  arrange(desc(mean))

plastic1 #Graph4
ggplot(plastic, aes(x=(Emissions), y=TPY)) + stat_summary(fun.y="mean", geom="point" , color="blue")

#county affected
plastic2<-plastic%>%
  filter(TPY > 00.000) %>% 
  select(Emissions,county,TPY,product.description) %>% 
  group_by(county) %>%
  summarise(mean=mean(TPY))%>%
  arrange(desc(mean))

plastic2
head(plastic2)

ggplot(plastic, aes(x=(county), y=TPY)) + stat_summary(fun.y="mean", geom="point" , color="green")
#Graph5

H1<-(filter(plastic,Emissions =="voc", TPY >= 250.00))# voc > 200.00 harmfull
H<-filter(plastic,Emissions =="voc")
mean(H$TPY)
median(H$TPY)
head(H1)
sum(H$TPY)


#companies
plastic3<-plastic%>%
  filter(TPY > 00.000) %>% 
  select(Emissions,TPY,factorysite) %>% 
  group_by(factorysite) %>%
  summarise(mean=mean(TPY),var=var(TPY))%>%
  arrange(desc(mean))
plastic3

mean(plastic$PPM)
ggplot(plastic, aes(x=(factorysite), y=TPY)) + stat_summary(fun.y="mean", geom="bar" , color="green")#graph6

#......................................................................................

#crude Industry 
A<-filter(tp4,product.description == "CRUDE PETROLEUM & NATURAL GAS")

dim(A)
group_by(A,county)
mean(A$TPY)
sum(A$TPY)

#graph7, Major contributor 
bplotcrude<- ggplot(A, aes(x="", y=TPY, fill=Emissions))+
  geom_bar(width = 1, stat = "identity")
bplotcrude + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2")


A%>%
  filter(TPY> 00.00) %>% 
  select(Emissions,county,TPY) %>% 
  group_by(county) %>%
  summarise(mean=mean(TPY))%>%
  group_by(county) %>% 
  arrange(desc(mean))

A1<-A%>%
  filter(TPY > 00.000) %>% 
  select(Emissions,county,TPY,product.description) %>% 
  group_by(Emissions) %>%
  summarise(mean=mean(TPY))%>%
  arrange(desc(mean))
A1

A2<-A%>%
  filter(PPM > 00.000) %>% 
  select(Emissions,county,PPM,factorysite) %>% 
  group_by(factorysite) %>%
  summarise(mean=mean(PPM), med=median(PPM))%>%
  arrange(desc(mean))
A2
#LIVE OAK 

A3<-filter(tp4,product.description == "CRUDE PETROLEUM & NATURAL GAS", Emissions == "voc")
dim(A3)
sum(A3$TPY)

#higher VOC 

AA<-filter(tp4,product.description == "CRUDE PETROLEUM & NATURAL GAS", Emissions == "voc",TPY > 66.30)
dim(AA)
head(AA)

A5<-filter(tp4,product.description == "CRUDE PETROLEUM & NATURAL GAS", TPY > 4000)
A5
#outliers
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#industrial gases 

Ind<-filter(tp4,product.description=="INDUSTRIAL GASES")
dim(filter(tp4,product.description=="INDUSTRIAL GASES", Emissions=="co")) #no of firms 

Ind1<-Ind%>%
  filter(TPY> 0.000) %>% 
  select(Emissions,county,TPY) %>% 
  group_by(county) %>%
  summarise(meanC=mean(TPY))%>%
  arrange(desc(meanC))
Ind1

Ind2<-Ind%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite) %>% 
  group_by(factorysite) %>%
  summarise(meanC=mean(TPY))%>%
  arrange(desc(meanC))
Ind2

Ind3<-Ind%>%
  filter(TPY> 0.000) %>% 
  select(Emissions,county,TPY,factorysite) %>% 
  group_by(Emissions) %>%
  summarise(meanC=mean(TPY))%>%
  arrange(desc(meanC))
Ind3

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Ngas<-filter(tp4,product.description=="NATURAL GAS LIQUIDS")
dim(filter(tp4,product.description=="NATURAL GAS LIQUIDS", Emissions=="co"))

Ngas1<-Ngas%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite) %>% 
  group_by(Emissions) %>%
  summarise(meanD=mean(TPY))%>%
  arrange(desc(meanD))

#graph8
ggplot(Ngas, aes(x=(Emissions), y=TPY)) + stat_summary(fun.y="mean", geom="point" , color="blue")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Ngt<-filter(tp4,tp4$product.description=="NATURAL GAS TRANSMISSION")
Ngt
dim(Ngt)
dim(filter(Ngt, Emissions == "co"))

Ngt1<-Ngt%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite) %>% 
  group_by(Emissions) %>%
  summarise(meanD1=mean(TPY))%>%
  arrange(desc(meanD1))

Ngt2<-Ngt%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite) %>% 
  group_by(county) %>%
  summarise(meanD1=mean(TPY))%>%
  arrange(desc(meanD1))

mean(Ngt$TPY)
Ngt2
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Analyse Through Emissions 

CO<-filter(tp4,tp4$Emissions=="co")
CO2<-CO%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite) %>% 
  group_by(county) %>%
  summarise(meanD1=mean(TPY))%>%
  arrange(desc(meanD1))
CO2
CO3<-CO%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite,product.description) %>% 
  group_by(product.description) %>%
  summarise(meanD1=mean(TPY))%>%
  arrange(desc(meanD1))
CO3

mean(CO$TPY)
sum(CO$TPY)

CO4<-filter(CO,TPY>37.00)
dim(CO4)
dim(CO)
CO5<-CO%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite,product.description) %>% 
  group_by(factorysite) %>%
  summarise(sum=sum(TPY))%>%
  arrange(desc(sum))
CO5
head(CO5)
Nox<-filter(tp4,tp4$Emissions=="nox")
Nox1<-Nox%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite,product.description) %>% 
  group_by(product.description) %>%
  summarise(meanD1=mean(TPY))%>%
  arrange(desc(meanD1))
Nox1

Nox2<-Nox%>%
  filter(TPY > 0.000) %>% 
  select(Emissions,county,TPY,factorysite,product.description) %>% 
  group_by(county) %>%
  summarise(meanD1=mean(TPY))%>%
  arrange(desc(meanD1))
Nox2

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

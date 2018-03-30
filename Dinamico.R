rm(list=ls())
library(ggplot2)
library(zoo)
library(xts)
library(dygraphs)
library(quantmod)
library(data.table)

setwd("...")

dataA<- as.data.table(na.omit(getSymbols("AZTECACPO.MX",src="yahoo",auto.assign = F),"extend"))
AZTK<-as.data.frame(dataA)
rm(dataA)

m1=getSymbols("ELEKTRA.MX",src="yahoo",auto.assign = F)
m2=getSymbols("USDMXN=X",src="yahoo",auto.assign = F)

m3=m1/m2

dataE<-as.data.table(na.fill(m3,"extend"))
ELEK<-as.data.frame(dataE)
rm(dataE)

dataMU<-as.data.table(na.fill(m2,"extend"))
MxUs<-as.data.frame(dataMU)
rm(dataMU)


colnames(ELEK)<-c("Date","Open","High","Low","Close","Volume","Adjusted")
colnames(AZTK)<-c("Date","Open","High","Low","Close","Volume","Adjusted")
colnames(MxUs)<-c("Date","Open","High","Low","Close","Volume","Adjusted")

View(ELEK)
View(AZTK)

sapply(AZTK, class)
sapply(ELEK, class)

ELEK$Date <- as.Date(as.character(ELEK$Date))
sapply(ELEK, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d"))))

ELEKC<-ELEK[,c(1,5)]
AZTKC<-AZTK[,c(1,5)]


ggplot(ELEKC,aes(Date,Close))+
  geom_line(aes(color="ELEKC"))+
  geom_line(data = AZTKC,aes(color="AZTKC"))+
  labs(color="Legend")+
  scale_colour_manual("",breaks=c("ELEKC","AZTKC"),
                      values = c("blue","red"))+
  ggtitle("Valor de las acciones al cierre: Elektra y Tv Azteca")+
  theme(plot.title = element_text(lineheight = 0.7,face = "bold"))


ELEK_xts<-xts(ELEK$Close,order.by = ELEK$Date,frequency = 365)
AZTK_xts<-xts(AZTK$Close,order.by = AZTK$Date,frequency = 365)


G_Salinas<-cbind(ELEK_xts,AZTK_xts)

dygraph(G_Salinas,ylab = "Cierre",
        main="Valor de las acciones al cierre: Elektra y Tv Azteca")%>%
  dySeries("..1",label = "ELEKTRA")%>%
  dySeries("..2",label = "Tv Azteca")%>%
  dyOptions(colors = c("red","blue"))%>%
  dyRangeSelector()


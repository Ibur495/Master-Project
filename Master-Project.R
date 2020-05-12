library(knitr)
library(ggplot2)
library(reshape)
library(ggmap)
library(lubridate)
library(stringr)
library(data.table)
library(dplyr)
library(maps)
library(kimisc)
dat1 <-read.csv("1.csv")
dat2 <-read.csv("2.csv")
dat3<- read.csv("3.csv")
dat4<- read.csv("4.csv")
dat1$Date <- as.Date(dat1$adjusted_date,format ="%m/%d/%y")
dat2$Date <- as.Date(dat2$adjusted_date,format ="%m/%d/%y")
dat3$Date <- as.Date(dat3$adjusted_date,format ="%m/%d/%y")
dat4$Date <- as.Date(dat4$adjusted_date,format ="%m/%d/%y")

dat7<-subset(dat1,select=c(latitude,longitude))
dat8<-round(sweep(dat7, 2,3600, "/"),2)
dat9<-data.frame(subset(dat1,select = c(speed,throttle,Date)),dat8)
#dat9<-dat9%>%filter(speed>0)
dat22<-within(dat9, location<- paste(latitude,longitude,sep=''))
dat22$newlocation<-gsub('[-.]','',dat22$location)
dat22<-na.omit(dat22)
dat22$index<- cumsum(c(1, abs(diff(as.numeric(dat22$newlocation)))>0))
dat17<-subset(dat2,select=c(latitude,longitude))
dat18<-round(sweep(dat17, 2,3600, "/"),2)
dat19<-data.frame(subset(dat2,select = c(speed,throttle,Date)),dat18)
#dat19<-dat19%>%filter(speed>0)
dat122<-within(dat19, location<- paste(latitude,longitude,sep=''))
dat122$newlocation<-gsub('[-.]','',dat122$location)
dat122<-na.omit(dat122)
dat122$index<- cumsum(c(1, abs(diff(as.numeric(dat122$newlocation)))>0))
dat27<-subset(dat3,select=c(latitude,longitude))
dat28<-round(sweep(dat27, 2,3600, "/"),2)
dat29<-data.frame(subset(dat3,select = c(speed,throttle,Date)),dat28)
#dat29<-dat29%>%filter(speed>0)
dat222<-within(dat29, location<- paste(latitude,longitude,sep=''))
dat222$newlocation<-gsub('[-.]','',dat222$location)
dat222<-na.omit(dat222)
dat222$index<- cumsum(c(1, abs(diff(as.numeric(dat222$newlocation)))>0))
dat37<-subset(dat4,select=c(latitude,longitude))
dat38<-round(sweep(dat37, 2,3600, "/"),2)
dat39<-data.frame(subset(dat4,select = c(speed,throttle,Date)),dat38)
#dat39<-dat39%>%filter(speed>0)
dat322<-within(dat39, location<- paste(latitude,longitude,sep=''))
dat322$newlocation<-gsub('[-.]','',dat322$location)
dat322<-na.omit(dat322)
dat322$index<- cumsum(c(1, abs(diff(as.numeric(dat322$newlocation)))>0))

ggplot()+geom_line(data =dat22,size=.5, aes(x =1:nrow(dat22), y =speed,color=factor(Date))) +
  geom_line(data =dat122,size=.6, aes(x =1:nrow(dat122), y =speed,color=factor(Date)))+
  geom_line(data =dat222,size=.5, aes(x =1:nrow(dat222), y =speed,color=factor(Date)))+
  geom_line(data =dat322,size=.4, aes(x =1:nrow(dat322), y =speed,color=factor(Date)))


dat22$chunk<- cumsum(c(1, abs(diff(dat22$speed))>1))
dat122$chunk<- cumsum(c(1, abs(diff(dat122$speed))>1))
dat222$chunk<- cumsum(c(1, abs(diff(dat222$speed))>1))
dat322$chunk<- cumsum(c(1, abs(diff(dat322$speed))>1))


ggplot()+geom_line(data =dat22,size=.5, aes(x =1:nrow(dat22), y =speed,color=factor(Date))) +
  geom_line(data =dat122,size=.6, aes(x =1:nrow(dat122), y =speed,color=factor(Date)))+
  geom_line(data =dat222,size=.5, aes(x =1:nrow(dat222), y =speed,color=factor(Date)))+
  geom_line(data =dat322,size=.4, aes(x =1:nrow(dat322), y =speed,color=factor(Date)))+
  facet_wrap(~chunk,scales='free')




dat322$chunk<-cumsum(c(1,abs(diff(dat322$speed)>1)))
dat22$chunk<-cumsum(c(1,abs(diff(dat22$speed)>1)))
dat222$chunk<-cumsum(c(1,abs(diff(dat222$speed)>1)))
library(ggmap)


p<-qmap('Missouri',zoom=7)
#p+geom_line(data=dat22,aes(latitude,longitude,group=index))+
# geom_line(data=dat122,aes(latitude,longitude,group=index))+
#geom_line(data=dat222,aes(latitude,longitude,group=index))+
#geom_line(data=dat322,aes(latitude,longitude,group=index))





#usa_center = as.numeric(geocode("c(MISSOURI),USA"))
#USAMap = ggmap(get_googlemap(center=usa_center, scale=1,zoom =5), extent="normal")
#USAMap

us<-p+geom_path(aes(longitude,latitude), data=dat322,col="red", alpha=1)
us


index2 =  rep(1/table(dat322$index),times=table(dat322$index))
dat322$index2 = cumsum(index2)
plot(speed~index2,data=dat322,type='l')
index2 =  rep(1/table(dat22$index),times=table(dat22$index))
dat22$index2 = cumsum(index2)
lines(speed~index2,data=dat22,col=2)

index2 =  rep(1/table(dat122$index),times=table(dat122$index))
dat122$index2 = cumsum(index2)
lines(speed~index2,data=dat122,col=3)

index2 =  rep(1/table(dat222$index),times=table(dat222$index))
dat222$index2 = cumsum(index2)
lines(speed~index2,data=dat222,col=4)


#plot with different locations
ggplot()+geom_line(data=dat322,aes(index2,speed,color=factor(Date)))+
  geom_line(data=dat222,aes(index2,speed,color=factor(Date)))+
  geom_line(data=dat22,aes(index2,speed,color=factor(Date)))+
  geom_line(data=dat122,aes(index2,speed,color=factor(Date)))

ggplot()+geom_line(data=dat322,aes(index2,speed,color=factor(Date)))
ggplot()+geom_line(data=dat222,aes(index2,speed,color=factor(Date)))
ggplot()+geom_line(data=dat22,aes(index2,speed,color=factor(Date)))
ggplot()+geom_line(data=dat122,aes(index2,speed,color=factor(Date)))




nndat1<-subset(dat322,select=c(speed,throttle,index,index2,Date))
newdata1<-nndat1$speed>46 & nndat1$speed<52
nndat<-data.frame(nndat1,heigest_speed_int=newdata1,speed_int=as.numeric(newdata1))
head(nndat)
nndat$chunk1<-cumsum(c(1,abs(diff(nndat$speed_int))>0))

nndat$local_min<-sapply(1:nrow(nndat), function(x) {
  idx=which(nndat$speed_int==nndat$speed_int[x])
  if(x==idx[1])
    return(TRUE)
  else
    return(FALSE)
})
head(nndat)

nn2<-subset(nndat,select=c(speed,index,index2,Date))
z<-cumsum(c(0,rep(1/table(nn2$index),times=table(nn2$index))>0))
z1<-data.frame(z)
library(qpcR)
newdat<-qpcR:::cbind.na(nn2,z1)
nn<-na.omit(newdat)
nn$csum <- ave(nn$speed, nn$index, FUN=cumsum)
#nn$csum1 <- ave(nn$index, nn$index, FUN=cumsum)

nn$new<-sapply(1:nrow(nn), function(i){
  idx=which(nn$index==nn$index[i])
  if(i==idx[1])
    return(1)
  else
    return(1)
})
head(nn)
nn$csum2 <- ave(nn$new, nn$index, FUN=cumsum)
d8<-nn%>% mutate(newspeed=csum/csum2)
ggplot(d8,aes(z,newspeed))+geom_line()
head(d81)


nndat2<-subset(dat22,select=c(speed,throttle,index,index2,Date))
newdata2<-nndat2$speed>44

nndat1<-data.frame(nndat2,heigest_speed_int=newdata2,speed_int=as.numeric(newdata2))
head(nndat1)
nndat1$chunk1<-cumsum(c(1,abs(diff(nndat1$speed_int))>1))

nndat1$local_min<-sapply(1:nrow(nndat1), function(x) {
  idx=which(nndat1$speed_int==nndat1$speed_int[x])
  if(x==idx[1])
    return(TRUE)
  else
    return(FALSE)
})
head(nndat1)

nn3<-subset(nndat1,select=c(speed,index,index2,Date))
z1<-cumsum(c(0,rep(1/table(nn3$index),times=table(nn3$index))))
z2<-data.frame(z1)
library(qpcR)
newdat1<-qpcR:::cbind.na(nn3,z2)
nn1<-na.omit(newdat1)
nn1$csum <- ave(nn1$speed, nn1$index, FUN=cumsum)
#nn$csum1 <- ave(nn$index, nn$index, FUN=cumsum)

nn1$new<-sapply(1:nrow(nn1), function(i){
  idx=which(nn1$index==nn1$index[i])
  if(i==idx[1])
    return(1)
  else
    return(1)
})
head(nn1)
nn1$csum2 <- ave(nn1$new, nn1$index, FUN=cumsum)
d81<-nn1%>% mutate(newspeed=csum/csum2)
head(d81)
ggplot(d81,aes(z1,newspeed))+geom_line()


head(d82)
nndat3<-subset(dat222,select=c(speed,throttle,index,index2,Date))
newdata3<-nndat3$speed>=46
nndat2<-data.frame(nndat3,high_speed_int=newdata3,speed_int=as.numeric(newdata3))
head(nndat2)
nndat2$chunk1<-cumsum(c(1,abs(diff(nndat2$speed_int))>0))

nndat2$local_min<-sapply(1:nrow(nndat2), function(x) {
   nndat2$speed[x]==min(nndat2$speed[nndat2$chunk1==nndat2$chunk1[x]]) & !nndat2$high_speed_int[x]
 })
nndat2$chunk2 = nndat2$chunk1 + nndat2$local_min*0.5
for (i in 2:nrow(nndat2)) {
  if (nndat2$chunk2[i]<nndat2$chunk2[i-1])
    nndat2$chunk2[i] = nndat2$chunk2[i-1]
}

nndat2$chunk2 = as.integer(as.factor(as.character(nndat2$chunk2)))
head(nndat2)
qplot(data=nndat2,index2,speed,geom="point",color=factor(chunk2))

nn4<-subset(nndat2,select=c(speed,index,index2,Date))
z3<-cumsum(c(0,rep(1/table(nn3$index),times=table(nn3$index))))
z4<-data.frame(z3)
library(qpcR)
newdat2<-qpcR:::cbind.na(nn4,z4)
nn7<-na.omit(newdat2)
nn7$csum <- ave(nn7$speed, nn7$index, FUN=cumsum)
#nn$csum1 <- ave(nn$index, nn$index, FUN=cumsum)
nn7
nn7$new<-sapply(1:nrow(nn7), function(i){
  idx=which(nn7$index==nn7$index[i])
  if(i==idx[1])
    return(1)
  else
    return(1)
})
head(nn7)
nn7$csum2 <- ave(nn7$new, nn7$index, FUN=cumsum)
d82<-nn7%>% mutate(newspeed=csum/csum2)
head(d82)
ggplot(d82,aes(z3,newspeed))+geom_line()






nndat4<-subset(dat122,select=c(speed,throttle,index,index2,Date))
newdata4<-nndat4$speed>44 & nndat4$speed<51
nndat3<-data.frame(nndat4,heigest_speed_int=newdata4,speed_int=as.numeric(newdata4))
head(nndat3)
nndat3$chunk1<-cumsum(c(1,abs(diff(nndat3$speed_int))))

nndat3$local_min<-sapply(1:nrow(nndat3), function(x) {
  idx=which(nndat3$speed_int==nndat3$speed_int[x])
  if(x==idx[1])
    return(TRUE)
  else
    return(FALSE)
})
head(nndat3)

nn5<-subset(nndat3,select=c(speed,index,index2,Date))
z4<-cumsum(c(0,rep(1/table(nn5$index),times=table(nn5$index))))
z5<-data.frame(z4)
library(qpcR)
newdat3<-qpcR:::cbind.na(nn5,z5)
nn8<-na.omit(newdat3)
nn8$csum <- ave(nn8$speed, nn8$index, FUN=cumsum)
#nn$csum1 <- ave(nn$index, nn$index, FUN=cumsum)

nn8$new<-sapply(1:nrow(nn8), function(i){
  idx=which(nn8$index==nn8$index[i])
  if(i==idx[1])
    return(1)
  else
    return(1)
})
head(nn8)
nn8$csum2 <- ave(nn8$new, nn8$index, FUN=cumsum)
d83<-nn8%>% mutate(newspeed=csum/csum2)
head(d83)
ggplot(d83,aes(z4,newspeed))+geom_line()

ggplot()+geom_line(data=d8,aes(z,newspeed))+
  geom_line(data=d81,aes(z1,newspeed))+
  geom_line(data=d82,aes(z2,newspeed))+
  geom_line(data=d83,aes(z4,newspeed))+facet_wrap(~Date,scales='free')+ggtitle('Time over different location')+
  xlab('time')




ggplot()+geom_line(data=dat322,aes(index2,speed,color=factor(Date)))+
  geom_line(data=dat222,aes(index2,speed,color=factor(Date)))+
  geom_line(data=dat22,aes(index2,speed,color=factor(Date)))+
  geom_line(data=dat122,aes(index2,speed,color=factor(Date)))+facet_wrap(~Date,scales='free')


int <- seq(0, max(d83$z4)+1, by =1) 
d1<-tapply(d83$newspeed, cut(d83$z4, int),mean)
d2<-data.frame(d1)
ggplot(d2,aes(1:nrow(d2),d1))+geom_line()

int <- seq(0, max(d8$z)+10, by =10) 
d1<-tapply(d8$newspeed, cut(d8$z, int),mean)
d3<-data.frame(d1)
ggplot(d3,aes(1:nrow(d3),d1))+geom_line()

int <- seq(0, max(d83$z4)+10, by =10) 
d1<-tapply(d83$newspeed, cut(d83$z4, int),mean)
d4<-data.frame(d1)
ggplot(d4,aes(1:nrow(d4),d1))+geom_line()
int <- seq(0, max(d81$z1)+10, by =10) 
d1<-tapply(d81$newspeed, cut(d81$z1, int),mean)
d5<-data.frame(d1)
ggplot(d5,aes(1:nrow(d5),d1))+geom_line()



ggplot()+geom_line(data=d2,aes(1:nrow(d2),d1))
+geom_line(data=d3,aes(1:nrow(d3),d1))+
  geom_line(data=d4,aes(1:nrow(d4),d1))+
  geom_line(data=d5,aes(1:nrow(d5),d1))
 



head(nndat)
head(nndat1)
head(nndat2)
head(nndat3)
dd1<-nndat%>%filter(speed>46)
dd2<-nndat1%>%filter(speed>43)
dd3<-nndat2%>%filter(speed>44)
dd4<-nndat3%>%filter(speed>44)

ggplot()+geom_line(data=dd1,aes(index2,speed))+geom_line(data=dd2,aes(index2,speed))+geom_line(data=dd3,aes(index2,speed))+
  geom_line(data=dd4,aes(index2,speed))+facet_wrap(~Date,scales='free')

d8<-subset(d8,select=c(speed,index,index2,Date,z,newspeed))
d81<-subset(d81,select=c(speed,index,index2,Date,z1,newspeed))
d82<-subset(d82,select=c(speed,index,index2,Date,z3,newspeed))
d83<-subset(d83,select=c(speed,index,index2,Date,z4,newspeed))

head(d8)
head(d81)
head(d82)
head(d83)
names(d8)

head(newdata)
head(newdata1)
head(newdata2)
head(newdata3)

head(dat322)

w=3
dat322$smoothSpeed = sapply(dat322$index2,function(a){mean(dat322$speed[dat322$index2<ceiling(a)+w & dat322$index2>floor(a)-w])})
plot(dat322$index2,dat322$smoothSpeed,type='l',main=paste("window width",w))

head(dat322)


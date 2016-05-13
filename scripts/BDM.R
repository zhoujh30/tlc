dat<- read.table('/Users/Yifan/Desktop/output.txt',sep = ',', header = F)

names(dat)[1]<-paste("day")
names(dat)[2]<-paste("hour")
names(dat)[3]<-paste("site")
names(dat)[4]<-paste("total")

write.csv(dat, file = "dat.csv")


for (i in 1:168){
  if (dat$day[i] == 3){
    dat$total[i] <- (dat$total[i]/53)*52
    }else{
      dat$total[i] <- dat$total[i]
    }
}

unique(dat$site)

sit <- unique(dat$site)


vect <- matrix(1:40656, ncol = 242)
vect <- as.data.frame(vect)

a<-c()
for (i in 1:242){
  a <- subset(dat, dat$site == sit[i])
  a <- a[order(a$day,a$hour),]
  vect[,i] <- a[4]/52
}

for (i in 1:242){
  names(vect)[i]<-paste(site[i])
}

write.csv(vect, file = "BDM.csv")

a <- as.matrix(vect)
b <- t(a)
c <- as.data.frame(b)
c[1]

barplot(c[1,])

row.names(vect) <- 1:168
colnames(c)<-1:168

write.csv(c, file = "BDM1.csv")


for (i in 100:262){
  if (summary(dat$site)[i] ==167){
    print (i)
  }else {
    print ("")
  }
}

a <- subset(dat, table(dat$site)==168)
a <- table(dat$site)==168
b <- subset(dat, dat$site ==names(a))


for (i in 1:length(table(dat$site))){
  if (table(dat$site)[[i]] == 168) {
    print(table(dat$site)[i])
  } 
}


for (i in 1:length(table(dat$site))){
  print(i)
}


a <-as.data.frame(table(dat$site))
names(a)[1]<-paste("site")
b <- dat
c <-merge(x = b, y = a, by = "site", all = TRUE)
d <- subset(c, Freq == 168)

#############################################################################
da <- d

for (i in 1:168){
  if (da$day[i] == 3){
    da$total[i] <- (da$total[i]/53)*52
  }else{
    da$total[i] <- da$total[i]
  }
}

vect <- matrix(1:19824, ncol = 118)
vect <- as.data.frame(vect)

si <- unique(da$site)

for (i in 1:118){
  a <- subset(da, da$site == si[i])
  a <- a[order(a$day,a$hour),]
  vect[,i] <- a[4]/52
}

#site <- unique(da$site)

for (i in 1:118){
  names(vect)[i]<-paste(site[i])
}

aa <- as.data.frame(t(as.matrix(vect)))
colnames(aa)<-1:168
write.csv(aa, file = "final.csv")

vect$Num <- 1:168
names(vect)[119]<-paste("Num")

write.csv(vect, file = "graph.csv")


plot(vect$Astoria,type = 'l', lwd = 1,col = 1, ylim = c(0,6300), xlim = c(0,168))
for (i in 1:118){
  lines(spline(vect[,i]),col=i,lwd = 1)
}
legend("topright", legend(vect), col = 1:118)

library(ggplot2)

supp1=c("OJ","OJ","OJ","VC","VC","VC")
dose1=c(0.5,1.0,2.0,0.5,1.0,2.0)
length1=c(13.23,22.70,26.06,7.98,16.77,26.14)
tgg=data.frame(supp1,dose1,length1)
ggplot(vect, aes(x=factor(dose1), y=length1, colour=colnames(vect),group=colnames(vect))) + geom_line(size=2)
ggplot(vect$Astoria)

time.strptime(2010-01-01, "%Y%m%d")

final <- read.csv('/Users/Yifan/Desktop/final2.csv')
fi <-t(as.matrix(final))
fi <- as.data.frame(fi,header = T)
final <- as.data.frame(t(as.matrix(final)), header = T)

write.csv(fi, file = "fi.csv")

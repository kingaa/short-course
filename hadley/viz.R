options(
  keep.source=TRUE,
  encoding="UTF-8"
  )

set.seed(594709947L)
require(ggplot2)
theme_set(theme_bw())
dat <- read.csv("http://kinglab.eeb.lsa.umich.edu/202/data/mosquitoes.csv")
wt <- subset(dat,type=="wildtype",select=lifespan)
tg <- subset(dat,type=="transgenic",select=-type)
mean(wt$lifespan)
mean(tg$lifespan)
plot(dat)
op <- par(mfrow=c(1,2))
hist(tg$lifespan,breaks=seq(0,55,by=5),ylim=c(0,40))
hist(wt$lifespan,breaks=seq(0,55,by=5),ylim=c(0,40))
par(op)
plot(sort(dat$lifespan),seq(1,nrow(dat))/nrow(dat),type='n')
lines(sort(wt$lifespan),seq(1,nrow(wt))/nrow(wt),type='s',col='blue')
lines(sort(tg$lifespan),seq(1,nrow(tg))/nrow(tg),type='s',col='red')
library(MASS)
plot(mammals)
plot(mammals,log='x')
plot(mammals,log='xy')
plot(mammals$body,mammals$brain,log='xy')
plot(brain~body,data=mammals,log='xy')
oil <- read.csv("http://kinglab.eeb.lsa.umich.edu/202/data/oil_production.csv",comment.char="#")
plot(oil)
plot(Gbbl~year,data=oil,subset=region=="North.America",type='l')
lines(Gbbl~year,data=oil,subset=region=="Eurasia",type="l",col='red')

require(reshape2)
dcast(oil,year~region) -> wideOil
names(wideOil)
wideOil$total <- wideOil$Africa+wideOil$Asia+wideOil$Central+wideOil$Eurasia+wideOil$Europe+wideOil$Middle+wideOil$North.America
wideOil$total <- apply(wideOil[,-1],1,sum)
plot(wideOil$year,wideOil$total,type='l')
energy <- read.csv("http://kinglab.eeb.lsa.umich.edu/202/data/energy_production.csv",comment.char="#")

require(ggplot2)

ggplot(data=energy,mapping=aes(x=year,y=TJ,color=region,linetype=source))+geom_line()
ggplot(data=energy,mapping=aes(x=year,y=TJ,color=region))+geom_line()+facet_wrap(~source)
ggplot(data=energy,mapping=aes(x=year,y=TJ,color=source))+geom_line()+facet_wrap(~region)
ggplot(data=energy,mapping=aes(x=year,y=TJ))+geom_line()
ggplot(data=energy,mapping=aes(x=year,y=TJ,group=source))+geom_line()
ggplot(data=energy,mapping=aes(x=year,y=TJ,group=source:region))+geom_line()
require(reshape2)

tot <- dcast(energy,year+source~'TJ',value.var="TJ",fun.aggregate=sum)
ggplot(data=tot,mapping=aes(x=year,y=TJ,color=source))+geom_line()
ggplot(data=tot,mapping=aes(x=year,y=TJ,fill=source))+geom_area()

reg <- dcast(energy,region+source~'TJ',value.var="TJ",fun.aggregate=mean)
ggplot(data=reg,mapping=aes(x=region,y=TJ,fill=source))+
   geom_bar(stat="identity")+coord_flip()
require(plyr)

ddply(energy,~region+source,summarize,TJ=mean(TJ)) -> x

ggplot(data=x,mapping=aes(x=region,y=TJ,fill=source))+
   geom_bar(stat="identity")+coord_flip()

ddply(x,~region,mutate,frac=TJ/sum(TJ)) -> y

ggplot(data=y,mapping=aes(x=region,y=frac,fill=source))+
   geom_bar(stat="identity")+coord_flip()+labs(x="fraction of production")

require(plyr)

mutate(energy,
       source=as.character(source),
       source1=mapvalues(source,
                         from=c("Hydro","Other Renewables","Coal","Oil","Gas"),
                         to=c("Renewable","Renewable","Carbon","Carbon","Carbon"))
       ) -> energy

ddply(energy,~source1+region+year,summarize,TJ=sum(TJ)) -> x

ggplot(data=x,mapping=aes(x=year,y=TJ,fill=source1))+
    geom_area()+
    facet_wrap(~region,scales="free_y",ncol=2)

ddply(energy,~source1+year,summarize,TJ=sum(TJ)) -> x

ggplot(data=x,mapping=aes(x=year,y=TJ,fill=source1))+
    geom_area()

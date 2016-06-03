list.files("./GainCapitalData/2016/1",pattern=".*\\.csv")

filename <- "AUD_USD_Week2.csv"

data <- read.table(paste(getwd(),filepath,filename,sep="/"), header=TRUE, sep=",")

z<-as.POSIXct(strptime(as.character(dataset$RateDateTime[1]),"%Y-%m-%d %H:%M:%OS"))
op <- options(digits.secs=3)
z
options(op)


t<- "./GainCapitalData/2016/2/AUD_USD_Week1.csv"

grep("2016/*/",t)




library(manipulate)
library(dplyr)

# load csv files

#setwd("/Users/Dexter/Documents/R")

setwd("C:/D/R/Forex")

rm(list=ls())

filepath <- "./GainCapitalData/2016/"

months <- c(1:5)

files <- c()

for(m in months)
{
	f <- list.files(paste(filepath,m,sep=""),pattern=".*\\.csv")
	f <- paste(filepath,m,"/",f,sep="")
	files <- c(files,f)
}


for(f in files)
{
	if(!exists("dataset")){
		dataset <- read.table(f, header=TRUE, sep=",")
		dataset$filename <- f
	}
	else{
		d <- read.table(f, header=TRUE, sep=",")
		d$filename <- f
		dataset <- rbind(dataset,d)
	}


}



# data manipulation

dataset$DateTime <- as.POSIXct(strptime(as.character(dataset$RateDateTime),"%Y-%m-%d %H:%M:%OS"))


dataset$filename <- gsub("./GainCapitalData/|\\.csv|AUD_USD_","",dataset$filename)

op <- options(digits.secs=3)

dataset <- dataset[,-c(2:4)]

# data exploring
# basic checking

dataset %>% group_by(filename) %>% summarise(count=n(),startTime=min(DateTime),endTime=max(DateTime),startId=min(lTid),endId=max(lTid))%>% mutate(startDay=weekdays(startTime),endDay=weekdays(endTime))



library(ggplot2)

unique(round(dataset$RateBid,4)) # 1008

#tmp_dataset <- head(dataset,10000)

tmp_dataset <- data.frame(bid=round(dataset$RateBid,4))

g<- ggplot(tmp_dataset,aes(bid))+geom_histogram(bins=1008)+geom_vline(xintercept=0.74)+annotate("text", x = 0.78, y = 18000, label = "Some text")+coord_flip()
g

manipulate(
ggplot(tmp_dataset,aes(bid))+geom_histogram(bins=1008)+geom_vline(xintercept=x.value)+annotate("text", x = 0.78, y = 18000, label = x.value)+coord_flip()
,x.value=slider(0.68,0.79))

dim(subset(dataset, DateTime >"2016-04-20"))
  

tmp_dataset_l <- data.frame(bid=round(subset(dataset, DateTime <="2016-04-20")$RateBid,4))
bins <- length(unique(tmp_dataset_l$bid))
g<- ggplot(tmp_dataset_l,aes(bid))+geom_histogram(bins=bins)+coord_flip()
g

tmp_dataset_r <- data.frame(bid=round(subset(dataset, DateTime >"2016-04-20")$RateBid,4))
bins <- length(unique(tmp_dataset_r$bid))
g<- ggplot(tmp_dataset_r,aes(bid))+geom_histogram(bins=bins)+coord_flip()
g

frequency_l<-tmp_dataset_l %>% group_by(bid) %>% summarise(frequency=n())
frequency_r<-tmp_dataset_r %>% group_by(bid) %>% summarise(frequency=n())

range(frequency_l$bid)
range(frequency_r$bid)

frequency_comb <- merge(frequency_l,frequency_r,by="bid")
range(frequency_comb$bid)

head(frequency_comb)

cor(frequency_comb$frequency.x,frequency_comb$frequency.y)

install.packages(Hmisc)
# Correlations with significance levels
library(Hmisc)
rcorr(x, type="pearson") # type can be pearson or spearman

#mtcars is a data frame 
rcorr(as.matrix(mtcars))

rcorr(as.matrix(frequency_comb))

ggplot(frequency_comb,aes(x=bid))+geom_line(aes(y=frequency.x,color="rise"))+geom_line(aes(y=frequency.y,color="fall"))

dim(tmp_dataset_l)
dim(tmp_dataset_r)

dataset<-dataset[order(DateTime),]

dataset$RateBidPre <- lag(dataset$RateBid,1)
dataset$RateAskPre <- lag(dataset$RateAsk,1)
dataset$diffBid <- with(dataset,RateBid-RateBidPre)
dataset$diffAsk <- with(dataset,RateAsk-RateAskPre)

dataset$diffBid_tr <- with(dataset,(RateBid-RateBidPre)/abs(RateBid-RateBidPre))
dataset$diffAsk_tr <- with(dataset,(RateAsk-RateAskPre)/abs(RateAsk-RateAskPre))
dataset[is.nan(dataset$diffBid_tr),"diffBid_tr"]<-0
dataset[is.nan(dataset$diffAsk_tr),"diffAsk_tr"]<-0
head(dataset)


install.packages("stats")
library(stats)
#moving sum of last n values
movsum <- function(x,n=25){filter(x,rep(1,n), sides=1)}

dataset$Bid_movsum_60 <- movsum(dataset$diffBid_tr,60)
dataset$Ask_movsum_60 <- movsum(dataset$diffAsk_tr,60)

dataset$Bid_movsum_300 <- movsum(dataset$diffBid_tr,300)
dataset$Ask_movsum_300 <- movsum(dataset$diffAsk_tr,300)


dataset$Bid_movsum_3600 <- movsum(dataset$diffBid_tr,3600)
dataset$Ask_movsum_3600 <- movsum(dataset$diffAsk_tr,3600)

dataset$Bid_movsum_4h <- movsum(dataset$diffBid_tr,3600*4)
dataset$Ask_movsum_4h <- movsum(dataset$diffAsk_tr,3600*4)

head(dataset,100)

hist(dataset$Bid_movsum_60)
hist(dataset$Bid_movsum_300)
hist(dataset$Bid_movsum_3600)
hist(dataset$Bid_movsum_4h)


head(dataset)
tmp_data <- dataset[dataset$DateTime >= "2016-05-15" & dataset$DateTime < "2016-05-20",] 
dim(tmp_data)
library(ggplot2)
ggplot(tmp_data)+geom_line(aes(x=DateTime,y=Bid_movsum_4h))+geom_line(aes(x=DateTime,y=RateBid))




install.packages("plotrix")
library(plotrix)
str(tmp_data)

range(tmp_data$DateTime)
with(tmp_data,twoord.plot(lx=DateTime,rx=DateTime,ly=RateBid,ry=Bid_movsum_4h,type="l"))
text(x = tmp_data$DateTime,  labels = paste(tmp_data$DateTime,' '), srt = 45, pos = 1, xpd = TRUE,cex=.7)

with(tmp_data,twoord.plot(lx=DateTime,rx=DateTime,ly=Bid_movsum_3600,ry=Bid_movsum_4h,type="l"))

cor(tmp_data$Bid_movsum_4h,tmp_data$RateBid)


## set up some fake test data
time <- seq(0,72,12)
betagal.abs <- c(0.05,0.18,0.25,0.31,0.32,0.34,0.35)
cell.density <- c(0,1000,2000,3000,4000,5000,6000)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(time, betagal.abs, pch=16, axes=FALSE, ylim=c(0,1), xlab="", ylab="", 
     type="b",col="black", main="Mike's test data")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Beta Gal Absorbance",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(time, cell.density, pch=15,  xlab="", ylab="", ylim=c(0,7000), 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Cell Density",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(time),10))
mtext("Time (Hours)",side=1,col="black",line=2.5)  

## Add Legend
legend("topleft",legend=c("Beta Gal","Cell Density"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))





## set up some fake test data
time <- seq(0,72,12)
betagal.abs <- c(0.05,0.18,0.25,0.31,0.32,0.34,0.35)
cell.density <- c(0,1000,2000,3000,4000,5000,6000)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
with(tmp_data,plot(DateTime, RateBid, pch=16, axes=FALSE, ylim=c(0.7,0.8), xlab="", ylab="", 
     type="b",col="black", main="Price vs momentum"))
axis(2, ylim=c(0.7,0.8),col="black",las=1)  ## las=1 makes horizontal labels
mtext("price",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(time, cell.density, pch=15,  xlab="", ylab="", ylim=c(0,7000), 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Cell Density",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(time),10))
mtext("Time (Hours)",side=1,col="black",line=2.5)  

## Add Legend
legend("topleft",legend=c("Beta Gal","Cell Density"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))





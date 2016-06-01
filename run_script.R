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

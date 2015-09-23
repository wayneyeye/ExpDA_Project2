#(list=ls())
#####################################
#plot1
##

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Calculating sums for pm2.5 data
ttl_usa<-tapply(NEI$Emissions,INDEX=as.factor(NEI$year),sum,na.rm=TRUE)
##
png(file="plot1.png",width=480,height=480)
plot(ttl_usa~names(ttl_usa),ann = F,pch=6,col="black",xaxt="n")
title(main = " Total PM2.5 emission from all sources in USA",
      xlab = "Year",ylab = "Tons of PM2.5 emitted")
axis(side=1,at=names(ttl_usa))
dev.off()

#####################################
#plot2
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Subseting data for Baltimore city
baltimore_data<-subset(NEI,NEI$fips=="24510")
##Calculating sums for pm2.5 data
ttl_baltimore<-tapply(baltimore_data$Emissions,INDEX=as.factor(baltimore_data$year),sum,na.rm=TRUE)
##
png(file="plot2.png",width=480,height=480)
plot(ttl_baltimore~names(ttl_usa),ann = F,pch=6,col="blue",xaxt="n")
title(main = " Total PM2.5 emission from all sources in Baltimore",
      xlab = "Year",ylab = "Tons of PM2.5 emitted")
axis(side=1,at=names(ttl_usa))
dev.off()

#####################################
#plot3
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
###
##Subseting data for Baltimore city
baltimore_data<-subset(NEI,NEI$fips=="24510")
###
library(dplyr)
grouped_baltimore<-group_by(baltimore_data,type,year)
baltimore_sum<-summarize(grouped_baltimore,total=sum(Emissions))
baltimore_sum<-data.frame(baltimore_sum)
###
library(ggplot2)
g_1<-ggplot(baltimore_sum,aes(year,total))
(g_1+ geom_smooth(methods="loess",aes(col=type),alfa=0.5)+geom_point(aes(col=type))
        +labs(x="Year",y="Tons of PM2.5 emitted")
        +ggtitle("Trends for different type of PM2.5 emissions")
)
ggsave(file="plot3.png")

#####################################
#plot4
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
###
###
library(dplyr)
SCC_EI<-select(SCC,SCC,EI.Sector)
cc_SCC_EI<-SCC_EI[grepl("- Coal",SCC_EI$EI.Sector),]
cc_SCC<-unique(cc_SCC_EI$SCC)
cc_us<-filter(NEI,SCC %in% cc_SCC)
##plot
ttl_usa_cc<-tapply(cc_us$Emissions,INDEX=as.factor(cc_us$year),sum,na.rm=TRUE)
##
png(file="plot4.png",width=480,height=480)
plot(ttl_usa_cc~names(ttl_usa_cc),ann = F,pch=1,col="black",xaxt="n")
title(main = " Total PM2.5 emission from coal combustion in USA",
      xlab = "Year",ylab = "Tons of PM2.5 emitted")
axis(side=1,at=names(ttl_usa_cc))
dev.off()
######################################
#plot5
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
###
##Subseting data for Baltimore city
baltimore_data<-subset(NEI,NEI$fips=="24510")
###
library(dplyr)
#Select from On-Road Vehicles
mv_baltimore<-filter(baltimore_data,type=="ON-ROAD")
####plot
ttl_baltimore_mv<-tapply(mv_baltimore$Emissions,INDEX=as.factor(mv_baltimore$year),sum,na.rm=TRUE)
##
png(file="plot5.png",width=480,height=480)
plot(ttl_baltimore_mv~names(ttl_baltimore_mv),ann = F,pch=2,col="black",xaxt="n")
title(main = " Total PM2.5 emission from Motor Vehicles in Baltimore",
      xlab = "Year",ylab = "Tons of PM2.5 emitted")
axis(side=1,at=names(ttl_baltimore_mv))
dev.off()
######################################
#plot6
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
###
##Subseting data for Baltimore city
bal_la_data<-subset(NEI,NEI$fips %in% c("24510","06037"))
###
library(dplyr)
#Select from On-Road Vehicles
mv_bal_la<-filter(bal_la_data,type=="ON-ROAD")
####Calculation
ttl_bal_la_mv<-tapply(mv_bal_la$Emissions,INDEX=as.factor(mv_bal_la$year),sum,na.rm=TRUE)
grouped_bal_la<-group_by(bal_la_data,fips,year)
bal_la_sum<-summarize(grouped_bal_la,total=sum(Emissions))
bal_la_sum<-data.frame(bal_la_sum)
####GGplot
###
library(ggplot2)
g_2<-ggplot(bal_la_sum,aes(year,total))
(g_2+ geom_smooth(methods="loess",aes(col=fips),size=1.5)+geom_point(aes(col=fips),size=5)
+labs(x="Year",y="Tons of PM2.5 emitted")
+ggtitle("Trends of PM2.5 emissions from Baltimore and Los Angeles's on-road vehicles")
+scale_color_manual(values=c("forestgreen","tomato"),labels=c("Los Angeles","Baltimore"))
)
ggsave(file="plot6.png")
###Reduction in PM2.5 emission
#Reduction in LA
bal_la_sum[1,3]-bal_la_sum[4,3]
#Reduction in Baltimore
bal_la_sum[5,3]-bal_la_sum[8,3]



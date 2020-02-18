source("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/UCC Dashboard_Input.R")
library(tinytex)
library(anytime)
library(lubridate)
library(ggplot2)
library(reshape2)
Location <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York")
Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
#Create DOW data frames for the entire repository
DOW <- DOW_Volume()
#The most recent week of data
DOW_Weekly <- DOW_Volume(Date=max(anytime(data$Arrival), na.rm = T)-604800)
#Fiscal year to date
DOW_FYTD <- DOW_Volume(Date = floor_date(Sys.Date(),"year"))
#Past 30 days
DOW_30day <- DOW_Volume(Date=max(anytime(data$Arrival), na.rm = T)-2952000)

#Create TOD data frames for the entire repository
TOD <- TOD_Volume()
#The most recent week of data
TOD_Weekly <- TOD_Volume(Date=max(anytime(data$Arrival), na.rm = T)-604800)
#Fiscal year to date
TOD_FYTD <- TOD_Volume(Date = floor_date(Sys.Date(),"year"))
#Past 30 days
TOD_30day <- TOD_Volume(Date=max(anytime(data$Arrival), na.rm = T)-2952000)



#grouped bar graph for all DOW data
bar_DOW <- function(DOW_table){
  one <- ggplot(data=melt(data = DOW_table[c(1:6,8),], id.var="Location"), aes(fill=Location, x=variable,y=value)) +
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle("Volume by Day of Week")+
    xlab("Day of Week")+
    ylab("Volume")+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(one)
}
#bar_DOW(DOW_30day)
#bar_DOW(DOW_FYTD)
#bar_DOW(DOW)
#bar_DOW(DOW_Weekly)

#line graph for all TOD data
line_TOD <- function(TOD_table){
  threemelt <- melt(data = TOD_table[c(1:6,7),], id.vars="Location")
  threemelt$variable <- as.numeric(threemelt$variable)
  three <- ggplot(data=threemelt, aes(x=variable,y=value,group=Location,color=Location))+
    geom_line(size=1.5)+
    geom_point(size=2.75, colour = "black")+
    scale_x_continuous(breaks=c(6:23), limits=c(6,23))+
    ggtitle("Volume by Hour of Day")+
    xlab("Hour of Day")+
    ylab("Volume")+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(three)
}
#line_TOD(TOD_30day)
#line_TOD(TOD_Weekly)

#Bar graph by DOW for FYTD, 30 day, weekly and 30 day system
bar_DOW_site <- function(loc){
  df <- rbind(DOW_FYTD[DOW_FYTD$Location == loc,],DOW_30day[DOW_30day$Location == loc,],DOW_Weekly[DOW_Weekly$Location == loc,],DOW_30day[DOW_30day$Location == "Median",])
  df[,1] <- c("FYTD", "30 Day","7 Day", "System Median")
  df[1,2:8] <- df[1,2:8]/as.numeric(strftime(max(as.POSIXct(data$Arrival[!(is.na(data$Arrival))])), format = "%j"))
  df[c(2,4),2:8] <- df[c(2,4),2:8]/30
  df[3,2:8] <- df[3,2:8]/7
  dfmelt <-  melt(data=df, id.vars="Location")
  df$Location <- factor(df$Location, levels=c("FYTD","30 Day", "7 Day", "System Median"))
  five <- ggplot(data = melt(data=df, id.vas="Location"),aes(fill=Location, x=variable,y=value))+
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle(paste0(loc," Volume by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Daily)")+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(five)
}
#bar_DOW_site("MS Express Care")

box_DOW_site <- function(loc){
  data$Arrival <- anytime(data$Arrival)
  data <- cbind(data,as.Date(data$Arrival,format = "%m/%d/%Y"))
  df1 <- data[data$Location == loc,]
  df1 <- df1[!is.na(df1$Location) & !is.na(df1$`Day of Week`),]
  df2 <- data[data$Location != loc,]
  df2 <- df2[!is.na(df2$Location) & !is.na(df2$`Day of Week`),]
  df2$Location <- "System"
  data1 <- rbind(df1,df2)
  daily <- unique(data1$`as.Date(data$Arrival, format = "%m/%d/%Y")`)
  DailyVol <- as.data.frame(matrix(0,nrow=2,ncol = length(daily)))
  for(i in 1:length(daily)){
    DailyVol[1,i] <- nrow(data1[data1$Location == loc & data1$`as.Date(data$Arrival, format = "%m/%d/%Y")` == daily[i],])
  }
  
  
  #datatest <- melt(data = data1, id = c(1,12), measure.vars = 10) 
  
}


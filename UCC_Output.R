source(paste0(getwd(),"/UCC_Input.R"))
library(tinytex)
library(anytime)
library(lubridate)
library(ggplot2)
library(reshape2)
library(anytime)
Location <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York")
Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
#Create DOW data frames for the entire repository
DOW <- DOW_Volume()
#The most recent week of data
DOW_Weekly <- DOW_Volume(Date=max(anytime(data$Arrival), na.rm = T)-604800)
#Fiscal year to date
DOW_FYTD <- DOW_Volume(Date = lubridate::floor_date(Sys.Date(),"year"))
#Past 30 days
DOW_30day <- DOW_Volume(Date=max(anytime(data$Arrival), na.rm = T)-2952000)

#Create TOD data frames for the entire repository
TOD <- TOD_Volume()
#The most recent week of data
TOD_Weekly <- TOD_Volume(Date=max(anytime(data$Arrival), na.rm = T)-604800)
#Fiscal year to date
TOD_FYTD <- TOD_Volume(Date = lubridate::floor_date(Sys.Date(),"year"))
#Past 30 days
TOD_30day <- TOD_Volume(Date=max(anytime(data$Arrival), na.rm = T)-2952000)

#Compliance table for FYTD
Compliance_FYTD <- compliance(Date = floor_date(Sys.Date(),"year"))
#Compliance table for most recent week of data
Compliance_Weekly <- compliance(Date = max(anytime(data$Arrival), na.rm = T)-604800)


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
  dfmelt <- melt(data = TOD_table[c(1:6,7),], id.vars="Location")
  dfmelt$variable <- as.numeric(dfmelt$variable)
  two <- ggplot(data=dfmelt, aes(x=variable,y=value,group=Location,color=Location))+
    geom_line(size=1.5)+
    geom_point(size=2.75, colour = "black")+
    scale_x_continuous(breaks=c(6:23), limits=c(6,23))+
    ggtitle("Volume by Hour of Day")+
    xlab("Hour of Day")+
    ylab("Volume")+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(two)
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
  three <- ggplot(data = melt(data=df, id.vas="Location"),aes(fill=Location, x=variable,y=value))+
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle(paste0(loc," Volume by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Daily)")+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(three)
}
#bar_DOW_site("MS Express Care")

#Box plot by DOW comparing site with entire system
box_DOW_site <- function(loc, Date = "1/1/2000"){
  data$Arrival <- anytime(data$Arrival)
  data <- cbind(data,as.Date(data$Arrival,format = "%m/%d/%Y"))
  data <- data[data$`as.Date(data$Arrival, format = "%m/%d/%Y")`>Date,]
  #all data for site of interest
  df1 <- data[data$Location == loc,]
  df1 <- df1[!is.na(df1$Location) & !is.na(df1$`Day of Week`),]
  #all data for entire system
  df2 <- data
  df2 <- df2[!is.na(df2$Location) & !is.na(df2$`Day of Week`),]
  df2$Location <- "System"
  #combine df1 and df2
  data1 <- rbind(df1,df2)
  #create list of all unique dates
  daily <- unique(data1$`as.Date(data$Arrival, format = "%m/%d/%Y")`)
  #create table with number encounters by uniqe date for site and entire system
  DailyVol <- as.data.frame(matrix(0,nrow=2,ncol = 1+length(daily)))
  colnames(DailyVol) <- c("Location",as.character(daily))
  DailyVol[,1] <- c(loc,"System")
  #calculate number encounters by date for site
  for(i in 1:length(daily)){
    DailyVol[1,i+1] <- nrow(data1[data1$Location == loc & data1$`as.Date(data$Arrival, format = "%m/%d/%Y")` == daily[i],])
  }
  #Calculate number encounters by date for entire system
  for(i in 1:length(daily)){
    DailyVol[2,i+1] <- nrow(data1[data1$Location == "System" & data1$`as.Date(data$Arrival, format = "%m/%d/%Y")` == daily[i],])
  }
  #melt DailyVol so we have 2 rows for every date (1 for site and 1 for entire system)
  dfmelt <- melt(data = DailyVol, id.vars = "Location")
  dfmelt$variable <- weekdays(anytime(dfmelt$variable))
  #Calculate system average for every system encounter value
  dfmelt[dfmelt$Location == "System",][,3] <- (dfmelt[dfmelt$Location == "System",][,3])/length(Location)
  dfmelt$variable <- factor(dfmelt$variable, levels = Days)
  dfmelt$Location <- factor(dfmelt$Location, levels = c(loc,"System"))
  four <- ggplot(data=dfmelt, aes(x=variable, y=value, fill=Location))+
    geom_boxplot()+
    ggtitle(paste0(loc," Encounters by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Encounters)")+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(four)
}

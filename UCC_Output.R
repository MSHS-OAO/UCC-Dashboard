source(paste0(getwd(),"/UCC_Input.R"))
library(tinytex)
library(anytime)
library(lubridate)
library(ggplot2)
library(reshape2)
library(anytime)
library(dplyr)
Location <- c(`Ms Express Care`="MS Express Care",`UC10UNION [41028001]`="UC Union Square",
              `UCBROADWAY [8316001]`="UC Broadway",`UCCADMAN [8315001]`="UC Cadman", 
              `UCCOLUMBUS [8314001]`="UC Columbus",`UCYORK [8317001]`="UC York")
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
  one <- ggplot(data=melt(data = DOW_table[c(1:7),], id.var="Location"), aes(fill=Location, x=variable,y=value)) +
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle("Volume by Day of Week")+
    xlab("Day of Week")+
    ylab("Volume")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
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
    scale_color_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(two)
}
#line_TOD(TOD_30day)
#line_TOD(TOD_Weekly)

#Bar graph by DOW for FYTD, 30 day, weekly and 30 day system
bar_DOW_site <- function(loc){
  df <- rbind(DOW_FYTD[DOW_FYTD$Location == loc,],DOW_30day[DOW_30day$Location == loc,],DOW_Weekly[DOW_Weekly$Location == loc,],DOW_30day[DOW_30day$Location == "Median",])
  df[,1] <- c("FYTD", "30 Day","7 Day", "System Median (30 Day)")
  df[1,2:8] <- df[1,2:8]/(as.numeric(strftime(max(as.POSIXct(data$Arrival[!(is.na(data$Arrival))])), format = "%j"))/7)
  df[c(2,4),2:8] <- df[c(2,4),2:8]/(30/7)
  dfmelt <-  melt(data=df, id.vars="Location")
  colnames(dfmelt) <- c("Timeframe","variable","value")
  df$Location <- factor(df$Location, levels=c("FYTD","30 Day", "7 Day", "System Median (30 Day)"))
  three <- ggplot(data = dfmelt,aes(fill=Timeframe, x=variable,y=value))+
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle(paste0(loc," Volume by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Daily)")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(three)
}
#bar_DOW_site("UC Union Square")

#Box plot by DOW comparing site with entire system
box_DOW_site <- function(loc, Date = "1/1/2000"){
  data$Arrival <- anytime(data$Arrival)
  data <- cbind(data,as.Date(data$Arrival,format = "%m/%d/%Y"))
  data <- data[data$`as.Date(data$Arrival, format = "%m/%d/%Y")`>Date,]
  #all data for site of interest
  df1 <- data[data$Location == loc,]
  df1 <- df1[!is.na(df1$Location) & !is.na(df1$`Day of Week`),]
  df1$Location <- Location[loc]
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
  DailyVol[,1] <- c(Location[loc],"System")
  #calculate number encounters by date for site
  for(i in 1:length(daily)){
    DailyVol[1,i+1] <- nrow(data1[data1$Location == Location[loc] & data1$`as.Date(data$Arrival, format = "%m/%d/%Y")` == daily[i],])
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
  dfmelt$Location <- factor(dfmelt$Location, levels = c(Location[loc],"System"))
  four <- ggplot(data=dfmelt, aes(x=variable, y=value, fill=Location))+
    geom_boxplot()+
    ggtitle(paste0(Location[loc]," Encounters by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Encounters)")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(four)
}

#Line graph by DOW showing volume by time of day
line_DOW_TOD_site <- function(loc, Date = "1/1/2000"){
  data$Arrival <- anytime(data$Arrival)
  #filter master data by location and date
  df1 <- data[data$Location == loc & data$Arrival > anytime(Date),]
  #remove all NAs leading to false row count
  df1 <- df1[!is.na(df1$Location) & !is.na(df1$`Day of Week`) & !is.na(df1$Arrival) & !is.na(df1$`Hour of Day`),]
  #Create data frame counting number of encounters for each DOW at each Hour of day
  TOD_DOW <- as.data.frame(matrix(0,nrow=7,ncol=25))
  colnames(TOD_DOW) <- c("Day of Week",0:23)
  TOD_DOW[,1] <- Days
  for(i in 2:25){
    for(j in 1:7){
      days <- as.numeric(round(difftime(Sys.Date(),min(df1$Arrival),units = "days"),digits = 0))/7
      TOD_DOW[j,i] <- nrow(df1[df1$`Hour of Day` == i-2 & df1$`Day of Week` == Days[j],])/days
    }
  }
  TOD_melt <- melt(data = TOD_DOW, id.vars = "Day of Week")
  TOD_melt$variable <- as.numeric(TOD_melt$variable)
  TOD_melt$`Day of Week` <- factor(TOD_melt$`Day of Week`, levels = c(Days))
  five <- ggplot(data=TOD_melt, aes(x=variable,y=value,group=`Day of Week`,color=`Day of Week`))+
    geom_line(size=1.5)+
    geom_point(size=2.75, colour = "black")+
    scale_x_continuous(breaks=c(6:23), limits=c(6,23))+
    ggtitle(paste(Location[loc],"Volume by Hour of day"))+
    xlab("Hour of Day")+
    ylab("Volume")+
    scale_color_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(five)
}

Arrival_Roomed <- function(Date = "1/1/2000"){
  library(dplyr)
  df <- data %>% mutate(Arrival = as.Date(data$Arrival),
                        Location = as.character(data$Location))
  for(i in 1:nrow(df)){
    df[i,1] <- Location[as.character(paste0(df[i,1]))]
  }
  df <- df %>% filter(df$Arrival >= Date, df$`Arrival to Roomed` > 0) %>% group_by(Location, `Day of Week`) %>% summarize(Average = mean(`Arrival to Roomed`, na.rm = T), Median = median(`Arrival to Roomed`, na.rm = T), N = n())
  Average <- data %>% mutate(Arrival = as.Date(data$Arrival)) 
  Average <- Average %>%
    filter(Average$Arrival >= Date, Average$`Arrival to Roomed` > 0) %>% group_by(`Day of Week`) %>% summarize(Average = mean(`Arrival to Roomed`, na.rm = T), Median = median(`Arrival to Roomed`, na.rm = T), N = n()) %>%
    mutate(Location = "Average") %>% select(Location, `Day of Week`,Average,Median,N)
  df <- bind_rows(df,Average)
  levels(df$`Day of Week`) <- Days
  six <- ggplot(data = df, aes(fill = Location, x = `Day of Week`, y = Average)) +
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle("Patient Wait Time by Day of Week")+
    xlab("Day of Week")+
    ylab("Average Wait Time (min)")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(size = 15,face="bold"),
          axis.text = element_text(size=13),
          legend.text = element_text(size = 12),
          legend.title = element_text(size=15))
  return(six)
}

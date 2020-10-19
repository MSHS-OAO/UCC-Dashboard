#UCC Dashboard Functions

library(tidyverse)
library(anytime)
library(lubridate)
library(reshape2)
library(plotly)

#read in Master_Calc file
data <- read.csv("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_Calc.csv",
                 check.names=F)

Location <- c(`Ms Express Care`="MS Express Care",`UC10UNION [41028001]`="UC Union Square",
              `UCBROADWAY [8316001]`="UC Broadway",`UCCADMAN [8315001]`="UC Cadman", 
              `UCCOLUMBUS [8314001]`="UC Columbus",`UCYORK [8317001]`="UC York")
Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

#Create dataframe for each UCC after user defined date
Site <- function(file = data,date){
  file$Arrival <- as.Date(file$Arrival,format="%Y-%m-%d %H:%M:%S")
  file <- file %>%
    filter(Arrival >= date)
  
  MS_Expresscare <<- filter(file,Location=="Ms Express Care")
  Union_Square <<- filter(file,Location=="UC10UNION [41028001]")
  Broadway <<- filter(file,Location=="UCBROADWAY [8316001]")
  Cadman <<- filter(file,Location=="UCCADMAN [8315001]")
  Columbus <<- filter(file,Location=="UCCOLUMBUS [8314001]")
  York <<- filter(file,Location=="UCYORK [8317001]")
}

#Function for Day of week Volume by site
DOW_Volume <- function(df = data,Date = "1/1/2000"){
  df$Arrival <- as.Date(df$Arrival,format="%Y-%m-%d %H:%M:%S")
  timeframe <- as.numeric(max(df$Arrival,na.rm=T)-Date)/7
  #create location vector
  Location <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York")
  Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  Site(file = df, date=Date)
  DOW <- as.data.frame(matrix(0,nrow=10,ncol = 7))
  for(i in 1:length(Days)){
    DOW[1,i] <- round(nrow(MS_Expresscare[MS_Expresscare$`Day of Week`== Days[i] & !(is.na(MS_Expresscare$Location)),])/timeframe,digits = 2)
    DOW[2,i] <- round(nrow(Union_Square[Union_Square$`Day of Week`==Days[i] & !(is.na(Union_Square$Location)),])/timeframe,digits = 2)
    DOW[3,i] <- round(nrow(Broadway[Broadway$`Day of Week`==Days[i] & !(is.na(Broadway$Location)),])/timeframe,digits = 2)
    DOW[4,i] <- round(nrow(Cadman[Cadman$`Day of Week`==Days[i] & !(is.na(Cadman$Location)),])/timeframe,digits = 2)
    DOW[5,i] <- round(nrow(Columbus[Columbus$`Day of Week`==Days[i] & !(is.na(Columbus$Location)),])/timeframe,digits = 2)
    DOW[6,i] <- round(nrow(York[York$`Day of Week`==Days[i] & !(is.na(York$Location)),])/timeframe,digits = 2)
    
    DOW[7,i] <- round(mean(DOW[1:6,i]),digits = 2)
    DOW[8,i] <- round(median(DOW[1:6,i]),digits = 2)
    DOW[9,i] <- round(max(DOW[1:6,i]),digits = 2)
    DOW[10,i] <- round(min(DOW[1:6,i]),digits = 2)
  }
  DOW <- cbind(c(Location, "Average","Median","Maximum","Minimum"),DOW)
  colnames(DOW) <- c("Location",Days)
  return(DOW)
}

#Function for Time of day volume by site
TOD_Volume <- function(df = data,Date = "1/1/2000"){
  df$Arrival <- as.Date(df$Arrival,format="%Y-%m-%d %H:%M:%S")
  timeframe <- as.numeric(max(df$Arrival,na.rm=T)-Date)/7
  #create location vector
  Location <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York")
  Site(file = df, date = Date)
  TOD <- as.data.frame(matrix(0,nrow=10,ncol = 24))
  for(i in 1:24){
    TOD[1,i] <- round((nrow(MS_Expresscare[MS_Expresscare$`Hour of Day`==i-1 & !(is.na(MS_Expresscare$Location)) & !(is.na(MS_Expresscare$`Hour of Day`)),]))/timeframe,digits = 2)
    TOD[2,i] <- round((nrow(Union_Square[Union_Square$`Hour of Day`==i-1 & !(is.na(Union_Square$Location)) & !(is.na(Union_Square$`Hour of Day`)),]))/timeframe,digits = 2)
    TOD[3,i] <- round((nrow(Broadway[Broadway$`Hour of Day`==i-1 & !(is.na(Broadway$Location)) & !(is.na(Broadway$`Hour of Day`)),]))/timeframe,digits = 2)
    TOD[4,i] <- round((nrow(Cadman[Cadman$`Hour of Day`==i-1 & !(is.na(Cadman$Location)) & !(is.na(Cadman$`Hour of Day`)),]))/timeframe,digits = 2)
    TOD[5,i] <- round((nrow(Columbus[Columbus$`Hour of Day`==i-1 & !(is.na(Columbus$Location)) & !(is.na(Columbus$`Hour of Day`)),]))/timeframe,digits = 2)
    TOD[6,i] <- round((nrow(York[York$`Hour of Day`==i-1 & !(is.na(York$Location)) & !(is.na(York$`Hour of Day`)),]))/timeframe,digits = 2)
    
    TOD[7,i] <- round(mean(TOD[1:6,i]),digits = 2)
    TOD[8,i] <- round(median(TOD[1:6,i]),digits = 2)
    TOD[9,i] <- round(max(TOD[1:6,i]),digits = 2)
    TOD[10,i] <- round(min(TOD[1:6,i]),digits = 2)
  }
  TOD <- cbind(c(Location, "Average","Median","Maximum","Minimum"),TOD)
  colnames(TOD) <- c("Location",0:23)
  return(TOD)
}

#Create Table for time stamp compliance
compliance <- function(Date = "1/1/2000"){
  Comp_Table <- as.data.frame(matrix(data=0,nrow=7,ncol = 10))
  Site(date = Date)
  Comp_Table[,1] <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York","Total")
  
  #Count total encounters in the past week for each site (column 2)
  Comp_Table[1,2] <- nrow(MS_Expresscare[!is.na(MS_Expresscare$Location),])
  Comp_Table[2,2] <- nrow(Union_Square[!is.na(Union_Square$Location),])
  Comp_Table[3,2] <- nrow(Broadway[!is.na(Broadway$Location),])
  Comp_Table[4,2] <- nrow(Cadman[!is.na(Cadman$Location),])
  Comp_Table[5,2] <- nrow(Columbus[!is.na(Columbus$Location),])
  Comp_Table[6,2] <- nrow(York[!is.na(York$Location),])
  
  #Count of Total encounters where there is an NA value for patient Arrival (column 4)
  Comp_Table[1,4] <- nrow(MS_Expresscare[is.na(MS_Expresscare$Arrival) & !is.na(MS_Expresscare$Location),])
  Comp_Table[2,4] <- nrow(Union_Square[is.na(Union_Square$Arrival) & !is.na(Union_Square$Location),])
  Comp_Table[3,4] <- nrow(Broadway[is.na(Broadway$Arrival) & !is.na(Broadway$Location),])
  Comp_Table[4,4] <- nrow(Cadman[is.na(Cadman$Arrival) & !is.na(Cadman$Location),])
  Comp_Table[5,4] <- nrow(Columbus[is.na(Columbus$Arrival) & !is.na(Columbus$Location),])
  Comp_Table[6,4] <- nrow(York[is.na(York$Arrival) & !is.na(York$Location),])
  
  #Count of TOtal encounters where there is an NA value for patient Roomed (Column 5)
  Comp_Table[1,5] <- nrow(MS_Expresscare[is.na(MS_Expresscare$Roomed) & !is.na(MS_Expresscare$Location),])
  Comp_Table[2,5] <- nrow(Union_Square[is.na(Union_Square$Roomed) & !is.na(Union_Square$Location),])
  Comp_Table[3,5] <- nrow(Broadway[is.na(Broadway$Roomed) & !is.na(Broadway$Location),])
  Comp_Table[4,5] <- nrow(Cadman[is.na(Cadman$Roomed) & !is.na(Cadman$Location),])
  Comp_Table[5,5] <- nrow(Columbus[is.na(Columbus$Roomed) & !is.na(Columbus$Location),])
  Comp_Table[6,5] <- nrow(York[is.na(York$Roomed) & !is.na(York$Location),])
  
  #Count of Total encounters where there is an NA value for patient Discharge (Column 6)
  Comp_Table[1,6] <- nrow(MS_Expresscare[is.na(MS_Expresscare$Discharge) & !is.na(MS_Expresscare$Location),])
  Comp_Table[2,6] <- nrow(Union_Square[is.na(Union_Square$Discharge) & !is.na(Union_Square$Location),])
  Comp_Table[3,6] <- nrow(Broadway[is.na(Broadway$Discharge) & !is.na(Broadway$Location),])
  Comp_Table[4,6] <- nrow(Cadman[is.na(Cadman$Discharge) & !is.na(Cadman$Location),])
  Comp_Table[5,6] <- nrow(Columbus[is.na(Columbus$Discharge) & !is.na(Columbus$Location),])
  Comp_Table[6,6] <- nrow(York[is.na(York$Discharge) & !is.na(York$Location),])
  
  #Count of encounters where paitent left at some point. Entire visit was not completed
  Left <- c("LWBS Before Triage [6]","LWBS After Triage [7]","Left Before Treatment Completed (Eloped) [5]	","Left","AMA [4]")
  Left_df <- subset(data, Disposition %in% Left)
  Site(file = Left_df, date = Date)
  Comp_Table[1,3] <- nrow(MS_Expresscare[!is.na(MS_Expresscare$Location),])
  Comp_Table[2,3] <- nrow(Union_Square[!is.na(Union_Square$Location),])
  Comp_Table[3,3] <- nrow(Broadway[!is.na(Broadway$Location),])
  Comp_Table[4,3] <- nrow(Cadman[!is.na(Cadman$Location),])
  Comp_Table[5,3] <- nrow(Columbus[!is.na(Columbus$Location),])
  Comp_Table[6,3] <- nrow(York[!is.na(York$Location),])
  
  #Sum each column for the totals column
  for(i in 2:ncol(Comp_Table)){
    Comp_Table[7,i] <- sum(Comp_Table[1:6,i])
  }
  
  for(j in 3:6){
    for(i in 1:nrow(Comp_Table)){
      Comp_Table[i,j+4] <- paste0(round(Comp_Table[i,j]/Comp_Table[i,2]*100,2),"%")          
    }
  }
  
  Comp_Table <- Comp_Table[,c(1,2,3,7,4,8,5,9,6,10)]
  colnames(Comp_Table) <- c("Location","Total","Left","Left %","NA_Arrival","Arrival %","NA_Roomed","Roomed %","NA_Discharge","Discharge %")
  
  #Return Comp Table
  return(Comp_Table)
}


# Mount Sinai corporate colors "USE THIS TO ADD COLORS"
MountSinai_colors <- c(
  `light pink`   = "#fcc9e9",
  `med pink`     = "#fa93d4",
  `dark pink`    = "#d80b8c",
  `light purple` = "#c7c6ef",
  `med purple`   = "#8f8ce0",
  `light blue`   = "#5cd3ff",
  `med blue`     = "#06ABEB",
  `dark blue`    = "#212070",
  `light grey`   = "#b2b3b2",
  `dark grey`    = "#686868",
  `yellow`       = "#E69F00"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("med blue","dark pink","dark blue","light grey", "light blue",
                            "light pink", "light purple","med pink","med purple","yellow" ),
  
  `main`  = MountSinai_cols("med blue","dark pink","dark blue","dark grey","light pink","light blue","light grey"),
  
  `pink`  = MountSinai_cols("light pink", "dark pink"),
  
  `blue`  = MountSinai_cols("light blue", "dark blue"),
  
  `grey`  = MountSinai_cols("light grey", "med blue")
  
)
MountSinai_palettes

MountSinai_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

###-----Create DOW data frames for the entire repository (ALL Daily AVG.)
#The most recent week of data
DOW_Weekly <- DOW_Volume(Date=max(as.Date(data$Arrival,format="%Y-%m-%d %H:%M:%S"),na.rm = T)-7)
#Fiscal year to date
DOW_FYTD <- DOW_Volume(Date = floor_date(Sys.Date(),"year"))
#Past 30 days
DOW_30day <- DOW_Volume(Date=max(as.Date(data$Arrival,format="%Y-%m-%d %H:%M:%S"),na.rm = T)-30)

#Create TOD data frames for the entire repository (ALL Daily AVG.)
#The most recent week of data
TOD_Weekly <- TOD_Volume(Date=max(as.Date(data$Arrival,format="%Y-%m-%d %H:%M:%S"),na.rm = T)-7)
#Fiscal year to date
TOD_FYTD <- TOD_Volume(Date = floor_date(Sys.Date(),"year"))
#Past 30 days
TOD_30day <- TOD_Volume(Date=max(as.Date(data$Arrival,format="%Y-%m-%d %H:%M:%S"),na.rm = T)-30)

###Create compliance table for entire repository
Compliance_tot <- compliance()
#Compliance table for most recent week of data
Compliance_Weekly <- compliance(Date = max(anytime(data$Arrival), na.rm = T)-604800)
#Compliance table for FYTD
Compliance_FYTD <- compliance(Date = floor_date(Sys.Date(),"year"))
#Past 30 Days
Compliance_30day <- compliance(Date=max(anytime(data$Arrival), na.rm = T)-2952000)

###-----Graphs
##System Graphs
line_TOD <- function(TOD_table){
  if(all(TOD_table == TOD_Weekly)){
    timeframe = 7
  } else if(all(TOD_table == TOD_30day)){
    timeframe = 30
  } else if(all(TOD_table == TOD_FYTD)){
    timeframe = as.numeric(Sys.Date()-floor_date(Sys.Date(),"year"))
  }
  dfmelt <- TOD_table[c(1:7),] %>% pivot_longer(cols = 2:ncol(TOD_table),names_to = "Hour of Day",values_to = "Volume")%>%
    mutate(Location = factor(Location, levels = c("MS Express Care","UC Union Square","UC Broadway","UC Cadman","UC Columbus","UC York","Average")),
           Volume = as.numeric(Volume),
           `Hour of Day` = as.numeric(`Hour of Day`))
  two <- ggplot(data=dfmelt, aes(x=`Hour of Day`,y=Volume,group=Location,color=Location))+
    geom_line(size=1.5)+
    geom_point(size=2.75)+
    scale_x_continuous(breaks=c(6:23), limits=c(6,23))+
    ggtitle(paste0("Average Hourly Volume ","(Past ",timeframe," )"))+
    xlab("Hour of Day")+
    ylab("Volume")+
    scale_color_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  twoly <- ggplotly(two,tooltip=c("group","x","y")) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(twoly)
}
bar_DOW <- function(DOW_table){
  if(all(DOW_table == DOW_Weekly)){
    timeframe = 7
  } else if(all(DOW_table == DOW_30day)){
    timeframe = 30
  } else if(all(DOW_table == DOW_FYTD)){
    timeframe = as.numeric(Sys.Date()-floor_date(Sys.Date(),"year"))
  } 
  data <- DOW_table[1:7,] %>% pivot_longer(cols = 2:ncol(DOW_table),names_to = "Day of Week", values_to = "Volume") %>%
    mutate(Location = factor(Location, levels = c("MS Express Care","UC Union Square","UC Broadway","UC Cadman","UC Columbus","UC York","Average")),
           `Day of Week`=factor(`Day of Week`,levels = Days))
  one <- ggplot(data=data, aes(fill=Location, x=`Day of Week`,y=Volume)) +
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle(paste0("Average Daily Volume"," (Past ",timeframe," days)"))+
    xlab("Day of Week")+
    ylab("Volume")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  onely <- ggplotly(one) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(onely)
}
Arrival_Roomed <- function(Date = "1/1/2000"){
  df <- data %>% mutate(Arrival = as.Date(data$Arrival, format="%Y-%m-%d %H:%M:%S"),
                        Location = as.character(data$Location))
  timeframe <- as.numeric(max(df$Arrival,na.rm=T)-Date)
  for(i in 1:nrow(df)){
    df[i,1] <- Location[as.character(paste0(df[i,1]))]
  }
  df <- df %>% filter(Arrival >= Date,`Arrival to Roomed` > 0) %>% group_by(Location, `Day of Week`) %>% summarize(Average = mean(`Arrival to Roomed`, na.rm = T), Median = median(`Arrival to Roomed`, na.rm = T), N = n())
  Average <- data %>% mutate(Arrival = as.Date(data$Arrival)) 
  Average <- Average %>%
    filter(Arrival >= Date,`Arrival to Roomed` > 0) %>% group_by(`Day of Week`) %>% summarize(Average = mean(`Arrival to Roomed`, na.rm = T), Median = median(`Arrival to Roomed`, na.rm = T), N = n()) %>%
    mutate(Location = "Average") %>% select(Location, `Day of Week`,Average,Median,N)
  df <- bind_rows(df,Average) %>% mutate(Average = round(Average,digits = 2))
  df <- df %>% pivot_wider(id_cols = Location,names_from = `Day of Week`,values_from = Average)
  df[is.na(df)] <- 0
  df <- df %>% pivot_longer(cols = 2:ncol(df),names_to = "Day of Week",values_to = "Average")
  df$Location <- factor(df$Location,levels = c(Location,"Average"))
  df$`Day of Week` <- factor(df$`Day of Week`,Days)
  six <- ggplot(data = df, aes(fill = Location, x = `Day of Week`, y = Average)) +
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle(paste0("Average Patient Wait Time ","(Past ",timeframe," days)"))+
    xlab("Day of Week")+
    ylab("Average Wait Time (min)")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(size = 15,face="bold"),
          axis.text = element_text(size=13),
          legend.text = element_text(size = 12),
          legend.title = element_text(size=15))
  sixly <- ggplotly(six) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(sixly)
}

##Site Graphs
bar_DOW_site <- function(loc){
  df <- rbind(DOW_FYTD[DOW_FYTD$Location == loc,],DOW_30day[DOW_30day$Location == loc,],DOW_Weekly[DOW_Weekly$Location == loc,],DOW_30day[DOW_30day$Location == "Average",])
  df <- mutate(df,Location = factor(c(paste0(year(Sys.Date())," Daily Avg."),"30 day Avg.","Past Week Avg.","System Daily Avg. (30 Day)"),levels = c(paste0(year(Sys.Date())," Daily Avg."),"30 day Avg.","Past Week Avg.","System Daily Avg. (30 Day)")))
  dfmelt <- df %>% pivot_longer(cols = 2:8,names_to = "Day of Week",values_to = "Volume")
  colnames(dfmelt) <- c("Timeframe","variable","value")
  three <- ggplot(data = dfmelt,aes(fill=Timeframe, x=variable,y=value))+
    geom_bar(position="dodge", stat="identity", colour = "Black") +
    ggtitle(paste0(loc," Volume by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Daily)")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  threely <- ggplotly(three) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(threely)
}
box_DOW_site <- function(loc, Date = "1/1/2000"){
  x <- Location[[loc]]
  df1 <- data %>% 
    mutate(Arrival = as.Date(Arrival,format = "%Y-%m-%d %H:%M:%S")) %>%
    filter(Arrival >= Date,
           Location == loc,
           !is.na(`Day of Week`))%>% 
    mutate(Location = x)
  #all data for entire system
  df2 <- data %>% 
    mutate(Arrival = as.Date(Arrival,format = "%Y-%m-%d %H:%M:%S")) %>%
    filter(Arrival >= Date,
           !is.na(Location),
           !is.na(`Day of Week`)) %>%
    mutate(Location = "System")
  #combine df1 and df2
  data1 <- rbind(df1,df2)
  data2 <- data1 %>%
    group_by(Location,Arrival,`Day of Week`) %>%
    summarise(Volume = n()) %>%
    ungroup() %>%
    select(Location,`Day of Week`,Volume) %>%
    mutate(Location = factor(Location,levels = c(x,"System")),
           `Day of Week` = factor(`Day of Week`,levels = Days))
  data3 <- data2 %>% 
    mutate(Volume =case_when(
      Location == "System" ~ round(as.numeric(Volume/5L),digits=2),
      TRUE ~ round(as.numeric(Volume),digits=2)))
  four <- ggplot(data=data3, aes(x=`Day of Week`, y=Volume, fill=Location))+
    geom_boxplot()+
    ggtitle(paste0(x," Daily Encounters by Day of Week"))+
    xlab("Day of Week")+
    ylab("Volume (Encounters)")+
    scale_fill_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  fourly <- ggplotly(four) %>%
    config(displaylogo = F,modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center"),
           boxmode = "group") #turn graph into plotly interactive
  return(fourly)
}
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
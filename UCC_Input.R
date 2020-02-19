#---UCC Dashboard Preprocess-------

#read in Master_Calc file
data <- read.csv("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_Calc.csv",
                 check.names=F) 
library(tinytex)
library(anytime)
library(lubridate)
library(ggplot2)
library(reshape2)

#Create dataframe for each UCC after user defined date
Site <- function(file = data, date = "1/1/2000"){
  date <- anytime(date)
  file$Arrival <- anytime(file$Arrival)
  file <- file[file$Arrival > date | is.na(file$Arrival),]
  
  MS_Expresscare <<- file[file$Location == "Ms Express Care",]
  Union_Square <<- file[file$Location == "UC10UNION [41028001]",]
  Broadway <<- file[file$Location == "UCBROADWAY [8316001]",]
  Cadman <<- file[file$Location == "UCCADMAN [8315001]",]
  Columbus <<- file[file$Location == "UCCOLUMBUS [8314001]",]
  York <<- file[file$Location == "UCYORK [8317001]",]
}

#Function for Day of week Volume by site
DOW_Volume <- function(df = data,Date = "1/1/2000"){
  #create location vector
  Location <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York")
  Days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  Site(file = df, date=Date)
  DOW <- as.data.frame(matrix(0,nrow=10,ncol = 7))
  for(i in 1:length(Days)){
    DOW[1,i] <- nrow(MS_Expresscare[MS_Expresscare$`Day of Week`== Days[i] & !(is.na(MS_Expresscare$Location)),])
    DOW[2,i] <- nrow(Union_Square[Union_Square$`Day of Week`==Days[i] & !(is.na(Union_Square$Location)),])
    DOW[3,i] <- nrow(Broadway[Broadway$`Day of Week`==Days[i] & !(is.na(Broadway$Location)),])
    DOW[4,i] <- nrow(Cadman[Cadman$`Day of Week`==Days[i] & !(is.na(Cadman$Location)),])
    DOW[5,i] <- nrow(Columbus[Columbus$`Day of Week`==Days[i] & !(is.na(Columbus$Location)),])
    DOW[6,i] <- nrow(York[York$`Day of Week`==Days[i] & !(is.na(York$Location)),])
    
    DOW[7,i] <- mean(DOW[1:6,i])
    DOW[8,i] <- median(DOW[1:6,i])
    DOW[9,i] <- max(DOW[1:6,i])
    DOW[10,i] <- min(DOW[1:6,i])
  }
  DOW <- cbind(c(Location, "Average","Median","Maximum","Minimum"),DOW)
  colnames(DOW) <- c("Location",Days)
  return(DOW)
}

#Function for Time of day volume by site
TOD_Volume <- function(df = data,Date = "1/1/2000"){
  #create location vector
  Location <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York")
  Site(file = df, date = Date)
  TOD <- as.data.frame(matrix(0,nrow=10,ncol = 24))
  for(i in 1:24){
    TOD[1,i] <- nrow(MS_Expresscare[MS_Expresscare$`Hour of Day`==i-1 & !(is.na(MS_Expresscare$Location)),])
    TOD[2,i] <- nrow(Union_Square[Union_Square$`Hour of Day`==i-1 & !(is.na(Union_Square$Location)),])
    TOD[3,i] <- nrow(Broadway[Broadway$`Hour of Day`==i-1 & !(is.na(Broadway$Location)),])
    TOD[4,i] <- nrow(Cadman[Cadman$`Hour of Day`==i-1 & !(is.na(Cadman$Location)),])
    TOD[5,i] <- nrow(Columbus[Columbus$`Hour of Day`==i-1 & !(is.na(Columbus$Location)),])
    TOD[6,i] <- nrow(York[York$`Hour of Day`==i-1 & !(is.na(York$Location)),])
    
    TOD[7,i] <- mean(TOD[1:6,i])
    TOD[8,i] <- median(TOD[1:6,i])
    TOD[9,i] <- max(TOD[1:6,i])
    TOD[10,i] <- min(TOD[1:6,i])
  }
  TOD <- cbind(c(Location, "Average","Median","Maximum","Minimum"),TOD)
  colnames(TOD) <- c("Location",0:23)
  return(TOD)
}

#Create Table for time stamp compliance
compliance <- function(Date = "1/1/2000"){
  Comp_Table <- as.data.frame(matrix(data=0,nrow=7,ncol = 6))
  Site(date = Date)
  Comp_Table[,1] <- c("MS Express Care", "UC Union Square", "UC Broadway", "UC Cadman", "UC Columbus", "UC York","Total")
  colnames(Comp_Table) <- c("Location","Total","Left","NA_Arrival","NA_Roomed","NA_Discharge")
  
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
  
  #Count of TOtal encounters where there is an NA value for patient Discharge (Column 6)
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
  
  #Return Comp Table
  return(Comp_Table)
}

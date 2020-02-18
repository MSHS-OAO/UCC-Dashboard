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
  file$Arrival <- anytime(file$Arrival)
  date <- anytime(date)
  MS_Expresscare <<- file[file$Location == "Ms Express Care" & file$Arrival > date,]
  Union_Square <<- file[file$Location == "UC10UNION [41028001]"& file$Arrival > date,]
  Broadway <<- file[file$Location == "UCBROADWAY [8316001]"& file$Arrival > date,]
  Cadman <<- file[file$Location == "UCCADMAN [8315001]" & file$Arrival > date,]
  Columbus <<- file[file$Location == "UCCOLUMBUS [8314001]"& file$Arrival > date,]
  York <<- file[file$Location == "UCYORK [8317001]"& file$Arrival > date,]
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

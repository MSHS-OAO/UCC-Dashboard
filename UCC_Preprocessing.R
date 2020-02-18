#------UCC Dashboard---------------

##Read in most recent file
#---File Selection---
#Read in most recent MS Expresscare file
df <- file.info(list.files("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/MS Expresscare", full.names = T))
Express <- read.csv(rownames(df)[which.max(df$mtime)], header = T)

#Read in the most recent UCC (all UCC's exceot MS Expresscare) file
df2 <- file.info(list.files("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Urgent Cares", full.names = T))
UCC <- read.csv(rownames(df2)[which.max(df2$mtime)], header = T)

#Read in Master raw files
Master_Express <- read.csv("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_Express.csv")
Master_UCC <- read.csv("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_UCC.csv")

#Append new raw files to masters
Master_Express <- rbind(Master_Express,Express)
Master_UCC <- rbind(Master_UCC,UCC)

#Overwrite Master raw files
write.csv(Master_Express, file="J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_Express.csv",row.names = F)
write.csv(Master_UCC, file="J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_UCC.csv", row.names = F )


##Take weekly raw files and perform calculations (add year for FYTD calcs)
#--MS Expresscare----
Express$Roomed <- as.character(Express$Roomed)
#Convert arrival date/time into proper format
Express$Arrival.Date.Time <- as.POSIXct(as.character(paste0(substr(Express$Arrival.Date.Time,start=2,stop=7),"20",substr(Express$Arrival.Date.Time,start=8,stop=12),":",substr(Express$Arrival.Date.Time,start=13,stop=14),":00"))
                                        , format = "%m/%d/%Y %H:%M:%S")
#Convert roomed date/time into proper format
Express$Roomed <- as.POSIXct(as.character(paste0(substr(Express$Disch.Date.Time,start=2,stop=11)," ",substr(Express$Roomed,start=nchar(Express$Roomed)-3,stop=nchar(Express$Roomed)-2),":",substr(Express$Roomed,start=nchar(Express$Roomed)-1,stop=nchar(Express$Roomed)),":00"))
                             , format = "%m/%d/%Y %H:%M:%S")
#Convert discharge date/time into proper format
Express$Disch.Date.Time <- as.POSIXct(as.character(paste0(substr(Express$Disch.Date.Time,start=2,stop=14),":",substr(Express$Disch.Date.Time,start=15,stop=16),":00"))
                                      , format = "%m/%d/%Y %H:%M:%S")
#--UCC----------------
#add "0" to all times with only 3 characters
UCC[nchar(UCC$Check.In)==3 & !is.na(UCC$Check.In), "Check.In"] <- paste0("0",UCC[nchar(UCC$Check.In)==3 & !is.na(UCC$Check.In), "Check.In"])
UCC[nchar(UCC$Roomed.Time)==3 & !is.na(UCC$Roomed.Time), "Roomed.Time"] <- paste0("0",UCC[nchar(UCC$Roomed.Time)==3 & !is.na(UCC$Roomed.Time), "Roomed.Time"])
UCC[nchar(UCC$Visit.End)==3 & !is.na(UCC$Visit.End), "Visit.End"] <- paste0("0",UCC[nchar(UCC$Visit.End)==3 & !is.na(UCC$Visit.End), "Visit.End"])

#Convert arrival time into proper format
UCC$Check.In <- as.POSIXct(as.character(paste0(UCC$Date,substr(UCC$Check.In,start=1,stop=2),":",substr(UCC$Check.In,start=3,stop=4),":00"))
                           , format = "%m/%d/%Y %H:%M:%S")
#Convert roomed time into proper format
UCC$Roomed.Time <- as.POSIXct(as.character(paste0(UCC$Date,substr(UCC$Roomed.Time,start=1,stop=2),":",substr(UCC$Roomed.Time,start=3,stop=4),":00"))
                              , format = "%m/%d/%Y %H:%M:%S")
#Convert visite end time into proper format
UCC$Visit.End <- as.POSIXct(as.character(paste0(UCC$Date,substr(UCC$Visit.End,start=1,stop=2),":",substr(UCC$Visit.End,start=3,stop=4),":00"))
                            , format = "%m/%d/%Y %H:%M:%S")

#Create data frames for MS Expresscare and UCC's with necessary timestamps
#MSE
MSE <- data.frame(Location=Express$Encounter.Dept.Name, Arrival=Express$Arrival.Date.Time, Roomed=Express$Roomed,
                  Discharge=Express$Disch.Date.Time, Disposition=Express$ED.Dispo)
#UCC
UC <- data.frame(Location=UCC$Dept, Arrival=UCC$Check.In, Roomed=UCC$Roomed.Time, Discharge=UCC$Visit.End,Disposition=UCC$Appt.Status)
#Combine data frames
Calc <- rbind(MSE,UC)
#Perform and bind necessary calculations
#Hour of arrival, arrival to roomed, roomed to discharge, arrival to discharges, DOW
library(lubridate)
Calc <- cbind(Calc,as.numeric(strftime(Calc$Arrival, format="%H")),difftime(Calc$Roomed,Calc$Arrival,units="mins"), difftime(Calc$Discharge,Calc$Roomed,units="mins"),difftime(Calc$Discharge,Calc$Arrival,units="mins"),
              weekdays(Calc$Arrival), year(Calc$Arrival))
#Apply Column names
colnames(Calc) <- c("Location","Arrival", "Roomed", "Discharge","Disposition", "Hour of Day", "Arrival to Roomed", "Roomed to Discharge", "Arrival to Discharge", "Day of Week", "Year")
#Sort by arrival date and time
Calc <- Calc[order(Calc$Arrival),]
#Append Calc to master file
Master_Calc <- read.csv("J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_Calc.csv")
colnames(Master_Calc) <- c("Location","Arrival", "Roomed", "Discharge","Disposition", "Hour of Day", "Arrival to Roomed", "Roomed to Discharge", "Arrival to Discharge", "Day of Week", "Year")
#Master_Calc[nchar(Master_Calc$`Hour of Day`)==1 & !is.na(Master_Calc$`Hour of Day`), "Hour of Day"] <- paste0("0",Master_Calc[nchar(Master_Calc$`Hour of Day`)==1 & !is.na(Master_Calc$`Hour of Day`), "Hour of Day"])
#Master_Calc$Hour.of.Day <- as.factor(Master_Calc$`Hour of Day`)
library(anytime)
Master_Calc$Arrival <- anytime(Master_Calc$Arrival)
Master_Calc$Roomed <- anytime(Master_Calc$Roomed)
Master_Calc$Discharge <- anytime(Master_Calc$Discharge)
Master_Calc <- rbind(Master_Calc,Calc)
#Overwrite Master Calc file
write.csv(Master_Calc,file="J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/MSHS UCC/Data/Master/Master_Calc.csv",row.names=F)
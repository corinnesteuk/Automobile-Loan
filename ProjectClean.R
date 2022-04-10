#reading in the data
library(lubridate) 
library(plyr) 
library(eeptools)
library(dplyr)
data <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault.csv", sep = ',')
head(data)


###DROPPING COLUMNS
#drop columns beginning with SEC (the secondary account info) 
data <- data[,!grepl("^SEC",names(data))]
#dropping other columns that do not pertain to project
data <- subset(data, select = -c(manufacturer_id, Current_pincode_ID, Driving_flag, Passport_flag, 
                                 MobileNo_Avl_Flag, Aadhar_flag, PAN_flag, VoterID_flag, UniqueID) )

###CREATING NEW COLUMN PERCENT.DISBURSED
#we want to create a column of the percentage of the loan given out at this time
#this is calculated by the disbursed/sanction columns (primary account only)
data['Percent.Disbursed'] <- data$PRI.DISBURSED.AMOUNT/ data$PRI.SANCTIONED.AMOUNT
#replacing the NaN with 0s
data$Percent.Disbursed[is.na(data$Percent.Disbursed)] <- 0

###FILLING EMPLOYEEMENT TYPE WITH NA (should we switch to "Unknown"?)
data[data==""]<- NA
data$Employment.Type


###CONVERTING DATE.OF.BIRTH TO AGE 
d <- as.list(data)
dob <- as.list(scan(text=d$Date.of.Birth, what=" "))
dob <- dmy(dob) 
# this package makes years earlier than "60" as 2060, so this loop changes them back to the twentieth century
i <- 1
while (i <= length(dob)){
  if (year(dob[i])> 2000){
    year(dob[i]) <- year(dob_l[i])-100
  }
  i <- i+1
}
#We add a new column for the age
data['Age'] <- age_calc(dob,  units = 'years')




head(data)

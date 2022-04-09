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
data$Date.of.Birth <- dob 
require(lubridate)
x_date <- as.Date("2022-01-01")
as.POSIXct(dob)
# trying to solve problem of years greater than 2000 (ex. 2068)
a <- c()
years <- format(dob, format="%Y")
years <- as.list(scan(text=years, what=" "))
years <- as.list(as.integer(years))
ind <- 1
for (i in years){
  if (i>2000){
    years[ind] <- i-100 
  }
  ind <- ind +1
}
#this separates the month and day, can we join them back with the year?
m_d <- format(dob, format="%m-%d")
m_d<- as.list(scan(text=m_d, what=" "))
#years are right but they are all with april, 9 (month and day)
x_date2 <- strptime(years, format = "%Y") 
x_date2 <- as.Date(x_date2)
#adding a column for age (the month and day of birthdays are all april 9) 
#but years are right, these may be relatively right depending on when this data
#was collected
data['Age'] <- age_calc(x_date2,  units = 'years')

head(data)

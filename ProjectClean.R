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

###DISBURSED DATE TO LENGTH TO MONTH COUNT
#we didn't have the year problem before since all these years are in the 21st century
dis <- as.list(scan(text=d$DisbursalDate, what=" "))
dis <- dmy(dis)
data['Disbursed.Time.Months'] <- age_calc(dis,  units = 'months')
head(data)

data1 <- data

###AVG ACCOUNT AGE & CREDIT HISTORY LENGTH TO LENGTH IN MONTHS
#Credit history length first:
data1$CREDIT.HISTORY.LENGTH <- regmatches(data1$CREDIT.HISTORY.LENGTH, gregexpr("[[:digit:]]+",data1$CREDIT.HISTORY.LENGTH))
to_date_in_months <- function(x){
  as.numeric(unlist(x))[1] * 12 + as.numeric(unlist(x))[2]
}

#switching from list to vector to impute in data
x <- lapply(data1$CREDIT.HISTORY.LENGTH, to_date_in_months)
i <- 1
v <- seq(1, length(x), 1)
while (i <= length(x)){
  v[i] <- as.integer(x[i])
  i <- i+1
}
data1$CREDIT.HISTORY.LENGTH <- v

#Now Account Age:
data1$AVERAGE.ACCT.AGE <- regmatches(data1$AVERAGE.ACCT.AGE, gregexpr("[[:digit:]]+", data1$AVERAGE.ACCT.AGE))

x <- lapply(data1$AVERAGE.ACCT.AGE, to_date_in_months)
i <- 1
v <- seq(1, length(x), 1)
while (i <= length(x)){
  v[i] <- as.integer(x[i])
  i <- i+1
}
data1$AVERAGE.ACCT.AGE <- v

write.csv(data1,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Clean.csv", row.names = FALSE)


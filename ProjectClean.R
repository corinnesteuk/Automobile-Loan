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
<<<<<<< HEAD
write.csv(data,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Clean.csv", row.names = FALSE)
=======
=======
data <- read.csv("Train_Dataset.csv", sep = ",")
head(data)

#checking how many NAs there are
sum(is.na(data))
#264134

#well, that's not ideal
#some are just empty strings
sum(is.na(data$Registration_Days))
sum(data$Registration_Days == '')
typeof(data$Registration_Days[3])
typeof(data$Client_Occupation[3])
data$Score_Source_1[2]

#replacing empty strings w/ NAs
n_rows <- dim(data)[1]
n_columns <- dim(data)[2]
for (i in 1:n_columns){
  data[,i][data[,i] == ''] <- NA
}

#Changing columns of numbers that were originally characters 
#to type integer or double (so NA's will populate)
options(digits = 10)
data$Client_Income <- as.integer(data$Client_Income)
data$Credit_Amount <- as.numeric(data$Credit_Amount)
data$Loan_Annuity <- as.numeric(data$Loan_Annuity)
data$Population_Region_Relative <- as.numeric(data$Population_Region_Relative)
data$Age_Days <- as.numeric(data$Age_Days)
data$Employed_Days <- as.numeric(data$Employed_Days)
data$Registration_Days <- as.numeric(data$Registration_Days)
data$ID_Days <- as.numeric(data$ID_Days)
data$Application_Process_Hour <- as.numeric(data$Application_Process_Hour)
data$Score_Source_3 <- as.numeric(data$Score_Source_3)

#checking NAs again
sum(is.na(data))
#395811

column_nas <- rep(0, n_columns)
for (i in 1:n_columns){
  column_nas[i] <- sum(is.na(data[,i]))
}
column_nas #number of NAs per column
column_nas / n_rows #percentage of NAs per column

sum(!complete.cases(data))
row_nas #number of rows w/ NAs
row_nas / n_rows #percentage of rows w/ NAs
>>>>>>> f5575e46a78d14dfb2cf1f486600a287f9b00d0f


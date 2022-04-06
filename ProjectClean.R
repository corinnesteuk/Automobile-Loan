#reading in the data
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

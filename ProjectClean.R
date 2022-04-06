data <- read.csv("/Users/corinnesteuk/Documents/STAT310/AutomobileLoan.csv", sep = ",")
head(data)

sum(is.na(data))

sum(is.na(data$Registration_Days ))
data$Registration_Days[3]
typeof(data$Registration_Days[3])
typeof(data$Client_Occupation[3])
data$Score_Source_1[2]

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

sum(is.na(data))

sum(is.na(data$Registration_Days ))
data$Registration_Days[3]
typeof(data$Registration_Days[3])
typeof(data$Client_Occupation[3])
data$Score_Source_1[2]

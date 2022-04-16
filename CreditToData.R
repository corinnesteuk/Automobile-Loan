library(dplyr)
library(plyr)
library(ggplot2)

credit_model <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CreditModel.csv")
risk_info <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CreditCheatSheet.csv")
credit_risk <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CreditRisk.csv")
data <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Clean.csv")
head(credit_risk)
head(data)
head(credit_model)
#We are joining credit_risk and data together on ID
data <- dplyr::rename(data, id = UniqueID)
full_data <- merge(credit_model, data, by = "id")  
head(full_data)

#Dropping duplicate columns including columns that we changed the format for  - DOC & Disbursal Date)
full_data <- subset(full_data, select = -c(PERFORM_CNS.SCORE.DESCRIPTION, PERFORM_CNS.SCORE, Date.of.Birth, DisbursalDate) )
#more cleaning - renaming columns
full_data <- dplyr::rename(full_data, risk_rate = letter)
full_data <- dplyr::rename(full_data, risk_desc = description)
full_data <- dplyr::rename(full_data, new_accts = NEW.ACCTS.IN.LAST.SIX.MONTHS)
full_data <- dplyr::rename(full_data, deliquent_accts = DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)
full_data <- dplyr::rename(full_data, months_disbursed = Disbursed.Time.Months)
full_data <- dplyr::rename(full_data, active_accts = PRI.ACTIVE.ACCTS )
full_data <- dplyr::rename(full_data, overdue_accts = PRI.OVERDUE.ACCTS)
full_data <- dplyr::rename(full_data, current_balance = PRI.CURRENT.BALANCE)
full_data <- dplyr::rename(full_data, sanctioned_amt = PRI.SANCTIONED.AMOUNT)
full_data <- dplyr::rename(full_data, install_amt = PRIMARY.INSTAL.AMT)
full_data <- dplyr::rename(full_data, avg_acct_age = AVERAGE.ACCT.AGE)
full_data <- dplyr::rename(full_data, credit_history_length = CREDIT.HISTORY.LENGTH)
full_data <- dplyr::rename(full_data, no_inquiries = NO.OF_INQUIRIES)
full_data <- dplyr::rename(full_data, no_accts = PRI.NO.OF.ACCTS)
head(full_data)

#Writing to CSV
write.csv(full_data,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Final.csv", row.names = FALSE)


###EXPLORATORY VISUALS
#Figure 1: Visuals for Credit Risk Info 
head(risk_info)
ggplot(data = full_data) + 
  stat_summary(
    mapping = aes(x = risk_rating, y = credit, color = risk_description),
    fun.min = min,
    fun.max = max,
    fun = mean) + 
  labs(title="Figure1: Credit Risk Info")

#Visuals for Distribution of Risk Rating & Description
ggplot(data = full_data) + 
  geom_bar(mapping=aes(x = risk_rating,  fill = risk_description))+ 
  labs(title="Figure2: Credit Risk Distribution")

#Visual Credit Score & Loan Default
ggplot(data = full_data) + 
  geom_bar(aes(x = risk_rating, fill = factor(loan_default)), position = 'dodge')+ 
  labs(title="Figure3: Credit Score & Loan Default")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

cor(full_data$ltv.y, full_data$asset)
head(full_data)
range(full_data$Age)

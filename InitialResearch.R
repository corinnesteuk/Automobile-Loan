data <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Clean.csv")
head(data)
credit_scores <- data$PERFORM_CNS.SCORE


###How many credit score unknowns are there is this data set?
i <- 1
count0 <- 0
countno <- 0
while (i <= length(credit_scores)){
  if (credit_scores[i] == 0){
    count0 <- count0 +1
  }else if (credit_scores[i] < 50){
    countno <- countno +1
  }
  i <- i+1
}
#it looks like about half of the credit scores are 0 (50%) and about 5% have a different value meaning they were not recorded 
count0
countno
total_empty_credit <- count0 +countno
#Therefore, about 55% of credit scores will need to be filled in 
total_empty_credit/length(credit_scores)

no_acc <- data$PRI.NO.OF.ACCTS
current_bal <- data$PRI.CURRENT.BALANCE
overdue_acct <- data$PRI.OVERDUE.ACCTS
new_acct <- data$NEW.ACCTS.IN.LAST.SIX.MONTHS
sanctioned <- data$PRI.SANCTIONED.AMOUNT
delinquent <- data$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS
disbursed <- data$disbursed_amount
asset <- data$asset_cost

df <- cbind()


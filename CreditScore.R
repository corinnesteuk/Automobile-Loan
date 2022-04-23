library(arm)
library(lmtest)
library(car)
library(dplyr)
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
disbursed <- data$PRI.DISBURSED.AMOUNT
asset <- data$asset_cost
age <- data$Age
ltv <- data$ltv
credit <- data$PERFORM_CNS.SCORE
default <- data$loan_default
id <- data$UniqueID
acct_age <- data$AVERAGE.ACCT.AGE
credit_age <- data$CREDIT.HISTORY.LENGTH

#creating a data frame and then subset only rows with known credit scores
#this is kept in a data frame called credit_known to be used in a prediction
#linear regression model
df <- data.frame(cbind(id, no_acc, current_bal, overdue_acct, new_acct, sanctioned,
            delinquent, disbursed, asset, age, default, ltv, credit, acct_age,credit_age  ))
credit_known <- df[df$credit>50, ] 
#Correlation of variables to each other and to credit variable
cor(credit_known)
credit_cor <- cor(df[ , colnames(df) != "credit"],  # Calculate correlations
                  df$credit)
credit_cor

#fitting logistic regression model on the known credit scores
fit_credit <- lm(credit ~ no_acc+current_bal+overdue_acct+new_acct+sanctioned+
                   delinquent+disbursed+asset+age+acct_age+credit_age+default, data = credit_known)
fit_null <- lm(credit~1, data = credit_known)
summary(fit_credit) 
#no_acc and age have high p values, we could look to see if they should be removed
# low R score of .42
# coefficients are either really large or really small
#maybe we need to adjust the model-not all parameters are necessary
lrtest(fit_null, fit_credit)
# small p-value leads us to reject null and conclude at least one Beta is non zero
Anova(fit_credit)

fit_no_age <- lm(credit ~ no_acc+current_bal+overdue_acct+new_acct+sanctioned+
                   delinquent+disbursed+asset, data = credit_known)
fit_no_acct <- lm(credit ~ current_bal+overdue_acct+new_acct+sanctioned+
                   delinquent+disbursed+asset+age, data = credit_known)
anova(fit_credit, fit_no_age)
anova(fit_credit, fit_no_acct)
#both anova tests confirm these values should be removed

fit_credit <- lm(credit ~ current_bal+overdue_acct+new_acct+sanctioned+
                                 delinquent+disbursed+asset, data = credit_known)
fit_sanctioned <- lm(credit ~ current_bal+overdue_acct+new_acct+
                       delinquent+disbursed+asset, data = credit_known)
anova(fit_credit, fit_sanctioned)
summary(fit_credit)
# although sanctioned has a high-ish p-value, I think this should be kept in because it is the total loan amount
#disbursed amount could be removed since it is just how much of the loan has been given 
#we then included ltv ratio, which is the percentage of the vehicle covered by loan, this takes the place of sanctioned and disbursed
#should we add default, would this be factor?
#the model below adds factor(default) & age, removes sanctioned & disbursed
fit_final <- lm(credit ~ ltv+current_bal+overdue_acct+new_acct+
                   delinquent+asset+age+ acct_age+credit_age+factor(default), data = credit_known)
summary(fit_final)
anova(fit_null, fit_final)
#although age still has a high p-value, we think this should be kept in the model logically 

#this is our test set that we need to use the credit model to predict
credit_unknown <- df[df$credit<50, ] 
credit_unknown <- subset(credit_unknown , select = -c(credit))
head(credit_unknown)

#how many people with unknown credit scores defaulted on their loans?
sum(credit_unknown$default)/length(credit_unknown$default)
#how many people with known credit scores defaulted on their loans?
sum(credit_known$default)/length(credit_known$default)

#prediction for unknown credit scores
prediction <- predict(fit_final, newdata = credit_unknown)
prediction
#replacing adding this to the credit_unknown data frame
credit_unknown <- cbind(credit_unknown, round(prediction))
credit_unknown <- rename(credit_unknown, credit = "round(prediction)")

#looking at the predictions
mean(prediction)
#mean: 695
max(prediction)
#max: 820
min(prediction)
#min: 110
# change minimums for categorizing purposes, since min of known credit scores is 300
min(credit_known$credit)
credit_unknown$credit[credit_unknown$credit < 300] <- 300
min(credit_unknown$credit)


head(credit_known)
head(credit_unknown)
full_data <- rbind(credit_known, credit_unknown)

head(full_data)
write.csv(full_data,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CleanCredit.csv", row.names = FALSE)



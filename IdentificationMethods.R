library(lmtest)

#Base rate of default
prop.table(table(data$loan_default))

#Rates of default based of different IDs
prop.table(table(data$Aadhar_flag, data$loan_default), margin = 1)
prop.table(table(data$PAN_flag, data$loan_default), margin = 1)
prop.table(table(data$VoterID_flag, data$loan_default), margin = 1)
prop.table(table(data$Driving_flag, data$loan_default), margin = 1)
prop.table(table(data$Passport_flag, data$loan_default), margin = 1)

#Contingency table w/ percentages by row for # of IDs used and default status
prop.table(table(rowSums(data[15:19])))
prop.table(table(rowSums(data[15:19]), data$loan_default), margin=1)

#comparing a model on IDs to a baseline model
fit0 <- glm(loan_default ~ 1, family = binomial, data = data)
fit <- glm(loan_default ~ data$Aadhar_flag + PAN_flag + VoterID_flag + Driving_flag + Passport_flag, family = binomial, data = data)
lrtest(fit, fit0)
summary(fit)
fit$

#predicted values for making an ROC curve
preds <- fitted(fit)
library(pROC)
rocplot <- roc(loan_default ~ preds, data=data)
plot.roc(rocplot, legacy.axes=TRUE)
auc(rocplot)

#subsetting data where people used two forms of ID and defaulted
#would like to figure out what were the most common pairings of IDs and then test for independence
yum <- data[rowSums(data[15:19]) == 2,c(15, 16, 17, 18, 19, 34)]
yum[yum$loan_default == 1,]
levels(as.factor(data$Aadhar_flag))

#function to turn 5 ID columns into 1
binary_levels <- function(x, y, z, p, d){
  return (x + y * 10 + z * 100 + p * 1000 + d * 10000)
}
IDs <- binary_levels(data$Aadhar_flag, data$PAN_flag, data$VoterID_flag, data$Driving_flag, data$Passport_flag)
levels(as.factor(IDs))
data$IDs <- IDs
prop.table(table(data$IDs, data$loan_default), margin = 1)

#ID Frequencies
#One ID
#Aadhar
sum(IDs == 1) #182745
#PAN
sum(IDs == 10) #973
#VoterID
sum(IDs == 100) #25251
#Driving
sum(IDs == 1000) #4051
#Passport
sum(IDs == 10000) #369

#Two IDs
#Aadhar & PAN Flag (11)
sum(IDs == 11) #9933
#Aadhar & Voter ID (101)
sum(IDs == 101) #2005
#Aadhar & Driving Flag(1001)
sum(IDs == 1001) #845 
#Aadhar & Passport (10001)
sum(IDs == 10001) #76
#Voter ID & PAN Flag (110)
sum(IDs == 110) #6095
#PAN & Driving (1010)
sum(IDs == 1010) #273
#VoterID & Driving (1100)
sum(IDs == 1100) #170
#Passport & PAN (10010)
sum(IDs == 10010) #29
#Passport & VoterID (10100)
sum(IDs == 10100) #5
#Passport & Driving (11000)
sum(IDs == 11000) #3

#THREE IDs
#Aadhar, PAN, VoterID (111)
sum(IDs == 111) #241
#Aadhar, PAN, Driving (1011)
sum(IDs == 1011) #51
#Aadhar, VoterID, Driving (1101)
sum(IDs == 1101) #12
#Aadhar, PAN, Passport (10011)
sum(IDs == 10011) #11
#Aadhar, Driving, Passport (11001)
sum(IDs == 11001) #1
#PAN, VoterID, Driving (1110)
sum(IDs == 1110) #10
#PAN, VoterID, Passport (10110)
sum(IDs == 10110) #1

#FOUR IDs
#Aadhar, PAN, VoterID, Driving Flag (1111)
sum(IDs == 1111) #3
#Aadhar, PAN, VoterID, Passport(10111)
sum(IDs == 10111) #1
data[IDs == 10111,] #Please just show us that you can drive

#Correlation between ID forms
cor(data$Aadhar_flag, data$PAN_flag)
cor(data$Aadhar_flag, data$VoterID_flag)
cor(data$Aadhar_flag, data$Driving_flag)
cor(data$Aadhar_flag, data$Passport_flag)
cor(data$PAN_flag, data$VoterID_flag)
cor(data$PAN_flag, data$Driving_flag)
cor(data$PAN_flag, data$Passport_flag)
cor(data$VoterID_flag, data$Driving_flag)
cor(data$VoterID_flag, data$Passport_flag)
cor(data$Driving_flag, data$Passport_flag)

#Making age categories
data$Age_cat <- cut(data$Age,
                    
                  breaks=c(20, 30, 40, 50, 60, 75),
                  labels=c('Young Adult', 'Adult', 'Middle Adult', 'Older Adult', "Senior"))

#Chi Squared Test btwn. Age Categories and ID Types
tabs <- table(data$Age_cat[IDs %in% c(1, 10, 1000, 100, 10000, 11, 110, 101, 1001, 111)], data$IDs[IDs %in% c(1, 10, 1000, 100, 10000, 11, 110, 101, 1001, 111)])
chisq.test(tabs)
stdres <- chisq.test(tabs)$stdres # standardized residuals

#Chi-squared test btwn. # of IDs used and Default Rate
forms <- table(rowSums(data[15:19])[rowSums(data[15:19]) < 4], data$loan_default[rowSums(data[15:19]) < 4])
chisq.test(forms)
rowSums(data[15:19])[rowSums(data[15:19]) < 4]
stdres <- chisq.test(forms)$stdres

library(ggplot2)
#Figure 8
prop.table(table(data$Age_cat))
ggplot(data = data) + 
  geom_bar(aes(x = as.factor(IDs), fill = factor(loan_default)), position = 'dodge')+ 
  labs(title="Figure8: ID Types & Loan Default")+ xlab("IDs Presented") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Figure 9
smaller_data <- data[IDs != 1,]
ggplot(data = smaller_data) + 
  geom_bar(aes(x = as.factor(IDs), fill = factor(loan_default)), position = 'dodge')+ 
  labs(title="Figure9: ID Types & Loan Default Subset")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
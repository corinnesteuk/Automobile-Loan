library(VGAM)
library(dplyr)
library(devtools)
library(formattable)
library(ggplot2)
data <- read_csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Final.csv")
head(data)

#Analyzing Age variable
range(data$Age)

#Creating a copy of Data
df <- data

#Creating a new column in data to categorize the Age in to 5 ordinal categories
df$Age_cat <- cut(df$Age,
    breaks=c(20, 30, 40, 50, 60, 75),
    labels=c('Young Adult', 'Adult', 'Middle Adult', 'Older Adult', "Senior"))
head(df) 
count <- c(rep(1, length(df$Age)))
df$count <- count

#Ordinal Response from Ch.6 - Risk description is response, Age_cat and employment type are explanatory 
# I changed the values for every combination to make the group data (see Table1 in report)
sum(df[which(df$Age_cat== 'Senior' & df$risk_description=='Very High Risk'
             &is.na(df$Employment.Type)), 40])
#Finding totals to check 
sum(is.na(df$Employment.Type))
#7661

sum(df[which(df$Age_cat=='Senior' & df$risk_description=='Very High Risk'), 40])
sal_ya <- c("sal", "ya", 4875, 20604, 988, 652, 341)
sal_a <- c("sal", "a", 9630, 21631, 2175, 1820, 1426)
sal_ma <- c("sal", "ma", 5884, 11251, 1281, 1395, 1237)
sal_oa <- c("sal", "oa", 3135, 5439, 723, 844, 635)
sal_s <- c("sal", "s", 568, 930, 136, 155, 103)
se_ya <- c("se", "ya", 3407, 20682, 811, 607, 304)
se_a <- c("se", "a", 9812, 30461, 2926, 2678, 1980)
se_ma <- c("se", "ma", 8041, 19022, 2296, 2471, 2311)
se_oa <- c("se", "oa", 3595, 9271, 1081, 1190, 1180)
se_s <- c("se", "s", 766, 2042, 220, 237, 244)
na_ya <- c("na", "ya", 691, 4810, 181, 158, 113)
na_a <- c("na", "a", 198, 589, 50, 58, 19)
na_ma <- c("na", "ma", 107, 326, 36, 22, 17)
na_oa <- c("na", "oa", 48, 140, 19, 16, 5)
na_s <- c("na", "s", 13, 35, 4, 4, 2)

group_dat <- data.frame(rbind(sal_ya, sal_a, sal_ma, sal_oa, sal_s, se_ya, se_a, se_ma, se_oa, 
      se_s,na_ya, na_a, na_ma, na_oa, na_s ))
colnames(group_dat)<- c("Employment","AgeGroup","VLR", "LR", "MR", "HR", "VHR")
rownames(group_dat) <- 1:nrow(group_dat) 
group_dat$VLR <- as.numeric(group_dat$VLR)
group_dat$LR <- as.numeric(group_dat$LR)
group_dat$MR <- as.numeric(group_dat$MR)
group_dat$HR <- as.numeric(group_dat$HR)
group_dat$VHR <- as.numeric(group_dat$VHR)
group_dat$AgeGroup <- factor(c("s", "oa", "ma", "a", "ya"), levels = c("s", "oa", "ma", "a", "ya"))

group_dat$AgeGroup
#analyzing model (setting parallel = TRUE)
fit_p <- vglm(cbind(VLR, LR, MR, HR, VHR) ~ Employment + AgeGroup, family = cumulative(parallel = TRUE),
            data = group_dat)
summary(fit_p)
fit0 <- vglm(cbind(VLR, LR, MR, HR, VHR) ~ 1, family = cumulative,
            data = group_dat)

#Likelihood Ratio Test
lrtest(fit_p, fit0)

#Estimated Response Category Probabilities --> Plot this?
probs <- data.frame(group_dat$Employment, group_dat$AgeGroup, fitted(fit_p))
probs
emp <- c(rep('sal', 5), rep('se', 5), rep('na', 5))
formattable(probs)
probs$VLR
probs1 <- data.frame(rbind(cbind(rep("VLR", 15), as.double(probs$VLR),emp) , cbind(rep("LR", 15), as.double(probs$LR), emp), cbind(rep("MR", 15), as.double(probs$MR), emp), cbind(rep("HR", 15), as.double(probs$HR),emp), cbind(rep("VHR", 15), as.double(probs$VHR), emp)))
probs1 <- dplyr::rename(probs1, est.prob = V2)
probs1 <- dplyr::rename(probs1, risk = V1)
probs1$est.prob <- as.numeric(probs1$est.prob)
probs1
probs1$AgeGroup <- rep(c("Senior", "Older Adult", "Middle Adult", "Adult", "Young Adult"), 15)
probs1
y <- seq(0, .8, .05)
#Figure 4: Estimated Category Probabilities
ggplot(data = probs1) + 
  geom_point(aes(AgeGroup, est.prob, colour = factor(risk), shape  = emp))+
  scale_y_continuous(name = "Est. Probability", breaks=y)+
  labs(shape="Employment Type", colour="Risk")
       
#Log Odds Ratio
#salaried vs. unknown
exp(0.220271)
#young adult vs. senior
1/exp(-0.080214)

#Wald Stat for Age Group = Adult (a)
z_2 = (-0.107901/0.007979 )^2
#Profile Likelihood Confidence Interval 
exp(confint(fit_p, method = "profile"))




#Creating Visual table for report (changing explanatory value names)
gd <- data.frame(group_dat)
gd
gd$AgeGroup <- replace(gd$AgeGroup, gd$AgeGroup == 'ya', 'Young Adult')
gd$AgeGroup <- replace(gd$AgeGroup, gd$AgeGroup == 'a', 'Adult')
gd$AgeGroup <- replace(gd$AgeGroup, gd$AgeGroup == 'ma', 'Middle Adult')
gd$AgeGroup <- replace(gd$AgeGroup, gd$AgeGroup == 'oa', 'Older Adult')
gd$AgeGroup <- replace(gd$AgeGroup, gd$AgeGroup == 's', 'Senior')
gd$Employment <- replace(gd$Employment, gd$Employment == 'sal', 'Salaried')
gd$Employment <- replace(gd$Employment, gd$Employment == 'se', 'Self Employed')
gd$Employment <- replace(gd$Employment, gd$Employment == 'na', 'Unknown')
gd <-gd %>%
  rowwise() %>%
  mutate(
    RowTotals = sum(c(VLR, LR, MR, HR, VHR))
  )
col_totals <- c("Column", "Totals", sum(gd$VLR), sum(gd$LR), 
                sum(gd$MR), sum(gd$HR), sum(gd$VHR),sum(gd$RowTotals))
gd <- rbind(gd, col_totals)
formattable(gd)



       
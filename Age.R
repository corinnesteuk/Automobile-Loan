library(VGAM)
library(dplyr)
library(devtools)
library(formattable)
library(ggplot2)
library(rethinker)
library(tidyverse)
library(ggExtra)
data <- read_csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Final.csv")
head(data)

#Analyzing Age variable
range(data$Age)


#Creating a copy of Data
df <- data
df
#Creating a new column in data to categorize the Age in to 5 ordinal categories
df$Age_cat <- cut(df$Age,
    breaks=c(20, 30, 40, 50, 60, 75),
    labels=c('Young Adult', 'Adult', 'Middle Adult', 'Older Adult', "Senior"))
head(df) 
count <- c(rep(1, length(df$Age)))
df$count <- count
df
#Finding totals to check 
sum(is.na(df$Employment.Type))
#7661

sum(df[which(df$Age_cat=='Senior' & df$risk_description=='Very High Risk'), 41])
#Ordinal Response from Ch.6 - Risk description is response, Age_cat and employment type are explanatory 
# I changed the values for every combination to make the group data (see Table1 in report)
sum(df[which(df$Age_cat== 'Young Adult' & df$risk_description=='Very Low Risk'
             &is.na(df$Employment.Type)), 41])

sum(df[which(df$Age_cat== 'Young Adult' & df$risk_description=='Very Low Risk'
             & df$Employment.Type== 'Self employed'), 41])

sal_ya <- c("sal", "ya", 19627, 5823, 1005, 664, 341)
sal_a <- c("sal", "a", 23620, 7586, 2198, 1852, 1426)
sal_ma <- c("sal", "ma", 13005, 4087, 1276, 1443, 1237)
sal_oa <- c("sal", "oa", 6436, 2103, 723, 869, 636)
sal_s <- c("sal", "s", 1115, 374, 141, 159, 103)
se_ya <- c("se", "ya", 18186, 5886, 827, 607, 305)
se_a <- c("se", "a", 29822, 10381, 2959, 2715, 1980)
se_ma <- c("se", "ma", 20158, 6822, 2304, 2545, 2312)
se_oa <- c("se", "oa", 9506, 3311, 1095, 1224, 1181)
se_s <- c("se", "s", 2113, 679, 2217, 256, 244)
na_ya <- c("na", "ya", 4228, 1261, 188, 163, 113)
na_a <- c("na", "a", 624, 160, 52, 58, 20)
na_ma <- c("na", "ma", 323, 109, 36, 23, 17)
na_oa <- c("na", "oa", 144, 43, 20, 16, 5)
na_s <- c("na", "s", 39, 9, 4, 4, 2)

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
  labs(shape="Employment Type", colour="Risk")+
  ggtitle("Figure4: Estimated Category Probabilities")

summary(fit_p)   
#Log Odds Ratio
#salaried vs. unknown
exp(0.0002785)
#salaried vs. self-employed
exp(0.0002785 - -0.1066360 )
#adult vs. senior
1/exp(-0.6403860)
#young adult vs. senior
1/exp(-1.2517745)

#Wald Stat for Age Group = Young Adult (ya)
z_2 = (-1.2517745/0.0105571 )^2
#Profile Likelihood Confidence Interval 
exp(confint(fit_p, method = "profile"))

#Residuals
r <- residuals(fit_p)
y1 <- r[1:15]
y2<- r[16:30]
y3 <- r[31:45]
y4 <- r[46:60]
probs_r <- (cbind(probs, y1, y2, y3, y4))
ggplot(data = probs_r) + 
  geom_point(aes(group_dat.AgeGroup , y1, color = 'VLR', shape = group_dat$Employment))+
  geom_point(aes(group_dat.AgeGroup , y2, color = 'LR'))+
  geom_point(aes(group_dat.AgeGroup , y3, color = 'MR'))+
  geom_point(aes(group_dat.AgeGroup , y4, color = 'HR'))+
  scale_y_continuous(name = "Residuals")+
  scale_x_discrete(name = "AgeGroup", labels = c('Senior', 'Older Adult', "Middle Adult", 'Adult', 'Young Adult'))+
  labs(color = "Risk", shape = 'Employment')+
  ggtitle("Figure5: AgeResiduals")


probs_r
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

gd
col_totals <- c("Column", "Totals", sum(gd$VLR), sum(gd$LR), 
                sum(gd$MR), sum(gd$HR), sum(gd$VHR),sum(gd$RowTotals))
gd <- rbind(gd, col_totals)
formattable(gd)



       
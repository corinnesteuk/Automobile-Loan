library(dplyr)
full_data <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-Clean.csv")
credit_score_data <- read.csv("/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CleanCredit.csv")

#renaming credit and risk rating columns in full data
full_data <- rename(full_data, risk_rating = PERFORM_CNS.SCORE.DESCRIPTION)
full_data <- rename(full_data, credit = PERFORM_CNS.SCORE)

#data frame with just credit score & risk rating 
credit_risk <- data_frame(full_data$credit, full_data$risk_rating)
credit_risk <- rename(credit_risk, risk_rating = `full_data$risk_rating`)
credit_risk <- rename(credit_risk, credit = `full_data$credit`)
credit_risk <- credit_risk[credit_risk$credit>50, ] 

#separating the letter from the risk description
col <- seq(1, length(credit_risk$risk_rating), 1)
col1 <- seq(1, length(credit_risk$risk_rating), 1)
i <- 1
while (i < length(credit_risk$risk_rating)){
  new <- as.list(strsplit(credit_risk$risk_rating[i], split = "-")[[1]])
  col[i] <- new[1]
  col1[i] <- new[2]
  i=i+1
}
#appending this to credit_risk
credit_risk$letter <- unlist(col)
credit_risk$description <- unlist(col1)
head(credit_risk)

#finding max and min range of the credit risk ratings
letter<- as.list(LETTERS[1:13])
risk_min <- c(seq(1, 13, 1))
risk_max <- c(seq(1, 13, 1))
i <- 1
while (i<=13){
  x <- credit_risk[credit_risk$letter==letter[i], ]
  risk_min[i] <- range(x$credit)[1]
  risk_max[i] <- range(x$credit)[2]
  i = i+1                    
}

#creating a data frame with the letter, minimum risk, maximum risk
df <- data.frame(unlist(letter), risk_min, risk_max)
df <- rename(df, letter = unlist.letter.)
df
df$letter[1]

#appending this to the credit_score_data in another column
#testing letters with descriptions
credit_risk[credit_risk$letter=='N', ]
desc <- c(rep('Very Low Risk', 4), rep('Low Risk', 3), rep('Medium Risk', 2),
          rep('High Risk', 2), rep('Very High Risk', 2))

df$description <- desc     
df

#creating a vector with the letter risk score for all entries in the credit_score_data
#this has all the known and imputed credit scores
vec_l <- seq(1, length(credit_score_data$credit))
vec_d <- seq(1, length(credit_score_data$credit))
i <- 1
while (i <= length(credit_score_data$credit)){
  j <- 1
  while (j <= length(df$risk_min)){
    if (credit_score_data$credit[i] >= as.integer(df$risk_min[j]) 
        & credit_score_data$credit[i] <= as.integer(df$risk_max[j])){
      vec_l[i] <- letter[j]
      vec_d[i] <- df$description[j]
      j <- j+1
  }else{
    j <- j+1
  }
  }
  i <-i+1
  
}
vec_l
vec_d
#appending this to the credit_score_data in another column
credit_score_data$risk_rating <- unlist(vec_l)
credit_score_data$risk_description <- vec_d

head(credit_risk)
head(credit_score_data)


write.csv(credit_score_data,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CreditModel.csv", row.names = FALSE)
write.csv(df,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CreditCheatSheet.csv", row.names = FALSE)
write.csv(credit_risk,"/Users/corinnesteuk/Documents/STAT310/VehicleLoanDefault-CreditRisk.csv", row.names = FALSE)





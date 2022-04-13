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

#creating a vector with the letter risk score for all entries in the credit_score_data
#this has all the known and imputed credit scores
vec <- seq(1, length(credit_score_data$credit))
i <- 1
while (i <= length(credit_score_data$credit)){
  j <- 1
  while (j <= length(df$risk_min)){
    if (credit_score_data$credit[i] >= as.integer(df$risk_min[j]) 
        & credit_score_data$credit[i] <= as.integer(df$risk_max[j])){
      vec[i] <- letter[j]
      j <- j+1
  }else{
    j <- j+1
  }
  }
  i <-i+1
  
}
vec
#appending this to the credit_score_data in another column
credit_score_data$risk_rating <- vec

#now I want to append the risk description that corresponds to the letter
head(credit_risk)
v2 <- seq(1, 13, 1)
while (i<=13){
  desc <- credit_risk$description[select(where(credit_risk$letter == letter[i]))]
  v2[i] <- desc
  i <- i+1
}

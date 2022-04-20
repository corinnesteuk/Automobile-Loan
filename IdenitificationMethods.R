ggplot(data = data, aes(x = data$PERFORM_CNS.SCORE.DESCRIPTION[PERFORM_CNS.SCORE.DESCRIPTION != "No Bureau History Available"])) +
  geom_bar()
summary(data$PERFORM_CNS.SCORE.DESCRIPTION)
data[15:19]

rowSums(data[15:19])
summary(as.factor(rowSums(data[15:19])))/dim(data[15:19])[1]
prop.table(table(rowSums(data[15:19]), data$loan_default), margin=1)
table()
sum(data$Passport_flag)
table(data[15:19], data$loan_default)
data[rowSums(data[15:19]) == 1]
sum(is.na(data$Employment.Type))/dim(data[15:19])[1]
sum(is.na(data))

gss <- read.csv(file.choose())
print(gss)

Education <- gss$Education
DeadthPenalty <- gss$DeathPenalty
table(Education)
table(DeadthPenalty)

View((gss))

index <- which(is.na(Education) | is.na(DeadthPenalty))
Education <- Education[-index]
DeadthPenalty <- DeadthPenalty[-index]

chisq.test(Education , DeadthPenalty)
print(Education)
print(DeadthPenalty)
table(Education , DeadthPenalty)


chisq <- function(obs)
{
  Expected <- outer(rowSums(obs),colSums(obs))/sum(obs)
  sum((obs-Expected)^2/Expected)
}

chisq(table(Education,DeadthPenalty))

observed <- chisq(table(Education,DeadthPenalty))
observed


#sample loop
set.seed(0)
B <- 100000-1
result <- numeric(B)
for (i in 1:B)
{
  DP.permutaion <-sample(DeadthPenalty)
  GSS.table <- table(Education , DP.permutaion)
  result[i] <- chisq(GSS.table)
}

hist(result , xlab='chi-square statistic' , main='Distribution of chi-square')
abline(v=observed,col='blue',lty=5)

P_value <- (sum(result >= observed)+1)/(B+1)
print(P_value)
#ที่เราเห็นๆใน python EDA chi-square test ยิ่งเลขเยอะ ยิ่งมีความสัมพันเพราะ P-value มันน้อย


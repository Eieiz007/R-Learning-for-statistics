library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


df <- read_csv("D:/Datasets for learning/airline delay causes.csv")
df <- df %>% select(-...22)
head(df,10)

str(df)
#View(df)
options(max.print = 10000)
table(df$year) #value_counts 2003-2020
table(df$carrier) #value_counts 

#Clean data 
#chr > factor 
df$carrier <- as.factor(df$carrier)
df$carrier_name <- as.factor(df$carrier_name)

df$airport <- as.factor(df$airport)
df$airport_name <- as.factor(df$airport_name)

#delete NA
summary(df)
na_counts <- colSums(is.na(df))
print(na_counts)

df <- na.omit(df)
print(df)
summary(df)
na_counts <- colSums(is.na(df))
print(na_counts)

# carrier and carrier_name ต้องมีจำนวน value_counts เท่ากัน
print(length(unique(df$carrier)))
print(length(unique(df$carrier_name)))
print(length(unique(df$airport))) #ไม่มีปัญหา
print(length(unique(df$airport_name)))

length(unique(df$carrier)) - 
  length(unique(df$carrier_name))

# Group by carrier and carrier_name and count occurrences
df_summary <- df %>%
  group_by(carrier, carrier_name) %>%
  summarize(count = n()) %>%
  ungroup()

# Filter for cases where carrier and carrier_name don't match
df_summary <- df_summary %>%
  filter(n_distinct(carrier) != n_distinct(carrier_name))

# Print the results
print(df_summary, n = 100)
#ลบ columns ทิ้ง 

# 9E DH EV OH MQ
df_summary %>% filter(carrier %in% c("9E", "DH","EV","OH","MQ"))
df <- df %>%
  mutate(
    carrier = case_when(
      carrier_name == "Endeavor Air Inc." & carrier == "9E" ~ "9E1",
      carrier_name == "Pinnacle Airlines Inc." & carrier == "9E" ~ "9E2",
      carrier_name == "Atlantic Coast Airlines" & carrier == "DH" ~ "DH1",
      carrier_name == "Independence Air" & carrier == "DH" ~ "DH2",
      carrier_name == "Atlantic Southeast Airlines" & carrier == "EV" ~ "EV1",
      carrier_name == "ExpressJet Airlines Inc." & carrier == "EV" ~ "EV2",
      carrier_name == "ExpressJet Airlines LLC" & carrier == "EV" ~ "EV3",
      carrier_name == "Comair Inc." & carrier == "OH" ~ "OH1",
      carrier_name == "PSA Airlines Inc." & carrier == "OH" ~ "OH2",
      carrier_name == "American Eagle Airlines Inc." & carrier == "MQ" ~ "MQ1",
      carrier_name == "Envoy Air" & carrier == "MQ" ~ "MQ2",
      TRUE ~ carrier
    )
  )

df %>% filter(carrier %in% c("9E", "DH","EV","OH","MQ"))
print(length(unique(df$carrier)))
print(length(unique(df$carrier_name)))

grouped_data <- df %>%
  group_by(carrier_name) %>%
  summarize(
    carriers = paste(unique(carrier), collapse = ", ") # รวม carrier ที่ต่างกันในแต่ละกลุ่ม
  )

# แสดงผลลัพธ์
print(grouped_data , n=1000)
df <- df %>%
  mutate(
    carrier = case_when(
      carrier_name == "ExpressJet Airlines Inc." & carrier == "XE" ~ "EV2",
      carrier_name == "ExpressJet Airlines Inc." & carrier == "RU" ~ "EV2",
      TRUE ~ carrier
    )
  )
print(length(unique(df$carrier)))
print(length(unique(df$carrier_name)))

#ไม่มีแถวที่ซ้ำ
dup_rows <- df[duplicated(df), ]
print(dup_rows)
#--------------------------------------------------------------------
#carrier_ct weather_ct nas_ct security_ct late_aircraft_ct 
#floating เมื่อสังเกต #carrier_ct weather_ct nas_ct security_ct late_aircraft_ct 
# นำมารวมกันจะได้arr_del15 แต่ว่าถ้า มากกว่าเท่ากับ 0.4 ปัดขึ้น
test1 <- tail(select(df, arr_del15,carrier_ct,weather_ct ,nas_ct, security_ct ,late_aircraft_ct),30)
print(test1)
test1$sum_test1 <- test1$carrier_ct + test1$weather_ct + test1$nas_ct + test1$security_ct + test1$late_aircraft_ct
test1$sum_test1


#จริงดั้งนั้นเราจะแปลง ปัดเลขขึ้นให้หมด

df_processed <- test1

# ปรับแต่งค่าในแต่ละคอลัมน์ตามเงื่อนไขที่กำหนด
df_processed$carrier_ct <- as.integer(ifelse(test1$carrier_ct >= 0.4, ceiling(test1$carrier_ct), test1$carrier_ct))
df_processed$weather_ct <- as.integer(ifelse(test1$weather_ct >= 0.4, ceiling(test1$weather_ct), test1$weather_ct))
df_processed$nas_ct <- as.integer(ifelse(test1$nas_ct >= 0.4, ceiling(test1$nas_ct), test1$nas_ct))
df_processed$security_ct <- as.integer(ifelse(test1$security_ct >= 0.4, ceiling(test1$security_ct), test1$security_ct))
df_processed$late_aircraft_ct <- as.integer(ifelse(test1$late_aircraft_ct >= 0.4, ceiling(test1$late_aircraft_ct), test1$late_aircraft_ct))

# แสดงผลลัพธ์
print(df_processed)

df_processed$sum_test2 <- df_processed$carrier_ct + df_processed$weather_ct + df_processed$nas_ct + df_processed$security_ct + df_processed$late_aircraft_ct
df_processed$sum_test2
rm(df_summary,dup_rows,grouped_data,na_counts)
#---------------------------------------------------------------------#
#Test QQ-plot learning 
df$year <- as.factor(df$year)
summary(df$carrier)
#format : num , categorical , statistic 
tapply(df$arr_flights, df$carrier, mean)
AQ_B6 <- df[df$carrier == 'AQ' | df$carrier == 'B6' , ]
AQ <- US_B6[US_B6$carrier == "AQ",]
B6 <- US_B6[US_B6$carrier == "B6",]

qqnorm(AQ$arr_flights)
qqline(AQ$arr_flights)
#shapiro.test(DL_qqtest_arr_flight)
set.seed(1) # For reproducibility
sample <- sample(AQ$arr_flights, 5000)
shapiro.test(sample)
#ต้องทำแหละ permutation test

#step 1 : Observed
mean(AQ$arr_flights)
mean(B6$arr_flights)
observed <- mean(AQ$arr_flights) - mean(B6$arr_flights)
observed

set.seed(1)
#Pool data
B <- 99999
result <- numeric(B)
for (i in 1:B) {
  index <- sample(10780, size = 253, replace = FALSE)
  result[i] <- mean(AQ_B6$arr_flights[index]) - mean(AQ_B6$arr_flights[-index])
}

hist(result, xlab = expression(bar(x)[AQ] - bar(x)[B6]),
     main = 'Permutation distribution for flight delays')
abline(v = observed, col = "blue", lty = 5)

P_value <- sum(result >= observed)+1/(B+1)
print(P_value)

#---------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv(file.choose())
head(df,5)
str(df)
view(df)
df$Type.1 <- as.factor(df$Type.1)
df$Type.2 <- as.factor(df$Type.2)
df$Legendary <- as.factor(df$Legendary)
dim(df)
summary(df)
unique(df$Legendary)

df[,5:11]
plot(df[,5:11])
plot(df[,8] , df[,10])

boxplot(df[,7] ~ df[,3] , main='Test')
boxplot(df$Attack ~ df$Type.1) #ต้องสลับแกน xy 

df %>% filter(df$Type.1 == 'Fire' | df$Type.1 == 'Water')
subset1 <- df[df$Type.1 == 'Fire' | df$Type.1 == 'Water' , ]
str(subset1)
#อย่าลืมแปลง factor ใหม่อีกรอบ
subset1$Type.1 <- factor(subset1$Type.1)
plot(subset1[,'Type.1'], subset1[,'Attack'],
     xlab = 'Type', ylab = 'Attack' , main = 'Fire vs Water (Attack)')

#จากภาพมัน ก็ใกล้ๆกัน ความต่างมันพอมั้ย ผลลัพธ์ของ 2 กลุ่มนี้มันต่างกันอย่างมีนัยสำคัญ
#ความแตกต่างมัน มีนัยสำคัญมั้ย
?t.test
Fire <- df[df$Type.1 == 'Fire' , 'Attack']
Water <- df[df$Type.1 == 'Water' , 'Attack']

t.test(Fire , Water) #แตกต่างอย่างมีนัยสำคัญว่า เพราะ p-value < alphaิ rejection  H0 
#สังเกต CI ไม่คุม 0 ก็แสดงว่ามันแตกต่างแน่ๆ
#Welch Two Sample t-test คือ Sigma 1 not equal Sigma 2 
t.test(Fire , Water , var.equal = TRUE) #Sigma 1 == Sigma2

t.test(Fire , Water ,alternative = 'less')

#วิธี check ว่า ข้อมูลถูกสุ่มหยิบมาจาก Population ที่เป็นการแจงแจงปกติ จริงๆหรือป่าว
qqnorm(Fire)
qqline(Fire)
shapiro.test(Fire)

qqnorm(Water)
qqline(Water)
shapiro.test(Water)

qqnorm(rnorm(300)
qqline(rnorm(300)
shapiro.test(rnorm(300)
             #shapiro ตัวเลขที่เช็คว่าข้อมูลไม่ได้มาจาก ปชก. 
#สมมุติเราเห็นว่า mean มันต่างกัน แล้กวมันต่างกันจริงๆ หรือมันต่างกันแบบบังเอิญ
             
             

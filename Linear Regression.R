# สร้าง dataframe ด้วยข้อมูลที่แสดงในภาพ
trip <- data.frame(
  Miles = c(89, 66, 78, 111, 44, 77, 80, 66, 109, 76),
  NumDeli = c(4, 1, 3, 6, 1, 3, 3, 2, 5, 3),
  GasPrice = c(3.84, 3.19, 3.78, 3.89, 3.57, 3.57, 3.03, 3.51, 3.54, 3.25),
  Time = c(7.0, 5.4, 6.6, 7.4, 4.8, 6.4, 7.0, 5.6, 7.3, 6.4)
)

# แสดง dataframe
print(trip)
cor(trip)
plot(trip$Miles , trip$NumDeli , pch=20)

library(PerformanceAnalytics)
chart.Correlation(trip)



y <- trip$Time
x <- trip$Miles
# lm Y 0ะขึ้นก่อน 
lm(Time ~ Miles , data=trip)
plot(x ,y , pch=20 , main='Plot of Miles versus Time ')
abline(3.70189 , 0.03628 ,col = 'red' , lwd = 3)

#Time_hat <- 3.70189 + 0.03628(Miles)
#ค่าประมาณ y ที่ใช้ใน data เบต้า0hat ถ้าไม่มี เบต้า1hat ก็คือ 0 หน่วย
#ค่าประมาณเวลาที่ใช้ใน trip 3.70189 ถ้าไม่มีเดินทางก็คือ 0 Miles

#ถ้าตัวแปร x เปลี่ยนไป 1 หน่วยเราคาดหวังว่าเวลาจะเปลี่ยนไปโดยประมาณ เบต้า1hat หน่วย
#When ระยะทางเพิ่มขึ้น 1 Mile เราคาดว่าหวังว่าเวลาจะเปลี่ยนไป 0.04 ชั่วโมงโดยประมาณ 
summary(lm(Time ~ Miles , data=trip))
summary(lm(Time ~ Miles , data=trip))$r.squared
#r^2 = 0.65
#65% of variation y  is associated to the existence  of the  distance  variable 
#65% ของการแปรผันในตัวแปร y มันสามารถอธิบายได้ด้วย x 
#model สามารถอธิบายความแปรปรวนได้ 65%
#ใส่ squared root ได้ 
summary(lm(Time ~ Miles , data=trip))$r.squared
confint(lm(Time ~ Miles , data=trip))


#Mitiple linear regression 
lm2 <- lm(Time  ~ Miles + NumDeli,data=trip )
summary(lm2)
# Time = 3.73216 + 0.02622(Miles) + 0.18404(NumDeli)
#ตีความกัน 
#Intercept ค่าประมาณของ y(time) เป็น 3.73216 เมื่อไม่ได้ขึ้นอยู่กับ x ใดๆเลย
#Miles เมื่อ Miles(x1) เพิ่มขึ้น 1 หน่วยคาดหวังว่าจะประมาณ y(time) ได้ 0.02622 เมื่อกำหนดให้ NumDeli(x2) ไม่เปลี่ยนแปลง
#NumDeli เมื่อ NumDeli(x2) เพิ่มขึ้น 1 หน่วยคาดหวังว่าจะประมาณ y(time) ได้ 0.18404 หน่วยเมื่อกำหนดให้ Miles(x1) ไม่เปลี่ยนแปลง
summary(lm2)$r.squared
# 87.14% ของการแปรผัน y สามารถอธิบายได้ด้วย Miles และ NumDeli
#เมื่อ มี Mutiple เพิ่มขึ้น R^2 จะเพิ่มขึ้นเรื่อยๆ เพราะเพิ่ม bata RSS จะลดลง ทำให้ R^2 สูงขึ้น


lm1 <- lm(Time ~ Miles, data = trip)
lm2 <- lm(Time ~ NumDeli, data = trip)
lm3 <- lm(Time ~ GasPrice, data = trip)
lm4 <- lm(Time ~ Miles + NumDeli, data = trip)
lm5 <- lm(Time ~ Miles + GasPrice, data = trip)
lm6 <- lm(Time ~ NumDeli + GasPrice, data = trip)
lm7 <- lm(Time ~ Miles + NumDeli + GasPrice, data = trip)

summary(lm1)$adj.r.square
summary(lm2)$adj.r.square
summary(lm3)$adj.r.square
summary(lm4)$adj.r.square
summary(lm5)$adj.r.square
summary(lm6)$adj.r.square
summary(lm7)$adj.r.square
#เริ่มแรกอาจจะดู R^2,adj.R^2
#ปรากฏว่า lm 3 ใช้ไม่ได้ และlm 6 สูงสุด

summary(lm6) 
#ตัวแปร GasPrice มันไม่ signif. ลองตัดออก
summary(lm2) 
#แต่ R^2 ลดลงนะ ก็ไม่เป็นไรมันไม่ได้ต่างกันมาก 
summary(lm1) 
#อันนี้ดีกว่านะ ก็แสดงว่า Datasets ชุดนีได้ มี 2 model ที่ทำได้ดีไม่แพ้กัน 
#มันก็มี Library มาช่วยด้วยนะ ในการเลือก model
library(leaps)
b <- regsubsets(Time ~ Miles + NumDeli + GasPrice, data = trip)
bestsubset <- summary(b)
bestsubset$which
#ถ้า n ตัวแปร ตัวแปรสูงสุดของ n ต้องไปดูเอง 



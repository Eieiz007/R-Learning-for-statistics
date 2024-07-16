#Regression Categorical
library(alr4)
data(UN11)
head(UN11)
str(UN11)
library(car) #What ?


boxplot(lifeExpF ~ group , data = UN11 , main='Title')

#Encoding 
levels(UN11$group)
U1 = with(UN11 , (group == levels(group)[1]) + 0)
U2 = with(UN11 , (group == levels(group)[2]) + 0)
U3 = with(UN11 , (group == levels(group)[3]) + 0)
head(data.frame(group = UN11$group , U1 , U2 ,U3 ) , 10)

UN11$U1 = U1
UN11$U2 = U2
UN11$U3 = U3

lm(lifeExpF ~ U2 + U3 , data= UN11) #ไม่นิยม
lm(lifeExpF ~ group , data = UN11) #เอา U1 เป็น baseline 

UN11$group
summary(lm(lifeExpF ~ group , data = UN11))

#Categorical + Continue 
library(ggplot2)
ggplot(UN11, aes(y = lifeExpF, x = log(ppgdp), color = group, pch = group)) + 
  geom_point()

#building model (separate lines) slope ต่างกัน
lmodi <- lm(lifeExpF ~ log(ppgdp)*group , data=UN11)
summary(lmodi)
coef(lmodi)

ggplot(UN11, aes(y = lifeExpF, x = log(ppgdp), color = group, pch = group)) + 
  geom_point() + geom_smooth(method = "lm") + theme_bw() + theme(legend.position = "top")

#builing model (parallel lines) slope เท่ากัน 
lmodi2 <- lm(lifeExpF ~ log(ppgdp) + group , data=UN11)
summary(lmodi2)
coef(lmodi2)

new_group <- as.numeric(UN11$group)
plot(lifeExpF ~ log(ppgdp) , data =UN11 , col=new_group , pch = new_group)
abline(49.529241 , 3.177320 , col=1)
abline(49.529241 -1.534683 , 3.177320 , col=2)
abline(49.529241 -12.170365 , 3.177320 , col=3)
legend('topleft' , legend = c("oecd" , "other" , "africa") , col = 1:3 ,pch = new_group , lty = 1)

library(HH)
UN11$ppgdp = log(UN11$ppgdp)
ancovaplot(lifeExpF ~ ppgdp + group  , data = UN11)

#anova F test 
anova(lmodi , lmodi2)




#Regression Diagnostics

library(faraway)
data("galapagos")
data("savings")
?galapagos



#model bulding 
lm1 <- lm(NS ~ Area + Elevation + DistSC + Anear + Area , data=galapagos)
summary(lm1)
plot(lm1 , which = 1)
abline(h=0)

lm2 <- lm(NS ~ sqrt(Area) + Elevation + DistSC + Anear + Area , data=galapagos)
summary(lm2)
plot(lm2 , which = 1)
abline(h=0)

par(mfrow = c(1,2))
plot(fitted(lm1) , residuals(lm1) ,xlab = "fitted" , ylab=" Residuals")
title("Original")
plot(fitted(lm2) , residuals(lm2) ,xlab = "fitted" , ylab=" Residuals")
title("Transformed")
par(mfrow = c(1,1))
#Normal
s1 <- lm(sr ~ pop15 + pop75 +  dpi +  ddpi , data = savings)
qqnorm(residuals(s1) , main ="qq plot of savings residuals")
qqPlot(residuals(s1) , main ="qq plot of savings residuals")
shapiro.test(residuals(s1))

#independent
data("globwarm")
g1 <- lm(nhtemp ~ wusa + jasper + westgreen + chesapeake + tornetrask + urals
         + mongolia + tasman , data=globwarm)

plot(residuals(g1) ~ year , na.omit(globwarm))
library(lmtest)
dwtest(g1)

#Influential observations 
plot(s1,which=4)
cooks.distance(s1)
summary(s1)


which(rownames(savings) == "Libya")
savings2 <- savings[-49,]
s1 <- lm(sr ~ pop15 + pop75 +  dpi +  ddpi , data = savings)
s2 <- lm(sr ~ pop15 + pop75 +  dpi +  ddpi , data = savings2)
summary(s1)
summary(s2)

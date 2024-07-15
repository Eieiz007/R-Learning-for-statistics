prostate <- read.csv(file.choose())

#แบ่ง DATA 
prostate.train <- prostate[prostate$train == TRUE , 1:9]
prostate.test <- prostate[prostate$train == FALSE , 1:9]


library(PerformanceAnalytics)
chart.Correlation(prostate[ , 1:9])

library(GGally)
ggcorr(prostate)

# train data
lmod1 <- lm(lpsa ~ .,data=prostate.train)
lmod2 <- lm(lpsa ~ . - gleason,data=prostate.train)
summary(lmod1)

#ทำไม p-value ถึงเปลี่ยน ระหว่างมี gleason กับไม่มี gleason 
#p-value ตอนแรกที่มี gleason มันมีทุก predictor ในการ train model 
#พอเอาออก model ก็จะ train ใน ชุด predictor นั้นๆ
#สรุปง่ายๆ ที่เปลี่ยนเพราะ model with respect to model ตัวนี้ที่ไม่มี gleason แล้ว 

#ข้ามวิธีตัดด้วย p-value 
#AIC backward , forward , stepwise 
library(MASS)
mod.int <- lm(lpsa ~ 1 , data = prostate.train) #ขอบล่าง
mod.full <- lm(lpsa ~ ., data = prostate.train) #ขอบบน

# forward selection 
stepAIC(mod.int , direction = 'forward' ,
        scope=list(lower=mod.int , upper=mod.full))

# backward selection 
stepAIC(mod.full , direction = 'backward' ,
        scope=list(lower=mod.int , upper=mod.full))

# stepwise selection 
stepAIC(mod.int , direction = 'both' ,
        scope=list(lower=mod.int , upper=mod.full))

#Bestsubset by minize RSS 
library(leaps)
b <- regsubsets(lpsa ~ . , data = prostate.train)
bestsubset <- summary(b)
bestsubset$which #Don't Belive 
bestsubset$rss
bestsubset$adjr2 #Good

dim(prostate.train)[1]
#AIC bestsubsets 
AIC <- (dim(prostate.train)[1])*log(bestsubset$rss/dim(prostate.train)[1]) + (2:9)*2
plot(AIC ~ I(1:length(AIC)) , pch = 20 ,
     main="Plot of AIC vs Number of Predictors used" , 
     xlab = "Number of predictors" , 
     ylab = "AIC score") 

#adjr2 for bestsubset 
plot(bestsubset$adjr2 ~ I(1:8),pch=20,
     ylab='Adj. R^2 score' ,
     xlab='Number of Predictors',
     main='Adj R^2 versus Number of predictors')

#MSE 
# Choose forward AIS model 
forward_model <- stepAIC(mod.int , direction = 'forward' ,
        scope=list(lower=mod.int , upper=mod.full))
forward_test <- predict.lm(forward_model , newdata = prostate.test)

mse_forward <- mean((forward_test - prostate.test$lpsa)^2)

# Choose backward AIS model 
backeward_model <- stepAIC(mod.full , direction = 'backward' ,
                         scope=list(lower=mod.int , upper=mod.full))
backeward_test <- predict.lm(backeward_model , newdata = prostate.test)

mse_backward <- mean((backeward_test - prostate.test$lpsa)^2)

#SUm  forward and backward (MSE)
cbind(mse_forward , mse_backward)
rbind(mse_forward , mse_backward)

#bestsubset MSE
bestsubset$which
#ค่อยโค้ดละกัน 



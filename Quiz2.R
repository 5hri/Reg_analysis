#Q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit = lm(y~x)
coef(fit)
summary(fit)$coefficients
#or do summary(fit), pvalue 0.053

#Q2
summary(fit)$sigma

#Q3
library(UsingR); data(mtcars);
mtcarswt = mtcars$wt - mean(mtcars$wt)
fit = lm(mtcars$mpg~mtcarswt) #fit2 <- lm(mpg ~ I(wt - mean(wt)) also works
coef(fit)
summary(fit)$coefficients

newdata <- data.frame(cbind(mtcars$mpg,mtcars$wt)
                      
p1 <- predict(fit, newdata, interval = ("confidence"))
p2 <- predict(fit, newdata, interval = ("prediction"))
plot(mtcars$wt, mtcars$mpg, frame=FALSE,xlab="wt",ylab="mpg",pch=21,col="black", bg="lightblue", cex=2)
abline(fit, lwd = 2)

sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]

#Q3 another way
x<-mtcars$wt
y<-mtcars$mpg
fit<-lm(y ~ x)

predict(fit,data.frame(x=mean(x)), interval="confidence")

p1<-predict(fit,data.frame(x), interval="confidence")
plot(x,y,xlab='Weight (1000lb)',ylab='MPG')
abline(fit,col="red")
lines(x,p1[,2],col="purple")
lines(x,p1[,3],col="purple")

#Q5
predict(fit,data.frame(x=3), interval="prediction")

#Q6
fit2<-lm(y~I(x/2))
tbl2<-summary(fit2)$coefficients
mn<-tbl2[2,1]      #mean is the estimated slope
std_err<-tbl2[2,2] #standard error
deg_fr<-fit2$df    #degree of freedom
#Two sides T-Tests
mn + c(-1,1) * qt(0.975,df=deg_fr) * std_err

par(mfrow=c(1,2))
plot(x,y)
abline(fit,col="red")
plot(x/2,y)
abline(fit2,col="red")

#Q7
summary(fit)$coefficients
fit3<-lm(y~I(x/100))
summary(fit3)$coefficients

#Q8
y = mtcars$mpg
x = mtcars$wt; x3= mtcars$wt +3;
fit = lm(y~x)
fit3 = lm(y~x3)
coef(fit)
coef(fit3)

#Q9
fit5<-lm(y ~ 1)
fit6<-lm(y ~ x - 1)
plot(x,y)

abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")
anova(fit)
anova(fit5)
 # The ratio is sum sq residual from fit / fit5 = 278/1126 = 0.2468

#Q10
sum(resid(fit))  #both intercept and slope
sum(resid(fit5)) #only intercept
sum(resid(fit6)) #only slope



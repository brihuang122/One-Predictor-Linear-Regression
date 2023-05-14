getwd()
wdir <- "C:/Users/huang/OneDrive/Documents/AMS 315"
setwd (wdir)

B <- read.csv('830410_PartB.csv', header = TRUE)
fit_B <- lm(y~x, data=B)
summary(fit_B) #r^2 = 0.3873
plot(B$y~B$x, main='Scatter : x~y', xlab= 'x', ylab = 'y', pch=20)
abline(fit_B, col = '#FF6699', lty="solid", lw= 3)
legend('topleft', legend ='Estimated Regression Line', lty="solid", lwd=3, col ="#FF6699")

#transform data
library(LambertW)
Btrans <- Gaussianize(B) #y gets transformed
fit_Btrans<- lm(y.X~ x.X, data=Btrans)
summary(fit_Btrans) #r^2=0.4038
plot(Btrans$y.X ~ Btrans$x.X, main='Scatter : x.X ~ y.x', xlab='x.X', ylab='y.x', pch=20)
abline(fit_Btrans, col = '#FF6699', lty ="solid", lwd=3)
legend("topleft", legend = 'Estimated Regression Line', lty="solid", lwd=3, col='#FF6699')

#bin data
groups <- cut (Btrans$x.X, breaks=140) #140 groups
table(groups)

x_ave <- ave(Btrans$x.X, groups)

databin <- data.frame(x=x_ave, y=Btrans$y.X)
summary (databin)
data_bin

library(alr3)
fit_data_bin <- lm(y ~ x, data=data_bin)
pureErrorAnova(fit_data_bin) #p value = 0.0553
#higher than 0. 05 SO, upon transforming our data, there is not significant Lack of Fit in our regressi ion model.
summary(fit_data_bin) #r^2=0.4037
fit_data_bin

#95% CI of slope:
confint(fit_data_bin, level=0.95)
#99% CI of Slope:
confint(fit_data_bin, level=0.99)

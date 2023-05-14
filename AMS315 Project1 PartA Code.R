getwd()
wdir <- "C:/Users/huang/OneDrive/Documents/AMS 315"
setwd(wdir)
DV <- read.csv('830410_DV.csv', header=TRUE)
IV <- read.csv('830410_IV.csv', header=TRUE)

df1 <- Merge.data.frame(DV, IV, by="ID") #merged table

any(is.na(df1[,2])==TRUE)
any(is.na(df1[,3])==TRUE)
any(is.na(df1[,2])==TRUE)
any(is.na(df1[,3])==TRUE)
any(is.na(df1[,2])==TRUE)
any(is.na(df1[,3])==TRUE)

#num of IDs with at least 1 IV or DV (636)
nrwo(df1[!is.na(df$DV)|!is.na(df1$IV),])
#num of IDs with IV (597)
sum(is.na(df1[,3])==FALSE)
#num of IDs with DV (585)
sum(is.na(df1[,2])==FALSE)
#num of IDs with both IV and DV (546)
nrow(df1[!is.na(df1$IV) & !is.na(df1$IV),])

library(mice)
#dropped IDs missing both IV and DV
df2 <- df1[!is.na(df1$DV) == TRUE|!is.na(df$IV) == TRUE,]

#linear regression using bootstrap
df3=mice(df2, method=c("", "norm.boot", "norm.boot"), printFlag = FALSE)
df4=complete(df3) #filled in table

M <- lm(DV ~ IV, data=df4) #regression model
M

plot(df4$DV ~ df4$IV, main='scatter: DV ~ IV', xlab='IV', ylab='DV', pch=20)
abline(M, col = '#FF6699', lty ="solid", lwd = 3)
legend('topleft', legend = 'Estímated Regression Line', lty="solid", lwd=3, col ='#FF6699')

library(knitr)
kable(anova(M), caption='ANOVA Table')
summary(M)
M

#95% cI of slope:
confint(M, level = 0.95)
#99% CI of slope:
confint(M, level = 0.99)
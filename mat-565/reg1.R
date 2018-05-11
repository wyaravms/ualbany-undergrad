# Example of Simple regression.
# Income and Savings (in thousands of dollars per year)
df <- data.frame(Family=c("A","B","C","D","E"),Income=c(8,11,9,6,6),
                 Savings=c(.6,1.2,1,.7,.3))

# Look at it:
df
plot(df)
summary(df)

# Add a new variable:
df$Comsumption <- df$I - df$S

# regress savings on income
lmSI <- lm(Savings~Income,data=df)

# look at it:
lmSI
summary(lmSI)
plot(lmSI)

# plot with 95% probability region for the bivariate gaussian
require(ellipse)
Z <- ellipse(cor(df$I,df$S))
Elli <- cbind(mean(df$I)+sd(df$I)*Z[,1], mean(df$S)+sd(df$S)*Z[,2])
plot(Elli,type='l',main="95% Confidence Region of bivariate Gaussian",
     xlab="Income [$1000s per year]",ylab="Savings")

# add the points
points(df$I,df$S)
# add the regression line in "blue"
abline(lmSI$coef,col="blue")

# Locate the center at the pt. of averages
abline(h=mean(df$S))
abline(v=mean(df$I))

# rmse for regression
re <- sqrt(1-cor(df$I,df$S)^2)*sd(df$S)

# 68% confidence regression band
ab <- lmSI$coef
abline(re+ab[1],ab[2],col="blue")
abline(-re+ab[1],ab[2],col="blue")

# label the points
require(calibrate)
textxy(df$I,df$S,df$F)

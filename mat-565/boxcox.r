# Using boxcox
library(MASS)
library(ggplot2)

# true parameters
n <- 250
true.lambda <- -1/2
true.a <- 122.5
true.b <- -pi
true.r <- 0.8
true.sx <- 3.5
true.mx <- -15

# hence,
true.sy <- true.b*true.sx/true.r
true.sigma <- sqrt(1-true.r^2)* true.sy
true.my <- true.a+true.b*true.mx
# data generation
e <- rnorm(n,true.sigma)
x <- rnorm(n,true.mx,true.sx)
Ty <- true.a+true.b*x+e
y <- (1+true.lambda*Ty)^(1/true.lambda)
obs <- data.frame(x=x,y=y)
####################################################
#
plot(x,y)
qplot(x,y,data=obs)

# fit a straight line
fm1 <- lm(y~x,data=obs)
ggplot(obs,aes(x,y))+geom_point()+geom_smooth(method=lm)
boxcox(fm1)

bcox <- function(y,lambda){
    if (lambda == 0) return(log(y))
    else return( (y^lambda-1)/lambda )
}

bcox.inv <- function(u,lambda){
    if (lambda == 0) return( exp(u) )
    else return( (1+lambda*u)^(1/lambda) )
}

# transform the y values
obs$ty <- bcox(y,lambda = -0.5)
fm0 <- lm(ty~x,data=obs)
ggplot(obs,aes(x=x,y=ty))+geom_point()+geom_smooth(method=lm)

# see the orig y with the new predictions
# source('plotfit.R')
newdata <- predictvals(fm0)
newdata$yhat <- bcox.inv(newdata$ty,lambda=-0.5)
qplot(x,y,data=obs)+geom_line(aes(y=yhat),data=newdata)

# what's the mse?
obs$yhat <- bcox.inv(predict(fm0,data.frame(x=obs$x)),lambda=-0.5)
with(obs, mean((y-yhat)^2))

# compare to the mse for fm1 (the straight line)
mean(fm1$res^2)

# fit a quadratic. Now there are 3 parameters.
fm2 <- lm(y~1+x+I(x^2),data=obs)
obs$yhat2 <- predict(fm2,data.frame(x=obs$x))
with(obs, mean((y-yhat2)^2))

# show the 3 models with the data
sfits(obs,list(fm1,fm2))+geom_line(aes(y=yhat),data=newdata)

####### women
wm1 <- lm(weight~height,women)
wm2 <- lm(weight~height+I(height^2),women)
p <- sfits(women,list(wm1,wm2))
p

# find that lambda = -1 is a good choice
boxcox(wm1)
women$iw <- 1/women$weight
wm0 <- lm(iw~height,women)
sfits(women,list(wm0))

# now show the 3 fits
pwm0 <- predictvals(wm0)
p+geom_line(aes(x=height,y=1/iw),data=pwm0)

# Notice: 0 prediction error !
women$what <- round(1/predict(wm0,data.frame(height=women$h)),0)
with(women,sum((weight-what)^2))

# see it:
qplot(height,weight,data=women)+geom_line(aes(y=what),col="red")



# example of simple regression
#
# PROBLEM:
# In a certain class, midterm scores average out to 60, with
# an SD of 15. The scores on the final exam have the same average
# and SD. The correlation between the midterm scores and the final
# scores is equal to 0.8. The best prediction for the final score
# of a student whose midterm score is 90 is closest to...

# 1. Simulate a large class with 200 students like the one in the problem.
rnorm2 <- function(n,mx=0,sx=1,my=0,sy=1,r=0){
    z1 <- rnorm(n)
    z2 <- rnorm(n)
    z3 <- r*z1+sqrt(1-r^2)*z2
    data.frame(x=mx+sx*z1,y=my+sy*z3)
}

mat108 <- rnorm2(n=200,mx=60,sx=15,my=60,sy=15,r=0.8)
names(mat108) <- c("midterm","final")
plot(mat108)

# plot with 95% probability region for the bivariate gaussian
require(ellipse)
df <- mat108
Z <- ellipse(cor(df$m,df$f))
Elli <- cbind(mean(df$m)+sd(df$m)*Z[,1], mean(df$f)+sd(df$f)*Z[,2])
plot(Elli,type='l',main="95% Confidence Region of bivariate Gaussian",
     xlab="midterm scores",ylab="final scores")
points(mat108)


fm <- lm(final~midterm,data=mat108)
abline(fm,col='red')
r <- 0.8
b <- r
a <- 60-b*60
abline(a,b,col='blue')

# prediction when midterm = 90 pts.
abline(v=90,col='blue')
y90 <- predict(fm,data.frame(midterm=90))
abline(h=y90,col='blue')

#error bars for:
# regression line
ebreg.plus <- function(x,fn=c(60,15,60,15,0.8),n=200){
    names(fn) <- c("mx","sx","my","sy","r")
    b <- fn["r"]*fn["sy"]/fn["sx"]
    a <- fn["my"]-b*fn["mx"]
    x0 <- (x-fn["mx"])/fn["sx"]
    a+b*x + sqrt((1+x0^2)/n)*fn["sy"]
}

ebreg.minus <- function(x,fn=c(60,15,60,15,0.8),n=200){
    names(fn) <- c("mx","sx","my","sy","r")
    b <- fn["r"]*fn["sy"]/fn["sx"]
    a <- fn["my"]-b*fn["mx"]
    x0 <- (x-fn["mx"])/fn["sx"]
    a+b*x - sqrt((1+x0^2)/n)*fn["sy"]
}


curve(ebreg.plus,add=TRUE)
curve(ebreg.minus,add=TRUE)



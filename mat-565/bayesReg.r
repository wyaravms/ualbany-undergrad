library(MCMCpack)
data(women)
names(women) <- c("h","w")
Bregs <- function(cc0=1,dd0=1000,BB0=1e-5) {
	# no intercept
	m0 <- MCMCregress(w~0+h,data=women,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")
	# with intercept
	m1 <- MCMCregress(w~h,data=women,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")
	# quadratic
	m2 <- MCMCregress(w~h+I(h^2),data=women,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")
	# boxcox with lambda=-1
	m3 <- MCMCregress((1-1/w)~h,data=women,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")
	attr(m3,"y") <- attr(m2,"y")

	BF <- BayesFactor(m0,m1,m2,m3)
	PPM <- PostProbMod(BF)

	list(m0=m0,m1=m1,m2=m2,m3=m3,BF=BF,PPM=PPM)
}

A <- Bregs()
A$P

####################
#  plot a sample of posterior curves
ppc <- function(mcmcout,x,k=100,type="p1") {
	ri <- sample(1:dim(mcmcout)[1],k)
	S <- mcmcout[ri,]
	if (type == "p1") { # y = a + bx
		for (j in 1:k) abline(S[j,1:2],col="grey")
		abline(mean(mcmcout[,1]),mean(mcmcout[,2]),col="red")
		points(x,attr(mcmcout,"y"),col="red")
	}
	if (type == "p02") { # y = a + bx^2
		for (j in 1:k) curve(S[j,1]+S[j,2]*x^2,col="grey",add=TRUE)
		ab <- c(mean(mcmcout[,1]),mean(mcmcout[,2]))
		curve(a+b*x^2,col="red",add=TRUE)
	}
	if (type =="p012") { # y = a0+a1x+a2x^2
		for (j in 1:k) curve(S[j,1]+S[j,2]*x+S[j,3]*x^2,col="brown",add=TRUE)
		b <- colMeans(mcmcout)
		curve(b[1]+b[2]*x+b[3]*x^2,col="black",add=TRUE)
	}
}

# example:  ppc(A$m1,x=women$h)

# Homework:  add the following other "types" to ppc:
#  type == "p0"  for plotting y = constant
#  type == "p01" for y = ax  no intercept
#  type == "p2" for y = a + bx +cx^2  quadratic
#  type == "p012" for y = bx + cx^2
#  type == "p002" for y = cx^2

#### simulated quadratic model
##
## true model:  y = a + b*x^2
a <- 2
b <- -1.5
sigmae <- 1.5
n <- 30
x <- runif(n,-2,2)
err <- rnorm(n,mean=0,sd=sigmae)
y <- a + b*x^2 + err
plot(x,y)
curve(a+b*x^2,col="red",add=TRUE)

# Models:
# k1 = "straight line"
plot(x,y)
k1 <- lm(y~x)
K1 <- MCMCregress(y~x,c0=1/n,d0=10,B0=1e-3,marginal.likelihood="Chib95")
abs2 <- summary(K1)$stat[,"Mean"]
curve(abs2[1]+abs2[2]*x,col="pink",add=TRUE)
ppc(K1,x)

# k2 = "a+bx^2"
plot(x,y)
k2 <- lm(y~I(x^2))
K2 <- MCMCregress(y~I(x^2),c0=1/n,d0=10,B0=1e-3,marginal.likelihood="Chib95")
abs2 <- summary(K2)$stat[,"Mean"]
ppc(K2,x,type="p02")
curve(abs2[1]+abs2[2]*x^2,col="blue",add=TRUE)
points(x,y)

# k3 = a0+a1x+a2x^2
plot(x,y)
k3 <- lm(y~x+I(x^2))
K3 <- MCMCregress(y~x+I(x^2),c0=1/n,d0=10,B0=1e-3,marginal.likelihood="Chib95")
a0123 <- summary(K3)$stat[,"Mean"]
ppc(K3,x,type="p012")
curve(a0123[1]+a0123[2]*x+a0123[3]*x^2,col="green",add=TRUE)
points(x,y)


# Model comparison: K1,K2,K3
BF <- BayesFactor(K1,K2,K3)
PostProbMod(BF)

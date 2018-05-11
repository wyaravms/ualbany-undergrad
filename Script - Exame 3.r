##############################################
#MAT 465/565 (FALL 2014)
##############################################

setwd("C:\\Users\\WYARAVMS\\Google Drive\\graduação-ufpi-2011.2015\\ualbany-fall-2014\\mat-465-564")
library(rv)
library(ggplot2)
library(MCMCpackage)

# Problem 1
# Data
# http://omega.albany.edu:8008/mat565/MuscleMass.txt
dados1=read.table("MuscleMass.txt", header=TRUE)
dados1
ls(dados1)
plot(dados1$Age,dados1$MuscleMass)
fit1=lm(dados1$MuscleMass~dados1$Age, data=dados1)
fit1

Post<-posterior(fit1)
Post

# Problem 2
# http://omega.albany.edu:8008/mat565/cheese.txt
dados2=read.table("cheese.txt")
dados2
names(dados2) <-c("x")
rvprior0.norm(n=1,alpha=1,mu0=1,ssq0=0.1849)

rvpost0.norm(dados2$x,n=1,alpha=1,mu0=1,ssq0=0.1849)

post=rvpost0.norm(dados2$x,n=1,alpha=1,mu=1.0,ssq=0.43)
mu0=1.0
sigma0=0.43
mu=1.2
sigma=0.4123106
r=(1/2)*((((mu-mu0)^2)/sigma0^2)+(sigma0/sigma)^2-log((sigma0/sigma)^2)-1)
r

# Problem 3
dados4=read.csv("aaup1.csv", header=TRUE)
dados4

plot(dados4[,7], dados4[,8])
y4=dados4[,7]
x4=dados4[,8]

D <- data.frame(x4=x4,y4=y4)

plot(x4, y4 )
fit1=lm(y4~x4, data=D)
fit1

sfits(D,list(fit1,fit0))

Post<-posterior(fit1)
Post

sigma <- Post$sigma
betas <- Post$beta
M <- model.matrix(fit1)
y.rep <- rvnorm(mean=M %**% betas, sd=sigma)

mlplot(y.rep) # Summarize graphically

y.rep <- rvpredict(fit1)
X.pred <- data.frame(x4=mean(x4))

y.pred <- rvpredict(fit1, newdata=X.pred)

X.rep <- x4
 X.rep <- rnorm(n=n, mean=mean(X.rep), sd=sd(X.rep))
 y.pred2 <- rvpredict(fit1, newdata=X.rep)
 y.pred2
 



rvhist(Post$b[1])


D<-predictvals(Post)

n=81
fit1=lm(y4~x4, data=D)
fit1 <- MCMCregress(y4~x4,c0=1/n,d0=10,B0=1e-3,marginal.likelihood="Chib95")
abs2 <- summary(fit1)$stat[,"Mean"]
curve(abs2[1]+abs2[2]*x4,col="pink",add=TRUE)
ppc(fit1,x4)

  # Model comparison: K1,K2,K3
  BF <- BayesFactor(fit1)
  PostProbMod(BF)

#Problem 4
dados4=read.csv("aaup1.csv", header=TRUE)
dados4

plot(dados4[,7], dados4[,8])
x4=log(dados4[,7])
y4=log(dados4[,8])
plot(x4,y4)

dados4 = data.frame(dados4, x4, y4)
dados4    
x4<-dados4[,7]
y4<-dados4[,8]
x4<-((x4-mean(x4))/sd(x4))
y4<-((y4-mean(y4))/sd(y4))
D <- data.frame(x4=x4,y4=y4)


plot(x4, y4 )
fit1=lm(y4~x4, data=D)
fit1

fit0=lm(y4~0+x4,data=D)
fit0

med100 <- function(D,model.list,reps=100){
    M <- matrix(nrow=reps,ncol=length(model.list))
    for (i in 1:reps) M[i,] <- post.prob.models(D,model.list)
    meds <- apply(M,2,summary)
    colnames(meds) <- lapply(model.list,formula)
    meds
}

med100(D,list(fit1,fit0),reps=100)


# Problem 5
# http://omega.albany.edu:8008/mat565/cheese.txt
dados5=read.table("cheese.txt")
dados5
names(dados5) <-c("x")
rvpost0.norm(dados2$x,n=1,alpha=1,mu0=1,ssq0=0.1849)
sqrt(rvpost0.norm(dados2$x,n=1,alpha=1,mu0=1,ssq0=0.1849))                                     

rvprior0.norm(n=1,alpha=1,mu0=1,ssq0=0.1849)


# Problem 6
dados6=read.csv("aaup.csv", header=TRUE)
dados6

taaup<-table(dados6[,3])
sort(taaup)


#Problem 7
library(rv)
y=-1.72
rvprior0.norm(n=1,alpha=0.01,mu0=mean(y),ssq0=1)

rvpost0.norm(y,n=1,alpha=0.01,mu0=mean(y),ssq0=1)





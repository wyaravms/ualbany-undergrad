# Bayesian Logistic Reg.
# # Contraceptive Data.
# "wantsMore" = wants to have more children
cuse <- read.table("http://omega.albany.edu:8008/mat565/cuse.dat",header=TRUE)
# fit1: only main effects
cuse.fit1 <- glm(cbind(using,notUsing)~.,family=binomial,data=cuse)
cuse.post1 <- posterior(cuse.fit1) plot(cuse.post1$beta)
# fit2: main effects + wants:age interaction
cuse.fit2 <- glm(cbind(using,notUsing)~.+age:wantsMore,family=binomial,data=cuse)
cuse.post2 <- posterior(cuse.fit2) plot(cuse.fit2$beta)
# revalue
cuse$noMore <- with(cuse,wantsMore=="no")
cuse$hiEd <- with(cuse,education=="high")
attach(cuse)
 Age <- age levels(Age)[2:3] <- "25-39"
 cuse cuse.fit3 <- glm(cbind(using,notUsing)~noMore+hiEd+Age+noMore:Age,family=binomial)
 AIC(cuse.fit3)
 AIC(cuse.fit2)
 cuse.post3 <- posterior(cuse.fit3)
 plot(cuse.post3$beta)
 Pr(cuse.post3$beta[5]>0)
 Pr(cuse.post3$beta[2]>0)
 Pr(cuse.post3$beta[2]>0 & cuse.post3$beta[5]>0)
 eps <- 1 Pr(abs(cuse.post3$beta[2])+ abs(cuse.post3$beta[5]) < eps)
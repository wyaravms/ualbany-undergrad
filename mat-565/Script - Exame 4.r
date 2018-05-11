
# Question 1
dataT=read.table("tData.txt",header=FALSE)
dataT
y=length(dataT)
y
N=length(dataT)
N

colMeans(dataT)

studentT.stan <- "data {
int<lower=0> N;
int<lower=2,upper=50> y[N];
}
parameters {
real<lower=2, upper=50> theta;
}
model {
theta ~ beta(1, 1);
for (n in 1:N)
y[n] ~ dunif(theta, alpha);
}"


data <- list(N=N, y=y)

#Question 2

library(languageR)
data(english)
data2=data(english)
head(english)


# Question 3

pain = c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug = c(rep("A",9), rep("B",9), rep("C",9))
migraine = data.frame(pain,drug)
migraine
plot(pain ~ drug, data=migraine)
//aov(response ~ factor, data=data_name)
results = aov(pain ~ drug, data=migraine)
results
summary(results)

pairwise.t.test(pain, drug, p.adjust="bonferroni")


m1 <- glm(pain ~ drug, family="poisson", data=migraine)
summary(m1)

post1 <- posterior(m1)

#Q3

library(rstan)
library(rv)
library(ggplot2)

A <- c(4,5,4,3,2,4,3,4,4)
B <- c(6,8,4,5,4,6,5,8,6)
C <- c(6,7,6,6,7,5,6,5,5)
anova_dat <- data.frame(DrugA=A,DrugB=B,DrugC=C)
anova_dat

colMeans(anova_dat)

drugs.stan <- "data {
  int<lower=0> N;
  int<lower=0> J;
  real y[N,J];
}
parameters {
  real mu[J];
  real<lower=0> sigmasq_y;
  real<lower=0> alpha0;
  real<lower=0> mu0;
}
transformed parameters {
  real<lower=0> sigma_y;
  real<lower=0> sigma_mu;

  sigma_y <- sqrt(sigmasq_y);
  sigma_mu <- sigma_y/sqrt(alpha0);
}
model {
  alpha0 ~ gamma(0.225, 0.15);
  mu0 ~ normal(0, 100);
  sigmasq_y ~ inv_gamma(0.001, 0.001);
  mu ~ normal(mu0, sigma_mu); // vectorized

  for (i in 1:N)
    for (j in 1:J)
      y[i,j] ~ normal(mu[j], sigma_y);

}
"

drugs_data <- list(N=9, J=3, y=anova_dat)
(drugs_fit <- stan(model_code = drugs.stan, data= drugs_data, iter = 1000, chains = 4))

mus <- extract(drugs_fit,pars="mu")$mu
mu <- rvsims(mus,n.sims=dim(mus))
plot(mu)


# This is the number you need to remember for the exam !!!!
Pr(mu[1] < mu[2] & mu[1] < mu[3])

Pr(mu[2] > mu[1])


E(mu[2]-mu[1])
a0s <- extract(drugs_fit,pars="alpha0")$alpha0
alpha0 <- rvsims(a0s,n.sims=dim(a0s))
rvhist(alpha0)


#Question 6

library(languageR)
data(english)
data2=data(english)
head(english)

y=english$RTlexdec
x1=english$CorrectLexdec
x3=english$Familiarity
x4=english$AgeSubject
x5=english$WordCategory
x6=english$WrittenFrequency
x7=english$WrittenSpokenFrequencyRatio
x8=english$FamilySize
x9=english$DerivationalEntropy
x10=english$InflectionalEntropy
x11=english$NumberSimplexSynsets
x12=english$NumberComplexSynsets
x13=english$LengthInLetters
x14=english$Ncount
x15=english$MeanBigramFrequency
x16=english$FrequencyInitialDiphone
x17=english$ConspelV
x18=english$ConspelN
x19=english$ConphonV
x20=english$ConphonN
x21=english$ConfriendsV
x22=english$ConfriendsN
x23=english$ConffV
x24=english$ConffN
x25=english$ConfbV
x26=english$ConfbN
x27=english$NounFrequency
x28=english$VerbFrequency
x29=english$CV
x30=english$Obstruent
x31=english$Frication
x32=english$Voice
x33=english$FrequencyInitialDiphoneWord
x34=english$FrequencyInitialDiphoneSyllable
x35=english$CorrectLexdec

fit1<-lm(y~x1 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
x31 + x32 + x33 + x34 + x35)

summary(fit1)
# Significatively
fit2<-lm(y~x1 + x3 + x4 + x6 + x7 + x10 + x12 + x15 + x16 + x17 + x18 + x22 + x27 + x28 + x32)
summary(fit2)



#question 6
library(languageR)
data(english)
data2=data(english)
head(english)
library(rv)
attach(english)
plot.rv(CorrectLexdec,RTlexdec, type="n")
points.rv(RTlexdec[AgeSubject=='young'],col="red")
points.rv(RTlexdec[AgeSubject=='old'],col="black")
y.fit=lm(RTlexdec[AgeSubject=='young'] ~ CorrectLexdec[AgeSubject=='young'] + I(CorrectLexdec[AgeSubject=='young']^2) + I(CorrectLexdec[AgeSubject=='young']^3))
o.fit=lm(RTlexdec[AgeSubject=='old'] ~ CorrectLexdec[AgeSubject=='old'] + I(CorrectLexdec[AgeSubject=='old']^2) + I(CorrectLexdec[AgeSubject=='old']^3))
py=posterior(y.fit)
po=posterior(o.fit)
py
po


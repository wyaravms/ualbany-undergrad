# example of Bayesian Poisson Regression
# from: http://www.ats.ucla.edu/stat/data/poisson_sim.csv
require(rv)
require(ggplot2)
source('plotfit.R')

p <- read.csv("poisson_sim.csv",header=TRUE)
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))
(post1 <- posterior(m1))

### Plot posterior curves:
m <- glm(num_awards ~ math, family="poisson", data=p)
q <- PpcL(m,colLine='black',show.points=FALSE)
q1 <- q+geom_point(aes(x=math,y=num_awards,col=prog),
                   position=position_jitter(h=.2))
q1


## predictions on subsets of the data
Gen.d <- subset(p,prog=="General")
mg <- glm(num_awards ~ math,family=poisson,data=Gen.d)
pg <- PpcL(mg, show.points=FALSE)
pg+geom_point(aes(x=math,y=num_awards,fill=prog),col='red',
                   position=position_jitter(h=.2))


Ac.d <- subset(p,prog=="Academic")
ma <- glm(num_awards ~ math,family=poisson,data=Ac.d)
pa <- PpcL(ma, show.points=FALSE, colLine='black')
pa+geom_point(aes(x=math,y=num_awards,fill=prog), col='green',
                   position=position_jitter(h=.2))

Voc.d <- subset(p,prog=="Vocational")
mv <- glm(num_awards ~ math, family=poisson, data=Voc.d)
pv <- PpcL(mv, colpoint='green', show.points=FALSE)
pv+geom_point(aes(x=math,y=num_awards,fill=prog), col='blue',
                   position=position_jitter(h=.2))

## show all
q1+newLine(ma,colLine='green')+
    newLine(mg,colLine='red')+
        newLine(mv,colLine='blue')

#########
# A simulated example
n <- 200
x <- rnorm(n)
logLambda <- rnorm(n,mean= 0.5*x,sd=0.5)
y <- rpois(n,lambda= exp(logLambda))
df <- data.frame(x=x,y=y)
(m0 <- glm(y~x,family=poisson,data=df))
(p0 <- posterior(m0))
PpcL(m0)

px <- PpcL(m0,show.points=FALSE)
px+geom_point(aes(x=x,y=y), col='black', alpha=0.2,
              position=position_jitter(h=.2))

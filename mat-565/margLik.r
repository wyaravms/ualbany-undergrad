# Exact marginal likelihoods in the gaussian case
#
# Case: theta = mu (unknown) but sigma known.
#
m.Lmu <- function(mu0,a,sigma,y,ret.log=TRUE){
    n <- length(y)
    ave <- mean(y)
    if (n>1) v <- (n-1)/n*var(y)
    else v <- 0
    s2 <- sigma^2
    E <- -n/(2*s2)*(v+a/(n+a)*(ave-mu0)^2)
    logC <- (1/2)*log(a/(a+n)) - (n/2)*log(2*pi*s2)

    if (ret.log) return( logC+E )
    else return( exp( logC+E ) )
}

Lik <- function(theta,sigma,y,ret.log=TRUE){
    n <- length(y)
    ave <- mean(y)
    if(n>1) v <- (n-1)/n*var(y)
    else v <- 0
    s2 <- sigma^2
    E <- -n/(2*s2)*(v+(theta-ave)^2)
    logC <- -(n/2)*log(2*pi*s2)

    if (ret.log) return( logC+E )
    else return( exp( logC+E ) )
}

# Example
# sigma <- 1
# y <- rnorm(10)
# theta0 <- mean(y)
# alpha <- 1
# Theta <- rvnorm(mean=theta0,var=sigma^2/alpha)
#
# lik <- function(theta) Lik(theta,sigma=sigma,y,ret.log=FALSE)
# Exact:
# m.Lmu(mu0=theta0,a=alpha,sigma=sigma,y=y)
# Approx:
# log( E(lik(Theta)) )

# Case: theta = sigma (unknown) but mu known.
#

lIk <- function(theta,mu,y,ret.log=TRUE){
    n <- length(y)
    ave <- mean(y)
    if(n>1) v <- (n-1)/n*var(y)
    else v <- 0
    E <- -n/(2*theta)*(v+(mu-ave)^2)
    if (ret.log) return( E - (n/2)*log(2*pi*theta) )
    else return( exp( E - n/2*log(2*pi*theta) ) )
}


m.Lsigma <- function(sigma0,a,mu,y,ret.log=TRUE){
    t0 <- sigma0^2
    n <- length(y)
    m <- mean(y)
    if(n>1) v <- (n-1)/n*var(y)
    else v <- 0
    A <- -(n/2)*log(2*pi*a*t0)-a/2*log(2)
    B <- 1+(n/(a*t0))*(v+(mu-m)^2)
    C <- log(gamma((n+a)/2)/gamma(a/2)) + (n+a)/2*log(2)

    if (ret.log) return( C+A - (n+a)/2*log(B) )
    else return( exp( C+A - (n+a)/2*log(B) ) )
}

m.Lmusigma <- function(mu0,sigma0,a,y,ret.log=TRUE){
    v0 <- sigma0^2
    n <- length(y)
    m <- mean(y)
    if(n>1) v <- (n-1)/n*var(y)
    else v <- 0
    an <- a/(n+a)
    logp <- log(gamma((n+a)/2)/gamma(a/2))+a/2*log(a*v0)+1/2*log(an)
    A <- n*v+a*v0+n*an*(m-mu0)^2
    logp <- logp - (n+a)/2*log(A) - n/2*log(pi)

    if (ret.log) return( logp )
    else exp(logp)
}

LIk <- function(mu,sigma,y,ret.log=TRUE) lIk(sigma,mu,y,ret.log)

rventropic0 <- function(alpha,mu0,sigma0){
    if(!(alpha>0 & sigma0>0)) stop("Invalid parameters!")
    Sigma2 <- rvinvchisq(df=alpha,scale=sigma0^2)
    Mu <- rvnorm(mean=mu0,var=Sigma2/alpha)
    Theta <- 1:2
    names(Theta) <- c("mu","var")
    impute(Theta) <- c(Mu,Sigma2)
    Theta
}

## general regression
LIK <- function(beta,sigma,X,y,ret.log=FALSE){
    s2 <- sigma^2
    n <- length(y)
    llik <- -n/2*log(2*pi*s2)
    llik <- llik -1/2*sum((y-X%**%beta)^2)/s2
    if (ret.log) return(llik)
    else return(exp(llik))
}

##
# fit <- lm(y~x)
# post <- posterior(fit)
# log of Estimated Marginal likelihood:
# log(E(LIK(post$b,post$s,model.matrix(fit),y)))
# Approximation fixing the thetas at their MLEs
# LIK(fit$coef,sd(fit$res),model.matrix(fit),y,TRUE)

# approx prior by posterior based on a few observations D0
# i.e. D0 <- D[1:4,]
MLik <- function(fit,D0,ret.log=TRUE){
    fit0 <- lm(formula(fit),data=D0)
    if (fit0$df == 0) stop("d.f. =0. Change D0")
    post0 <- posterior(fit0)
    margLik(fit,post=post0,ret.log=ret.log)
}

margLik <- function(fit,post=posterior(fit),ret.log=TRUE){
    X <- model.matrix(fit)
    beta <- post$beta
    sigma <- post$sigma
    y <- fit$model[,1]
    if (ret.log) return(log(E(LIK(beta,sigma,X,y))))
    else return(E(LIK(beta,sigma,X,y)))
}

Lik.atMLE <- function(fit,ret.log=TRUE){
    X <- model.matrix(fit)
    beta <- fit$coefficients
    sigma <- sd(fit$residuals)
    y <- fit$model[,1]
    if(ret.log) return(LIK(beta,sigma,X,y,ret.log=TRUE))
    else return(LIK(beta,sigma,X,y,ret.log=FALSE))
}


# approx. Posterior Probabilities for list of models.
grank <- function(model) model$rank

post.prob.models <- function(data,model.list,ret.log=FALSE){
    n0 <- 1+max(unlist(lapply(model.list,grank)))
    n <- nrow(data)
    ri <- sample(1:n,n0)
    D0 <- data[ri,]
    log.pp <- unlist(lapply(model.list,MLik,D0=D0))
    pp <- exp(log.pp)
    pp <- pp/sum(pp)
    if(ret.log) return(log(pp))
    else return(round(pp,2))
}

## Example: True model x=runif(n,-1,1), y = rnorm(n,mean=3*x-x^4,sd=0.4)
# look at the change when x=runif(n,-1,1.5) instead.
#   n <- 50
#   D <- data.frame(x=x,y=y)
#   f1 <- lm(y~x,D)
#   f01 <- lm(y~0+x,D)
#   f2 <- lm(y~x+I(x^2),D)
#   f4 <- lm(y~x+I(x^3)+I(x^4),D)
#   f04 <- lm(y~0+I(x^4),D)
#
#   Ppc(f1); Ppc(f04,xvar="x")
#   sfits(D,list(f1,f4,f04),xvar="x")
#
#   post.prob.models(D,list(f1,f01,f2,f4,f04))
#   post.prob.models(D,list(f1,f01,f2,f4,f04))
#
#   median of 100 reps:

med100 <- function(D,model.list,reps=100){
    M <- matrix(nrow=reps,ncol=length(model.list))
    for (i in 1:reps) M[i,] <- post.prob.models(D,model.list)
    meds <- apply(M,2,summary)
    colnames(meds) <- lapply(model.list,formula)
    meds
}
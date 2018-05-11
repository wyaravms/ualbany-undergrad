##############################################################
# sampling the 0prior for 1dim Gaussian.
# Based on alpha > 0 prior observations with
# sufficient statistics mu0 and ssq0.
#
# (mu,ssq) ~ invchisq(alpha,ssq0) * N(mu0,ssq/alpha)
#
require(rv)
rvprior0.norm <- function(n=1,alpha=1,mu0=0,ssq0=1){
    ssq <- rvinvchisq(n,df=alpha,scale=ssq0)
    mu <- rvnorm(mean=mu0,var=ssq/alpha)
    theta <- list(mu=mu,sigma.sq=ssq)
    theta
}

rvpost0.norm <- function(y,n=1,alpha=1,mu0=0,ssq0=1){
    mean.obs <- mean(y)
    var.obs <- mean(y^2)-mean.obs^2
    n.obs <- length(y)
    mu1 <- (alpha*mu0+n.obs*mean.obs)/(alpha+n.obs)
    alpha1 <- alpha+n.obs
    ssq1 <- (alpha*(ssq0 + (mu0-mu1)^2) +
             n.obs*(var.obs + (mean.obs-mu1)^2))/alpha1
    theta <- rvprior0.norm(n,alpha=alpha1,mu0=mu1,ssq0=ssq1)
    theta
}

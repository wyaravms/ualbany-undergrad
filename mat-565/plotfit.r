# plot points with fitted model
library(ggplot2)

plotfit <- function(fm,ebars=FALSE,level=0.68,xvar=NULL,...){
    Y <- fm$fit
    call <- as.character(fm$call)
    data <- eval(as.name(call[length(call)]))
    if(!is.null(xvar)) x <- data[xvar][,1]
    else  x <- fm$mod[,2]
    y <- fm$mod[,1]
    if (!is.null(xvar)) namex <- xvar
    else namex <- names(fm$mod)[2]
    namey <- names(fm$mod)[1]
    title <- gsub("~","\x7e",fm$call)[2]
    p <- qplot(x,y)+xlab(namex)+ylab(namey)+geom_line(y=Y)+ ggtitle(title)
    if(ebars){
         pv <- predict(fm,data,interval="prediction",level=level)
        return(p+geom_errorbar(aes(ymin=lwr,ymax=upr),data=as.data.frame(pv)))
    }
    else return(p)
}

##################################################################################
# Given a model, predict values of yvar from xvar
# This supports one predictor and one predicted variable
# xrange: If NULL, determine the x range from the model object. If a vector with
# two numbers, use those as the min and max of the prediction range.
# samples: Number of samples across the x range.
# ...: Further arguments to be passed to predict()

orig.predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
    # If xrange isn't passed in, determine xrange from the models.
    # Different ways of extracting the x range, depending on model type
    if (is.null(xrange)) {
        if (any(class(model) %in% c("lm", "glm")))
            xrange <- range(model$model[[xvar]])
        else if (any(class(model) %in% "loess"))
            xrange <- range(model$x)
        }
    newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
    names(newdata) <- xvar
    newdata[[yvar]] <- predict(model, newdata = newdata, ...)
    newdata
}


# same as orig.predictvals BUT xvar and yvar are determined from model.
predictvals <- function(model, xvar=NULL, xrange=NULL, samples=100, ...) {
    # If xrange isn't passed in, determine xrange from the models.
    # Different ways of extracting the x range, depending on model type
    if (is.null(xvar)) xvar <- names(model$model)[2]
    yvar <- names(model$model)[1]
    if (any(class(model) %in% "loess")){
        yvar <- names(attr(model$terms,"dataClasses"))[1]
        xvar <- names(attr(model$terms,"dataClasses"))[2]
    }

    if (is.null(xrange)) {
        if (any(class(model) %in% c("lm", "glm")))
            if(is.null(xvar)) xrange <- range(model$model[[xvar]])
            else {
                call <- as.character(model$call)
                data <- eval(as.name(call[length(call)]))
                xrange <- range(data[xvar][,1])
            }
        else if (any(class(model) %in% "loess"))
            xrange <- range(model$x)
        }
    newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
    names(newdata) <- xvar
    newdata[[yvar]] <- predict(model, newdata = newdata, ...)
    newdata
}



#####
# Example:
# fm1 <- lm(weight~height,women)
# fm2 <- lm(weight~height+I(height^2),women)
# sfits(women,list(fm1,fm2))
#


sfits <- function(df,model.list, xvar=NULL, xrange=NULL, samples=100,
                      cols=c("red","blue","green","brown"),...){
    M <- model.list[[1]]
    if (is.null(xvar)) xvar <- names(M$model)[2]
    yvar <- names(M$model)[1]
    if (any(class(M) %in% "loess")){
        yvar <- names(attr(M$terms,"dataClasses"))[1]
        xvar <- names(attr(M$terms,"dataClasses"))[2]
    }

    p <- ggplot(df,aes_string(x=xvar,y=yvar))+
        geom_point()+xlab(xvar)+ylab(yvar)

    for (i in 1:length(model.list)){
        M <- model.list[[i]]
        p <- p+geom_line(data=predictvals(M,xvar,xrange,samples),col=cols[i],...)
    }
    p
}


#######
# Plot sample of Posterior Curves.

Ppc <- function(fit,post=posterior(fit),xvar=NULL,xrange=NULL,size=100){
    if(is.null(xvar)) xvar <- names(fit$model)[2]
    yvar <- names(fit$model)[1]
    betas <- sims(post[[1]])
    N <- dim(betas)[1]
    ri <- sample(1:N,size)
    df <- as.data.frame(fit$model)
    p <- ggplot(df,aes_string(x=xvar,y=yvar))+
        geom_point()+xlab(xvar)+ylab(yvar)

    for(j in 1:size){
        fit$coefficients <- betas[ri[j],]
        p <- p+geom_line(data=predictvals(fit,xvar=xvar,xrange=xrange),col='grey')
    }
    fit$coefficients <- colMeans(betas)
    p+geom_point()+
        geom_line(data=predictvals(fit,xvar=xvar,xrange=xrange),col='red')
}




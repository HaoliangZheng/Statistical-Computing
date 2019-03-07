### 1 sample beta and sigma with Bayesian Inference

## Data generating process

n = 1000
p = 3

epsilon = rnorm(n, 0, 1)
beta = c(-3, 1, 3, 5)
X = cbind( 1, matrix(runif(n*p),n,p) )
y = X%*%beta + epsilon

## log posterior function

library(mvtnorm)

logPos1 <- function(beta, sig, y, X)
{
  if(sig<0)
  {
    stop("sig should be morn than zero")
  }
  
  p = length(beta)
  
  ## The log likelihood function
  LogLikelihood <- sum(dnorm(y,X%*%beta,sig,log = TRUE))

  ## The priors
  LogPrior4beta <- dmvnorm(x = t(beta),matrix(0, 1, p),diag(p), log = TRUE)
  
  LogPrior4sigma2 <- dchisq(sig, df = 10,log = TRUE)
  
  LogPrior <- LogPrior4beta + LogPrior4sigma2
  
  ## The log posterior
  LogPosterior <- LogLikelihood + LogPrior
  
  return(LogPosterior)
}

## Metropolis Algorithm

mvm.blocked = function(n, beta0, sig0, burnin=1000, lambda, td)
{ 
  if(sig0<0)
  {
    stop("sig0 should be morn than zero")
  }
  
  n = n + burnin
  p = ncol(X)
  
  beta <- matrix(NA, n, p)
  sig <- matrix(NA, n, 1)
  a = b = rep(NA,n)
  
  beta[1,]=beta0;sig[1,]=sig0
  

  for (i in 1:(n-1))
  {
    
    bnew = as.numeric(rmvnorm(1, beta[i,], lambda*diag(p)))
    logalpha.b = min( 0, td(bnew,sig[i],y,X)-td(beta[i,],sig[i],y,X) )
    alpha.b = exp(logalpha.b)
    a[i]=alpha.b
    
    pd = runif(1)
    
    if(pd<alpha.b)
    {
      beta[i+1,]=bnew
    }
    
    if(pd>alpha.b)
    {
      beta[i+1,]=beta[i,]
    }
    
    
    signew <- rgamma(1, 1)
    logalpha.s = min( 0, td(beta[i+1,],signew,y,X)-td(beta[i+1,],sig[i],y,X) )
    alpha.s = exp(logalpha.s)
    b[i]=alpha.s
    
    pd = runif(1)
    
    if(pd<alpha.s)
    {
      sig[i+1,]=signew
    }
    
    if(pd>alpha.s)
    {
      sig[i+1,]=sig[i,]
    }
    
  }
  
  return(list( "all.beta"=beta[1:n,],"selected.beta"=beta[(burnin+1):n,],"all.sig"=sig[1:n,],"selected.sig"=sig[(burnin+1):n,] ))
}

## result

result1 = mvm.blocked(10000,c(1,1,1,1),1,1000,0.01,logPos1)
op = par(mfrow=c(2,2))
plot(result1$all.beta[,1],type="l")
plot(result1$all.beta[,2],type="l")
plot(result1$all.beta[,3],type="l")
plot(result1$all.beta[,4],type="l")
par(op)
plot(result1$all.sig,type="l")

apply(result1$selected.beta, 2, mean)
mean(result1$selected.sig)


### 2 logistic regression with Bayesian Inference

## data

library("car")
library("carData")
library("lmtest")
library("zoo")
library("sandwich")
library("survival")
library("AER")

data("Affairs", package = "AER");   # 数据导入与展示

Affairs$Yaffair[Affairs$affairs == 0] = 0
Affairs$Yaffair[Affairs$affairs > 0] = 1   # 将频率转化为0-1

y=Affairs$Yaffair    # 因变量为是否发生婚外恋
x1=Affairs$age       # 自变量为年龄和婚姻的自我评分
x2=Affairs$rating    # 1表示非常不幸福，5表示非常幸福

## log posterior function

logPos2 = function(beta)
{
  LogLikelihood = sum(-y*log(1+exp(-1*beta[1]*x1-beta[2]*x2))-(1-y)*log(1+exp(beta[1]*x1+beta[2]*x2)))
  LogPrior = dnorm(beta[1],0,5,log=TRUE)*dnorm(beta[2],0,5,log=TRUE)
  LogPosterior = LogLikelihood + LogPrior
  return(LogPosterior)
}

## Metropolis Algorithm

mvm = function(n, x0, y0, burnin=1000, lambda, td)
{  
  n = n + burnin
  points = matrix(NA,n,2)
  a = rep(NA,n)
  points[1,1]=x0;points[1,2]=y0
  
  if(n<=burnin)
  {
    stop("n should be morn than 10000")
  }
  
  for (i in 1:(n-1))
  {
    
    xnew = rmvnorm(1, points[i,], lambda*diag(2))
    logalpha = min( 0, td(c(xnew[1],xnew[2]))-td(c(points[i,1],points[i,2])) )
    alpha = exp(logalpha)
    a[i]=alpha
    
    pd = runif(1)
    
    if(pd<alpha)
    {
      points[i+1,]=xnew
    }
    
    if(pd>alpha)
    {
      points[i+1,]=points[i,]
    }
  }
  
  return(list( "all"=points ,"burnin"=points[1:burnin,] , "selected"=points[(burnin+1):n,] ))
}

## result

result2 = mvm(1000,0,0,10000,0.01,logPos2)
op = par(mfrow=c(2,1))
plot(result2$all[,1],type="l")
plot(result2$all[,2],type="l")
par(op)

apply(result2$selected, 2, mean)      # compared with the result from glm
fit0 = glm(Yaffair~-1+age+rating,data = Affairs,family = binomial)
fit0$coefficients

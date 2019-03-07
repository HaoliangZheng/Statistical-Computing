### Bayesian Inference with MCMC

## data

x = cars$speed
y = cars$dist

lm1=lm(y~-1+x)
lm1$coefficients


## distribution

p_y_theta = function(theta)
{
  s = 0
  for(i in 1:length(x))
  {
    s = s + dnorm(y[i],theta*x[i],1,log=TRUE)
  }
  return(s)
}

p_theta = function(theta)
{
  dnorm(theta,0,5,log=TRUE)
}

p_theta_y = function(theta)
{
  p_y_theta(theta)+p_theta(theta)
}

x0 = seq(-20,20,0.01)
plot(x0,p_theta_y(x0),type="l")
x0[which.max(p_theta_y(x0))]

## Metroplis Algorithm

malg = function(n, x0, lambda, td)
{  
  burnin = 10000
  a = x = rep(NA,n)
  x[1]=x0
  
  if(n<=burnin)
  {
    stop("n should be morn than 10000")
  }
  
  for (i in 1:n)
  {
    
    xnew = rnorm(1, x[i], lambda)
    logalpha = min( 0, td(xnew)-td(x[i]) )
    alpha = exp(logalpha)
    a[i]=alpha
    
    pd = runif(1)
    
    if(pd<alpha)
    {
      x[i+1]=xnew
    }
    
    if(pd>alpha)
    {
      x[i+1]=x[i]
    }
  }
  
  return(list("x.all"=x, "x.selected" = x[(burnin+1):length(x)],"alpha.all"=a,"alpha.selected"=a[(burnin+1):length(a)]))
}

## result
library("coda")     # install.packages(coda)

myplot = function(x)
{
  op = par(mfrow=c(2,2))
  
  hist(x$x.selected)
  plot(density(x$x.selected),col = "blue")
  plot(x$x.all,type = "l") 
  acf(x$x.selected)
}

ESS = function(x)
{
  #library("coda")
  effectiveSize(x$x.selected)/length(x$x.selected)
}

result1 = malg(100000,3,3,p_theta_y)
mean(result1$x.selected)
myplot(result1)
ESS(result1)

result2 = malg(100000,3,sqrt(30),p_theta_y)
mean(result2$x.selected)
myplot(result2)
ESS(result2)

result3 = malg(100000,3,1,p_theta_y)
mean(result3$x.selected)
myplot(result3)
ESS(result3)

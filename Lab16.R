### Multivariate Target Distribution and Gibbs Sampling

## target distribution and plot

library(mvtnorm)

mvtd = function(x,y)
{
  a = cbind(x,y)
  
  mean1 = c(1,1)
  sigma1 = matrix(c(1,0.3,0.3,1),2,2)
  
  delta2=c(-1,-1)
  df2 = 3
  sigma2 = matrix(c(1,0.7,0.7,1),2,2)
  
  p = 0.4*dmvnorm(a,mean=mean1,sigma=sigma1) + 
      0.6*dmvt(a,delta=delta2,sigma=sigma2,df=df2,log = FALSE)
  return(p)
}

x0 = y0 =seq(-4,4,0.1)
z0 = outer(x0,y0,mvtd)

persp(x0,y0,z0,theta = 0,phi = 10,col='royalblue')
contour(x0,y0,outer(x0,y0,mvtd),nlevels = 20)
image(x0, y0, z0)
filled.contour(x0,y0,z0)


## Multivariate Metropolis
# Algorithm 1

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
    logalpha = min( 0, log(td(xnew[1],xnew[2]))-log(td(points[i,1],points[i,2])) )
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

# result 1

result.mvm = mvm(1000,0,0,10000,3,mvtd)
contour(x0,y0,outer(x0,y0,mvtd),nlevels = 20)
points(result.mvm$selected[,1],result.mvm$selected[,2],col="blue",pch = 20, cex=0.75)

stay = as.data.frame(table(result.mvm$selected))[,2]>1
points(result.mvm$selected[,1][stay],result.mvm$selected[,2][stay],col="red",pch = 20, cex=0.75)
stay2 = as.data.frame(table(result.mvm$selected))[,2]>2
points(result.mvm$selected[,1][stay2],result.mvm$selected[,2][stay2],col="green",pch = 20, cex=0.75)

plot(result.mvm$selected[,1],col="blue",type = "l")
points(result.mvm$selected[,2],col="red",type = "l")

# Algorithm 2

mvm2 = function(n, x0, y0, burnin=1000, lambda, td)
{  
  n = 5*n + burnin
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
    logalpha = min( 0, log(td(xnew[1],xnew[2]))-log(td(points[i,1],points[i,2])) )
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
  
  return(list( "all"=points ,"burnin"=points[1:burnin,] , "selected"=points[seq((burnin+1),n,5),] ))
}

# result 2

result.mvm2 = mvm2(1000,0,0,10000,3,mvtd)
contour(x0,y0,outer(x0,y0,mvtd),nlevels = 20)
points(result.mvm2$selected[,1],result.mvm2$selected[,2],col="blue",pch = 20, cex=0.75)

stay = as.data.frame(table(result.mvm2$selected))[,2]>1
points(result.mvm2$selected[,1][stay],result.mvm2$selected[,2][stay],col="red",pch = 20, cex=0.75)
stay2 = as.data.frame(table(result.mvm2$selected))[,2]>2
points(result.mvm2$selected[,1][stay2],result.mvm2$selected[,2][stay2],col="green",pch = 20, cex=0.75)

plot(result.mvm2$selected[,1],col="blue",type = "l")
points(result.mvm2$selected[,2],col="red",type = "l")


## Rejection Sampling within Gibbs
# Algorithm

gibbs.rs = function(n,x0,y0,burnin=1000,mvtd)
{
  n = n + burnin
  points = matrix(NA,n,2)
  points[1,1]=x0;points[1,2]=y0
  
  rej = function(otd)
  {
    x = seq(-10,10,0.01)
    z = rep(NA,length(x))
    z = otd(x)/dt(x,df=4)
    xM = x[which.max(z)]
    M = otd(xM)/dt(xM,df=4)
    
    i = 0
    
    while(i<1)
    {
      u = runif(1-i)
      v = rt(1-i,4)
      l = length(v[u < 1/M * otd(v)/dt(v,4)])
      y = v[u < 1/M * otd(v)/dt(v,4)]
      i = i+l
    }
    return(y)
  }
  
  for(i in 1:(n-1))
  {
    xtd = function(x)
    {
      mvtd(x,y=points[i,2])
    }
    points[(i+1),1] = rej(xtd)
    
    ytd = function(y)
    {
      mvtd(x=points[(i+1),1],y)
    }
    points[(i+1),2] = rej(ytd)
  }
  
  return(list( "all"=points ,"burnin"=points[1:burnin,] , "selected"=points[(burnin+1):n,] ))
}

# result

result.rs = gibbs.rs(1000,0,0,1000,mvtd)
contour(x0,y0,outer(x0,y0,mvtd),nlevels = 20)
points(result.rs$selected[,1],result.rs$selected[,2],col="blue",pch = 20, cex=0.75)

sum(as.data.frame(table(result.rs$selected))[,2]>1)

plot(result.rs$selected[,1],col="blue",type = "l")
points(result.rs$selected[,2],col="red",type = "l")


## Metropolis within Gibbs
# Algorithm

gibbs.m = function(n,x0,y0,burnin=1000,mvtd)
{
  n = 5*n + burnin
  points = matrix(NA,n,2)
  points[1,1]=x0;points[1,2]=y0
  
  malg = function(x0, otd)
  {  
    xnew = rnorm(1,x0,5)
    logalpha = min( 0, log(otd(xnew))-log(otd(x0)) )
    alpha = exp(logalpha)
    
    pd = runif(1)
    
    if(pd<alpha)
    {
      x=xnew
    }
    
    if(pd>alpha)
    {
      x=x0
    }
    
    return(x)
  }
  
  for(i in 1:(n-1))
  {
    xtd = function(x)
    {
      mvtd(x,y=points[i,2])
    }
    points[(i+1),1] = malg(points[(i),1],xtd)
    
    ytd = function(y)
    {
      mvtd(x=points[(i+1),1],y)
    }
    points[(i+1),2] = malg(points[(i),2],ytd)
  }
  
  return(list( "all"=points ,"burnin"=points[1:burnin,] , "selected"=points[seq((burnin+1),n,5),] ))
}

# result

result.m = gibbs.m(1000,0,0,1000,mvtd)

contour(x0,y0,outer(x0,y0,mvtd),nlevels = 20)
points(result.m$selected[,1],result.m$selected[,2],col="blue",pch = 20, cex=0.75)
stay = as.data.frame(table(result.m$selected))[,2]>1
points(result.m$selected[,1][stay],result.m$selected[,2][stay],col="red",pch = 20, cex=0.75)
stay2 = as.data.frame(table(result.m$selected))[,2]>2
points(result.m$selected[,1][stay2],result.m$selected[,2][stay2],col="green",pch = 20, cex=0.75)

plot(result.m$selected[,1],col="blue",type = "l")
points(result.m$selected[,2],col="red",type = "l")

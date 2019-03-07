### Metroplis-Hastings Algorithm

## target distribution

td = function(x)
{
  0.4*dnorm(x,3,1)+0.3*dnorm(x,5,3)+0.3*dnorm(x,0,2)
}
x0 = seq(-5,10,0.01)
plot(x0,td(x0),type = "l")

Etd = function(x)
{
  x*td(x)
}

Ex = integrate(Etd,-Inf,Inf); Ex

Dtd = function(x)
{
  (x-2.7)^2*td(x)
}

Dx = integrate(Dtd,-Inf,Inf); sd = sqrt(8.11)
Dx; sd

E3 = function(x)
{
  (x-2.7)^3*td(x)
}
Ss = integrate(E3,-Inf,Inf)
Sk = 7.026/8.11^(3/2);Sk

E4 = function(x)
{
  (x-2.7)^4*td(x)
}
Ks = integrate(E4,-Inf,Inf)
Ku = 251.2437/8.11^(2);Ku-3


## Algorithm

mhalg = function(n, x0, mu, sig, td)
{  
  burnin = 10000
  a = x = rep(NA,n)
  x[1]=x0
  
  if(n<=burnin)
  {
    stop("n should be morn than 10000")
  }
  
  qd = function(x){
    dnorm(x,mu,sig)
  }
  
  for (i in 1:n)
  {
    
    xnew = rnorm(1, mu, sig)
    logalpha = min( 0, log(td(xnew))+log(qd(x[i]))-log(td(x[i]))-log(qd(xnew)) )
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


## myfunctions

mySummary=function(x)
{
  n = length(x)
  skewness = function(x) (sum((x-mean(x))^3)/(n))/(sum((x-mean(x))^2)/(n))^(3/2)
  kurtosis = function(x) (sum((x-mean(x))^4)/(n))/(sum((x-mean(x))^2)/(n))^2-3
  
  if(!is.vector(x))
  {
    stop("You must input a vector")
  }
  if(!is.numeric(x))
  {
    data.frame("length"=length(x),"type"=typeof(x),row.names = "")  
  }
  else
  {
    data.frame("Min."=min(x),"Q1"=quantile(x,probs=0.25),"Median"=quantile(x,probs=0.5),
               "Q3"=quantile(x,probs=0.75),"Max." =max(x),"Mean"=mean(x),"Var."=var(x)*n/(n-1),"Sd."=sqrt(var(x)*n/(n-1)),
               "skew."=skewness(x),"kurt."=kurtosis(x),"length"=length(x),"type"=typeof(x),row.names = "")
  }
}

myplot = function(x)
{
  hist(x$x.selected)
  plot(x0,td(x0),type = "l",col = "red")
  lines(density(x$x.selected),col = "blue")
  plot(x$x.all,type = "l") 
  acf(x$x.selected)
}


## result

op = par(mfrow=c(2,2))

set.seed(1)
result1 = mhalg(100000,0,0,1,td)            # initial
mySummary(result1$x.selected)
myplot(result1)

set.seed(1)
result2 = mhalg(100000,0,2,1,td)            # effect of proposal expected value
mySummary(result2$x.selected)
myplot(result2)

set.seed(1)
result3 = mhalg(100000,0,2,3,td)            # effect of proposal variance
mySummary(result3$x.selected)
myplot(result3)

set.seed(1)
result4 = mhalg(100000,0,5,3,td)            # effect of proposal variance
mySummary(result4$x.selected)
myplot(result4)

## test

set.seed(1)
test1 = mhalg(100000,0,5,1,td)
mySummary(test1$x.selected)
myplot(test1)

set.seed(1)
test2 = mhalg(100000,2.5,5,1,td)            # effect of starting value
mySummary(test2$x.selected)
myplot(test2)

set.seed(1)
test3 = mhalg(100000,2.5,5,3,td)
mySummary(test3$x.selected)
myplot(test3)

set.seed(1)
test3.1 = mhalg(100000,100,5,3,td)
mySummary(test3.1$x.selected)
myplot(test3.1)

set.seed(1)
test3.2 = mhalg(100000,2.5,15,3,td)
mySummary(test3.2$x.selected)
myplot(test3.2)

### Metropolis algorithm

## function

dt5 = function(x){
  (1+x^2/5)^(-3)
}

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
    logalpha = min( 0, log(td(xnew))-log(td(x[i])) )
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

## plot x , summarize x + acf(x)

op=par(mfrow=c(2,2))

result1 = malg(100000,1,1,dt5)
mean(result1$x.selected);var(result1$x.selected)
plot(result1$x.all,type = "l")
hist(result1$x.selected)
plot(density(result1$x.selected))
acf(result1$x.selected)

result2 = malg(100000,100,1,dt5)                    # effect of starting value
mean(result2$x.selected);var(result2$x.selected)
plot(result2$x.all,type = "l")                      # still converge 
hist(result2$x.selected)
plot(density(result2$x.selected))
acf(result2$x.selected)
                                                    
result3 = malg(100000,100,3,dt5)                    # effect of proposal variance
mean(result3$x.selected);var(result3$x.selected)    
plot(result3$x.all,type = "l")                      # converge faster    
hist(result3$x.selected)
plot(density(result3$x.selected))
acf(result3$x.selected)                             # acf[1] is smaller

## mean(alpha)

par(op)

meanalpha = rep(NA,10)
for(i in 1:10)
{
  meanalpha[i]=mean(malg(100000,1,i,dt5)$alpha.selected)
}

plot(meanalpha,type = "l", xlab="lambda", ylab = "mean(alpha)")

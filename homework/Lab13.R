### 拒绝接受法抽样

## 1 完整密度表达式

# 曲线图

x1 = seq(-4,4,by = 0.01)
ft1 = dt(x1,4)
fn1 = dnorm(x1)
plot(x1, ft1, xlab = "X", ylab = "density", col = "blue", type = "l", ylim = c(0,0.4))
points(x1, fn1, col = "red", type = "l", lwd = 1)

# 抽样函数

rejnorm = function(n)
{
  x = seq(-2,2,0.01)
  xM = x[which.max(dnorm(x)/dt(x,df=4))]
  M = dnorm(xM)/dt(xM,df=4)
  
  i = 0
  y = rep(NA,n)
  
  while(i<n)
  {
    u = runif(n-i)
    v = rt(n-i,4)
    l = length(v[u < 1/M * dnorm(v)/dt(v,4)])
    y[(i+1):(i+l)] = v[u < 1/M * dnorm(v)/dt(v,4)]
    i = i+l
  }
  return(y)
}

r1 = rejnorm(500);r1
mean(r1)
var(r1)
hist(r1)

# 抽样作图

rejnormplot = function(n)
{
  x = seq(-4,4,0.01)
  xM = x[which.max(dnorm(x)/dt(x,df=4))]
  M = dnorm(xM)/dt(xM,df=4)
  
  ft = M*dt(x,4)
  fn = dnorm(x)
  plot(x, ft, xlab = "X", ylab = "density", col = "blue", type = "l", ylim = c(0,0.45))
  points(x, fn, col = "red", type = "l", lwd = 1)
  
  nb = 0
  i = 0
  y = rep(NA,n)
  
  while(i<n)
  {
    u = runif(n-i)
    v = rt(n-i,4)
    l = length(v[u < 1/M * dnorm(v)/dt(v,4)])
    
    r = u < 1/M * dnorm(v)/dt(v,4) ; b = u >= 1/M * dnorm(v)/dt(v,4)
    points(v[r],M*dt(v[r],4)*u[r],col = "red", pch = 20, cex=0.75)
    points(v[b],M*dt(v[b],4)*u[b],col = "blue", pch = 20, cex=0.75)
    
#   y[(i+1):(i+l)] = v[u < 1/M * dnorm(v)/dt(v,4)]
    nb = nb + length(v[b])
    i = i+l
  }
  return(nb)
}

rejnormplot(500)


## 2 只用kernel

# 核函数

knorm = function(x)
{
  exp(-x^2/2)
}

kt = function(x,df)
{
  (1+x^2/df)^(-(df+1)/2)
}

# 曲线图

x2 = seq(-4,4,by = 0.01)
ft2 = kt(x2,4)
fn2 = knorm(x2)
plot(x2, ft2, xlab = "X", ylab = "density", col = "blue", type = "l", ylim = c(0,1))
points(x2, fn2, col = "red", type = "l", lwd = 1)

# 抽样函数

rejk = function(n)
{
  x = seq(-2,2,0.01)
  xM = x[which.max(knorm(x)/kt(x,df=4))]
  M = knorm(xM)/kt(xM,df=4)
  
  i = 0
  y = rep(NA,n)
  
  while(i<n)
  {
    u = runif(n-i)
    v = rt(n-i,4)
    l = length(v[u < 1/M * knorm(v)/kt(v,4)])
    y[(i+1):(i+l)] = v[u < 1/M * knorm(v)/kt(v,4)]
    i = i+l
  }
  return(y)
}

r2 = rejk(500);r2
mean(r2)
var(r2)
hist(r2)

# 抽样作图

rejkplot = function(n)
{
  x = seq(-4,4,0.01)
  xM = x[which.max(knorm(x)/kt(x,df=4))]
  M = knorm(xM)/kt(xM,df=4)
  
  ft = M*kt(x,4)
  fn = knorm(x)
  plot(x, ft, xlab = "X", ylab = "density", col = "blue", type = "l", ylim = c(0,1.1))
  points(x, fn, col = "red", type = "l", lwd = 1)
  
  nb = 0
  i = 0
  y = rep(NA,n)
  
  while(i<n)
  {
    u = runif(n-i)
    v = rt(n-i,4)
    l = length(v[u < 1/M * knorm(v)/kt(v,4)])
    
    r = u < 1/M * knorm(v)/kt(v,4) ; b = u >= 1/M * knorm(v)/kt(v,4)
    points(v[r],M*kt(v[r],4)*u[r],col = "red", pch = 20, cex=0.75)
    points(v[b],M*kt(v[b],4)*u[b],col = "blue", pch = 20, cex=0.75)
    
#   y[(i+1):(i+l)] = v[u < 1/M * knorm(v)/kt(v,4)]
    nb = nb + length(v[b])
    i = i+l
  }
  return(nb)
}

rejkplot(500)


## 3 对比

set.seed(1)
rejnorm(5)

set.seed(1)
rejk(5)


set.seed(1)
rejnormplot(5)

set.seed(1)
rejkplot(5)     # 经过对比发现二者完全一致

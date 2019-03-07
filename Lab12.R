### 运用Nelder-Mead算法求解logistic回归

### 原做法：
## 1 数据获取与处理

# install.packages("AER")

library("car")
library("carData")
library("lmtest")
library("zoo")
library("sandwich")
library("survival")
library("AER")

data("Affairs", package = "AER");Affairs   # 数据导入与展示

Affairs$Yaffair[Affairs$affairs == 0] = 0
Affairs$Yaffair[Affairs$affairs > 0] = 1   # 将频率转化为0-1

y=Affairs$Yaffair    # 因变量为是否发生婚外恋
x1=Affairs$age       # 自变量为年龄和婚姻的自我评分
x2=Affairs$rating    # 1表示非常不幸福，5表示非常幸福


## 2 多维牛顿迭代函数

ndNR<-function(x0,f,g,h)
{
  epsilon = 1e-10
  MaxIter = 10
  i = 1
  mat = matrix(NA,length(x0),11); mat[,1] = x0-solve(h(x0),g(x0))
  fv = rep(NA,12); fv[1] = f(x0) ; fv[2] = f(mat[,1])
  
  change = abs(fv[2]-fv[1])
  
  while((i<=MaxIter)&&(change>epsilon))
  {
    mat[,i+1] = mat[,i] - solve(h(mat[,i]),g(mat[,i]))
    fv[i+2] = f(mat[,i+1])
    change = abs(fv[i+2]-fv[i+1])
    i = i+1
  }
  plot(fv, type = "l")
  return(mat)
}


## 3 logistic回归

#目标函数、梯度与黑塞矩阵

func = function(beta)
{
  sum(-y*log(1+exp(-1*beta[1]*x1-beta[2]*x2))-(1-y)*log(1+exp(beta[1]*x1+beta[2]*x2)))
}

grad = function(beta)
{
  g1 = sum(x1*y*(1+exp(beta[1]*x1+beta[2]*x2))^(-1)-x1*(1-y)*(1+exp(-1*beta[1]*x1-beta[2]*x2))^(-1))
  g2 = sum(x2*y*(1+exp(beta[1]*x1+beta[2]*x2))^(-1)-x2*(1-y)*(1+exp(-1*beta[1]*x1-beta[2]*x2))^(-1))
  
  out = matrix(c(g1,g2),2,1)
  return(out)
}

hess = function(beta)
{
  h1 = -1*sum(x1*y*x1*exp(beta[1]*x1+beta[2]*x2)*(1+exp(beta[1]*x1+beta[2]*x2))^(-2)
              + x1*(1-y)*x1*exp(-1*beta[1]*x1-beta[2]*x2)*(1+exp(-1*beta[1]*x1-beta[2]*x2))^(-2))
  h2 = -1*sum(x2*y*x1*exp(beta[1]*x1+beta[2]*x2)*(1+exp(beta[1]*x1+beta[2]*x2))^(-2)
              + x2*(1-y)*x1*exp(-1*beta[1]*x1-beta[2]*x2)*(1+exp(-1*beta[1]*x1-beta[2]*x2))^(-2))
  h3 = -1*sum(x1*y*x2*exp(beta[1]*x1+beta[2]*x2)*(1+exp(beta[1]*x1+beta[2]*x2))^(-2)
              + x1*(1-y)*x2*exp(-1*beta[1]*x1-beta[2]*x2)*(1+exp(-1*beta[1]*x1-beta[2]*x2))^(-2))
  h4 = -1*sum(x2*y*x2*exp(beta[1]*x1+beta[2]*x2)*(1+exp(beta[1]*x1+beta[2]*x2))^(-2)
              + x2*(1-y)*x2*exp(-1*beta[1]*x1-beta[2]*x2)*(1+exp(-1*beta[1]*x1-beta[2]*x2))^(-2))
  
  out = matrix(c(h1,h2,h3,h4),2,2)
  return(out)
}

# 结果:beta的估计(无截距)

ndNR(c(0.01,0.01),func,grad,hess)
beta0 = ndNR(c(0.01,0.01),func,grad,hess)[,5]


### Nelder-Mead算法：
fmin = function(beta){-1*func(beta)}

nmi = function(p1,p2,p3,f)
{
  epsilon = 1e-9
  MaxIter = 50
  area = 10
  i = 1
  
  while((i<=MaxIter)&&(area>epsilon))
  {
    mat = cbind(p1,p2,p3)
    
    px = mat[,order(c(f(p1),f(p2),f(p3)))[1:2]]
    pp1 = mat[,order(c(f(p1),f(p2),f(p3)))[1]]
    pp2 = mat[,order(c(f(p1),f(p2),f(p3)))[2]]
    pp3 = mat[,order(c(f(p1),f(p2),f(p3)))[3]]
    
    x0 = apply(px,1,mean) 
    xr = x0 + x0 - pp3
    
    if(f(pp1)<= f(xr) & f(xr)<f(pp2))
    {
      p1=pp1
      p2=xr
      p3=pp2
    }
    
    if(f(xr)<f(pp1))
    {
      xe = x0 + 2*(xr-x0)
      if(f(xe)<f(xr)){p1 = pp1;p2 = xe; p3 = pp2}
      else {p1 = pp1;p2 = pp2; p3 = xr}
    }
    
    if(f(xr)>=f(pp2))
    {
      xc = x0+ 0.5*(pp3-x0)
      if(f(xc)<f(pp3)){p1 = pp1;p2 = xc; p3 = pp2}
      else {
        x2 = pp1 + 0.5*(pp2-pp1)
        x3 = pp1 + 0.5*(pp3-pp1)
        p1 = pp1;p2 = x2; p3 = x3}
    }
    
    a1 = p2-p1
    a2 = p3-p1
    area = abs(1/2*det(cbind(a1,a2)))
    i=i+1
  }
  return(cbind(p1,p2,p3))
}

nmi(c(0,0),c(1,0),c(0,1),fmin)

optim(c(1,2),fmin)

### 运用logistic回归预测发生婚外恋的概率

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

# (含截距)
# x1 = 1
# ndNR(c(0.01,0.01),func,grad,hess)

# 概率的预测

prob = function(beta,x1,x2)
{
  P = rep(NA,length(x1))
  for(i in 1:length(x1))
  {
  P[i]=(1+exp(-1*beta[1]*x1[i]-beta[2]*x2[i]))^(-1)
  }
  return(P)
}

prob(beta0,rep(mean(Affairs$age),5),c(1,2,3,4,5))

## 4 用R自带函数实现

# logistic回归 (属于广义线性回归)
fit0 = glm(Yaffair~-1+age+rating,data = Affairs,family = binomial)
summary(fit0)

# fit1 = glm(Yaffair~rating,data = Affairs,family = binomial)
# summary(fit1)

# 预测
testdata = data.frame(rating=c(1,2,3,4,5),age=mean(Affairs$age))
pr = predict(fit0, newdata = testdata, type = "response")
pr

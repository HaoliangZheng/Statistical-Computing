### 运用极大似然估计线性模型参数

## 思路
#通过最优化极大似然函数的表达式可以看出，当sigma固定时，极大似然估计与最小二乘估计是对同一个表达式求最优
#我们可以先固定sigma，运用极大似然估计出beta；再将beta代入似然函数，将sigma看作待估参数，运用极大似然估计sigma
#以下以简单线性回归为例


## 创建数据
set.seed(1)
x = runif(200,-100,100)
set.seed(1)
y = 1 + 3*x + rnorm(length(x),0,10)

plot(x,y)


## 求解beta估计值

# 目标函数、梯度与黑塞矩阵
func = function(beta)
{
  sum((y-beta[1]-beta[2]*x)^2)
}

grad = function(beta)
{
  matrix(c(sum(-2*(y-beta[1]-beta[2]*x)),sum(-2*x*(y-beta[1]-beta[2]*x))),2,1)
}

hess = function(beta)
{
  matrix(c(2*length(x),2*sum(x),2*sum(x),2*sum(x^2)),2,2)
}

# 多维牛顿迭代
ndNR<-function(x0,f,g,h)
{
  epsilon = 1e-10
  MaxIter = 10
  i = 1
  mat = matrix(NA,length(x0),10)
  mat[,1] = x0-solve(h(x0),g(x0))
  change = abs(f(mat[,1])-f(x0))

  while((i<=MaxIter)&&(change>epsilon))
  {
    mat[,i+1] = mat[,i] - solve(h(mat[,i]),g(mat[,i]))
    change<-abs(f(mat[,i+1])-f(mat[,i]))
    i = i+1
  }
  return(mat)
}

mat = ndNR(c(100,20),func,grad,hess)
beta = mat[,length(na.omit(mat[1,]))]

## 求解sigma估计值

# 目标函数、一阶导与二阶导

f = function(sig)
{
  sum(dnorm(y,beta[1]+beta[2]*x,sig,log = TRUE))
}

g = function(sig)
{
  sum(-1/sig+(y-beta[1]-beta[2]*x)^2*sig^(-3))
}

g1 = function(sig)
{
  sum(1/sig^2-3*(y-beta[1]-beta[2]*x)^2*sig^(-4))
}

# 牛顿迭代

NR<-function(x0,f,g,g1)
{
  epsilon = 1e-10
  MaxIter = 10
  i = 1
  sig = rep(NA,10)
  sig[1] = x0-g(x0)/g1(x0)
  change = abs(f(sig[1])-f(x0))
  
  while((i<=MaxIter)&&(change>epsilon))
  {
    sig[i+1]=sig[i]-g(sig[i])/g1(sig[i])
    change<-abs(f(sig[i+1])-f(sig[i]))
    i = i+1
  }
  return(sig[i])
}

sigma = NR(1,f,g,g1)


## 结果验证（与lm、optim作比较）
# 对beta
beta
lm(y~x)
optim(c(100,20),func,grad,method = "BFGS")

# 对sigma
sigma
sqrt(sum((y-beta[1]-beta[2]*x)^2)/length(x))
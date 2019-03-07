############################################
## 用一维牛顿迭代法解极大似然估计
# 目标函数、一阶导数与二阶导数
f = function(x,mu,sig)
{
  sum(dnorm(x,mu,sig,log = TRUE))
}

g = function(x,mu,sig)
{
  sum(-1/sig + (x-mu)^2/(sig)^3)
}

g1 = function(x,mu,sig)
{
  sum(1/(sig)^2 - 3*(x-mu)^2/(sig)^4)
}

# 创建数据
x = c(20738,19967,18889,15889,15872,13488,13309,12943,12528,11918)
mu = 15500:15600
sig = rep(NA,20)
sig[1]=2000
fmax = sigmax = rep(NA,length(mu))

# 对不同的mu循环进行牛顿迭代
for(j in 1:length(mu))
{
  for(i in 1:20)
  {
    sig[i+1]=sig[i]-g(x,mu[j],sig[i])/g1(x,mu[j],sig[i])
    i=i+1
  }
  sigmax[j] = sig[20]             # 记录下在这个mu下使f取到最值的sig 
  fmax[j] = f(x,mu[j],sigmax[j])  # 记录下在这个mu下f的最值
}

sigmax                          # 对于每一个mu，使f取到最大值的sig
sigmax[which.max(fmax)]         # 所有mu和sig中，对应f最大的sig

# 绘图
n = seq(2000,4200,0.1)
h=c()
for(i in 1:length(n))
{
  h[i] = g(x,15554,n[i])
}
plot(n,h,"l")
abline(h=0)
for(i in 1:20)
{
  sig[i+1]=sig[i]-g(x,15554,sig[i])/g1(x,15554,sig[i])
  
  lines(c(sig[i],sig[i]),c(0,g(x,15554,sig[i])),col = "blue")
  lines(c(sig[i],sig[i+1]),c(g(x,15554,sig[i]),0),col = "red")
  
  i=i+1
}


############################################
## 用多维牛顿迭代法解最小二乘法
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

# 创建数据
set.seed(1)
x = runif(200,-100,100)
set.seed(1)
y = 1 + 3*x + rnorm(length(x),0,10)

plot(x,y)

# 多维牛顿迭代
nralgo<-function(x0,f,g,h)
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

# 结果（与lm、optim作比较）
mat = nralgo(c(100,20),func,grad,hess)
mat
lm(y~x)
optim(c(100,20),func,grad,method = "BFGS")

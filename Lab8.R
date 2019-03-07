#######################################
## 1/ alignment grid
x1 = runif(10)
plot(x1, panel.first = grid(), main = "grid",
     ylab = "value", xlim = c(1,10), ylim = c(0,1))


#######################################
## 2/ layout: 3 plots in one screen
split.screen(c(1,2))                      
split.screen(c(2,1),2) 

x2 = rexp(100)

screen(1)
hist(x2,main = "Histogram", xlab = "x")
screen(3)
plot(sort(x2), dexp(x2[order(x2)]),type = "l",
     xlab = "x", ylab = "p(x)", main = "PDF")
text(x = 3,y = .5,expression(p(x)==paste(lambda,e^{-lambda*x})))
screen(4)
plot(sort(x2),pexp(x2[order(x2)]),type = "l",
     xlab = "x", ylab = "F(x)", main = "CDF")

close.screen(all = TRUE)


#######################################
## 3/ add more plots(including 3D)  

#1 two-dimensional standard normal distribution
x = y = seq(-4,4,by = 0.1)
f = function(x,y){exp(-0.5*(x^2+y^2))/(2*pi)}
z = outer(x,y,f)
persp(x,y,z,theta = 0,phi = 10,col='royalblue')

#2 3D Scatter Plot
library(scatterplot3d)
attach(mtcars)
s3d = scatterplot3d(wt, disp, mpg, pch=16, highlight.3d=TRUE, type="h",
                    main="3D Scatter Plot with Vertical Lines and Regression Plane")
fit = lm(mpg ~ wt+disp)
s3d$plane3d(fit)

#3 interactive 3D
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=5)

#4 terrain of a volcano 
z = volcano
x = 10*(1:nrow(z))   
y = 10*(1:ncol(z))    

image(x, y, z, col=terrain.colors(100), axes=F)
contour(x, y, z, levels=seq(from=min(z), to=max(z), by=10),axes=F, add=T)

persp(x,y,z,theta=120,phi=15,scale=F,axes=F)


#######################################
## 4/ Newton's algorithm for finding the maximum of loglikelihood
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

x = c(20738,19967,18889,15889,15872,13488,13309,12943,12528,11918)
mu = 15500:15600
sig = rep(NA,20)
sig[1]=2000
fmax = sigmax = rep(NA,length(mu))

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
  
  lines(c(sig[i],sig[i]),c(0,g(x,15554,sig[i])))
  lines(c(sig[i],sig[i+1]),c(g(x,15554,sig[i]),0))
  
  i=i+1
}



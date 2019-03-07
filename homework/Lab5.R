## to put 4 graphs together
par(mfrow=c(2,2))

## continuous distributions
#1 normal
x1 = seq(-5,5,length.out = 100)
y1 = rnorm(100)
plot(x1,dnorm(x1))
plot(y1,dnorm(y1),xlim = c(-5,5))
hist(pnorm(x1))
hist(pnorm(y1))

#2 beta
x2 = seq(0,1,length.out = 100)
y2 = rbeta(100,0.5,0.5)
plot(x2,dbeta(x2,0.5,0.5))
plot(y2,dbeta(y2,0.5,0.5))
hist(pbeta(x2,0.5,0.5))
hist(pbeta(y2,0.5,0.5))

#3 cauchy
x3 = seq(-5,5,length.out = 100)
y3 = rcauchy(100)
plot(x3,dcauchy(x3))
plot(y3,dcauchy(y3),xlim = c(-5,5))
hist(pcauchy(x3))
hist(pcauchy(y3))

#4 chi-squared
x4 = seq(0,6,length.out = 100)
y4 = rchisq(100,1)
plot(x4,dchisq(x4,1))
plot(y4,dchisq(y4,1),xlim = c(0,6))
hist(pchisq(x4,1))
hist(pchisq(y4,1))

#5 exponential
x5 = seq(0,5,length.out = 100)
y5 = rexp(100)
plot(x5,dexp(x5))
plot(y5,dexp(y5),xlim = c(0,5))
hist(pexp(x5))
hist(pexp(y5))

#6 F
x6 = seq(0,5,length.out = 100)
y6 = rf(100,5,5)
plot(x6,df(x6,5,5))
plot(y6,df(y6,5,5),xlim = c(0,5))
hist(pf(x6,5,5))
hist(pf(y6,5,5))

#7 gamma
x7 = seq(0,8,length.out = 100)
y7 = rgamma(100,2)
plot(x7,dgamma(x7,2))
plot(y7,dgamma(y7,2),xlim = c(0,8))
hist(pgamma(x7,2))
hist(pgamma(y7,2))

#8 log-normal
x8 = seq(0,5,length.out = 100)
y8 = rlnorm(100)
plot(x8,dlnorm(x8))
plot(y8,dlnorm(y8),xlim = c(0,5))
hist(plnorm(x8))
hist(plnorm(y8))

#9 student's t
x9 = seq(-5,5,length.out = 100)
y9 = rt(100,1)
plot(x9,dt(x9,1))
plot(y9,dt(y9,1),xlim = c(-5,5))
hist(pt(x9,1))
hist(pt(y9,1))

#10 uniform
x10 = seq(0,1,length.out = 100)
y10 = runif(100)
plot(x10,dunif(x10))
plot(y10,dunif(y10))
hist(punif(x10))
hist(punif(y10))

#11 weibull
x11 = seq(0,4,length.out = 100)
y11 = rweibull(100,1)
plot(x11,dweibull(x11,1))
plot(y11,dweibull(y11,1),xlim = c(0,4))
hist(pweibull(x11,1))
hist(pweibull(y11,1))

#12 logistic
x12 = seq(-5,5,length.out = 100)
y12 = rlogis(100)
plot(x12,dlogis(x12))
plot(y12,dlogis(y12))
hist(plogis(x12))
hist(plogis(y12))

## discrete distributions
#13 binomial
x13 = 0:30
y13 = rbinom(30,30,0.7)
plot(x13,dbinom(x13,30,0.7))
plot(y13,dbinom(y13,30,0.7),xlim = c(0,30))
hist(pbinom(x13,30,0.7))
hist(pbinom(y13,30,0.7))

#14 geometric
x14 = 0:50
y14 = rgeom(50,0.1)
plot(x14,dgeom(x14,0.1))
plot(y14,dgeom(y14,0.1),xlim = c(0,50))
hist(pgeom(x14,0.1))
hist(pgeom(y14,0.1))

#15 hypergeometric
x15 = 0:20
y15 = rhyper(50,25,25,20)
plot(x15,dhyper(x15,25,25,20))
plot(y15,dhyper(y15,25,25,20),xlim = c(0,20))
hist(phyper(x15,25,25,20))
hist(phyper(y15,25,25,20))

#16 negative binomial
x16 = 0:20
y16 = rnbinom(100,20,0.75)
plot(x16,dnbinom(x16,20,0.75))
plot(y16,dnbinom(y16,20,0.75),xlim = c(0,20))
hist(pnbinom(x16,20,0.75))
hist(pnbinom(y16,20,0.75))

#17 poisson
x17 = 0:20
y17 = rpois(50,10)
plot(x17,dpois(x17,10))
plot(y17,dpois(y17,10),xlim = c(0,20))
hist(ppois(x17,10))
hist(ppois(y17,10))

#18	signed rank
x18 = 0:20
y18 = rsignrank(20,5)
plot(x18,dsignrank(x18,5))
plot(y18,dsignrank(y18,5),xlim = c(0,20))
hist(psignrank(x18,5))
hist(psignrank(y18,5))

#19 Wilcoxon
x19 = 0:30
y19 = rwilcox(30,5,5)
plot(x19,dwilcox(x19,5,5))
plot(y19,dwilcox(y19,5,5),xlim = c(0,30))
hist(pwilcox(x19,5,5))
hist(pwilcox(y19,5,5))
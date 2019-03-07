## mySummary
#1
mySummary=function(x)
{
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
      skewness = function(x) (sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/length(x))^(3/2)
      kurtosis = function(x) (sum((x-mean(x))^4)/length(x))/(sum((x-mean(x))^2)/length(x))^2-3
      data.frame("Min." = min(x),"Q1" = quantile(x,probs=0.25),"Median" = quantile(x,probs=0.5),
             "Q3"= quantile(x,probs=0.75),"Max." = max(x),"Mean" = mean(x),"Var." = var(x),"Sd."=sd(x),
             "skew."= skewness(x),"kurt."= kurtosis(x),"length" = length(x),"type" = typeof(x),row.names = "")
    }
}

#2
mySummary(c("test","cool","good"))
mySummary(rep(1,10))
mySummary(1:100)
mySummary(rnorm(50,0,0.1))

#3
weekPlanNew=data.frame(today="a",tomorrow=1)
mySummary(weekPlanNew)


## quaroot
#1
quaroot = function(a,b,c)
{
  if(!is.numeric(a)||!is.numeric(b)||!is.numeric(c))
  {
    stop("They must be numeric")
  }
  if(length(a) != length(b)||length(a) != length(c)||length(b) != length(c))
  {
    stop("They must be the same length")
  }
  x1 = x2 = delta = 1:length(a)
  for(i in 1:length(a))
  {
    if(a[i]==0)
    {
      stop("quadratic coefficients can't be 0")
    }
    delta[i] = b[i]^2-4*a[i]*c[i]
    if(delta[i]<0)
    {
      x1[i] = complex(real = -b[i]/(2*a[i]),imaginary = (sqrt(-delta[i]))/(2*a[i]))
      x2[i] = complex(real = -b[i]/(2*a[i]),imaginary = -(sqrt(-delta[i]))/(2*a[i]))
    }else
    {
    x1[i] = (-b[i]+sqrt(delta[i]))/(2*a[i])
    x2[i] = (-b[i]-sqrt(delta[i]))/(2*a[i])
    }
  }
  cbind(x1,x2)
}

#2
quaroot(1,4,-1)
quaroot(-2,2,0)
quaroot(3,-9,1)
quaroot(1,0,-4)
a = c(1,-2,3,1);b = c(4,2,-9,0);c = c(-1,0,1,-4)
quaroot(a,b,c)

#3
quaroot(1,0,4)
quaroot(2,1,4)
x = c(1,2);y = c(0,1);z = c(4,4)
quaroot(x,y,z)
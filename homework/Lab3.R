##apply(X, MARGIN, FUN, ...)
ary=array(1:27,c(3,3,3));ary
apply(ary,1,sum)
apply(ary,c(1,2),sum)
dat=data.frame(a=1:10,b=1:10);dat
apply(dat,1,sum)
#对数组、数据框，按行或列进行循环计算

##tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
##(table,分组计算)
x=1:10;x
set.seed(2)
t=round(runif(10,0,2));t 
tapply(x,t,sum)
#以向量t为索引,把数据集X进行分组,再apply

##mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,USE.NAMES = TRUE)
##(multivariate，sapply的变形，可接收多个数据)
n=rep(4,4);mean=rep(0,4);var=rep(1,4)
mapply(rnorm,n,mean,var)
#生成4个符合正态分布的数据集，均值和方差均为0和1
#其中n,mean,var均作为输入rnorm的参数

###对list计算
##lapply(X, FUN, ...)
##(list)
set.seed(2)
x=list(a=1:10,b=rnorm(50,0,1),c=c(TRUE,FALSE,FALSE,TRUE));x
lapply(x,mean)

##sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
##(simplify，lapply的简化版，增加了2个参数simplify和USE.NAMES)
sapply(x,mean)
class(lapply(x,mean));class(sapply(x,mean))
#让输出看起来更友好，返回值为向量，而不是list
lapply(x,mean)
sapply(x,mean,simplify=FALSE,USE.NAMES=FALSE)
class(lapply(x,mean));class(sapply(x,mean,simplify=FALSE,USE.NAMES=FALSE))
#如果simplify=FALSE和USE.NAMES=FALSE，那么sapply函数就等于lapply函数

##vapply(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
##(value,控制返回值,类似于sapply)
vapply(x,mean,FUN.VALUE = 0)
vapply(x,mean,FUN.VALUE = as.integer(0))
#当返回值设为整型时，因不满足要求，所以报错

##rapply(object, f, classes = "ANY", deflt = NULL,
##      how = c("unlist", "replace", "list"), ...)
##(recursive，对list的每个元素进行递归遍历，如果list包括子元素则继续遍历)
lst=list(x=list(x1=1:3,x2=4:6));lst
lapply(lst,sum)
#此时不能使用lapply
rapply(lst,sum,how="unlist")
rapply(lst,sum,how="replace")

###其他
##simplify2array(x, higher = TRUE)
set.seed(2)
x=list(a=1:10,b=rnorm(50,0,1),c=c(TRUE,FALSE,FALSE,TRUE));
simplify2array(lapply(x,mean))
sapply(x,mean)
class(simplify2array(lapply(x,mean)));class(sapply(x,mean))
#apply族的simplify=TRUE通过调用simplify2array得以实现

##sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
##STATS使其与apply不同的主要一点
M=matrix(1:12,ncol=3);M
sweep (M, 2, c (1: 3), "+")
#每一列的每一项加其对应列数

##aggregate(x, by, FUN, ..., simplify = TRUE, drop = TRUE)
##(将数据进行分组,然后对每一组数据进行函数统计,最后把结果组合成表格返回)
state.x77
state.region
aggregate(state.x77, list(Region = state.region), mean)
#对美国50个州八个指标的数据集根据地区进行分类求均值

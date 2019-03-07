### the plot of classification success

## Read the data

azip <- read.table("azip.dat")
dzip <- as.numeric(read.table("dzip.dat"))
testzip <- read.table("testzip.dat")
dtest <- read.table("dtest.dat")


## Codes in "ClassificationofHandwrittenDigits.R"
# 以下是课件中用于画识别率图的代码

test.idx <- 1:100
basis.max <- 1:20
nCorrectOut <- numeric(length(test.idx))
resid.norm <- matrix(0, 10, 1, dimnames = list(0:9,"resid"))

for(b in basis.max)
{
  nCorrect <- 0
  for(k in test.idx)
  {
    for(i in 0:9)
    {
      img.mat <- azip[, i == dzip, drop = FALSE]                      # 这两行在循环中被不断执行，导致代码运行速度较慢
      img.matSVD <- svd(img.mat)                                      # 实则可以在循环前先将数据储存在变量中
      resid.norm[i+1, ] <- norm(matrix(lm(testzip[, test.idx[k]]~
                                            0+img.matSVD$u[, 1:basis.max[b]])$resid), "F")
    }
    if((which.min(resid.norm)-1)  == dtest[k])
    {
      nCorrect <- nCorrect+1
    }
  }
  nCorrectOut[b] <- nCorrect
  print(b)
}


## My Codes
# 根据以上对代码的分析，我改进了代码来提高运行速度，并将整个流程放到一个函数中，可以改变test和basis的范围

CorrectOut = function(tidx,bmax)
{
  test.idx <- 1:tidx
  basis.max <- 1:bmax
  nCorrectOut <- numeric(length(basis.max))
  resid.norm <- matrix(0, 10, 1, dimnames = list(0:9,"resid"))
  
  p <- paste("img.mat",0:9,sep="")
  
  for(i in 0:9)
  {
    img.mat <- azip[, i == dzip, drop = FALSE]
    img.matSVD <- svd(img.mat)
    assign(p[i+1],img.matSVD$u)
  }
  
  for(b in basis.max)
  {
    nCorrect <- 0
    
    for(k in test.idx)
    {
      for(i in 0:9)
      {
        
        resid.norm[i+1, ] <- norm(matrix(lm(testzip[, test.idx[k]]~
                                              0+get(p[i+1])[, 1:basis.max[b]])$resid), "F")
      }
      if((which.min(resid.norm)-1)  == dtest[k])
      {
        nCorrect <- nCorrect+1
      }
    }
    
    nCorrectOut[b] <- nCorrect
    print(b)
  }

  ratio = nCorrectOut/length(test.idx)
  plot(ratio,type = "l",main=paste("test.idx=",tidx),xlab = "No. of bases",ylab = "Correct Specification Rate")
  return(ratio)
}
  # 返回识别率，并画识别率图


## result
# 查看test范围对识别率的影响

length(testzip)

result1 = CorrectOut(100,88)
result2 = CorrectOut(200,88)
result3 = CorrectOut(500,88)
result4 = CorrectOut(1000,88)
result5 = CorrectOut(length(testzip),88)

result = list("test.idx=100"=result1,"test.idx=200"=result2,"test.idx=500"=result3,"test.idx=1000"=result4,
              "test.idx=2007"=result5)
save(result,file="result.Rdata")

# 由于以上代码的运行仍需要一段时间，因此我把结果的图像与数据保存下来，通过邮件发送

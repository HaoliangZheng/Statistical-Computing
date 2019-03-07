###--------------------------------------------------------------------------------------
### SCEXAM -- 运用朴素贝叶斯法预测能否成功申请美研统计学专业
###--------------------------------------------------------------------------------------

## Name:郑昊亮
## ID:2016310868
## Class:应用统计学16


###--------------------------------------------------------------------------------------
### 1 Data
###--------------------------------------------------------------------------------------

## Getting data with web scraping

library(xml2)                 # install.packages("xml2")
library(rvest)                # install.packages("rvest")
library(stringr)              # install.packages("stringr")
library(readr)                # install.packages("readr")


get_offer_detail = function(indices)
{
  # 创建空的原始数据集
  rawdata = data.frame("index"=NA,"result"=NA,"apply_for"=NA,"major"=NA,"degree"=NA,
                       "gpa"=NA,"TOEFL"=NA,"GRE"=NA,"from"=NA)
  rawdata = rawdata[-1,]
  
  n = length(indices)
  for(i in 1:n)
  {
    link = as.character(indices[i])                                      # 由于网址的特征，只需要
    url = paste("http://bbs.gter.net/thread-",link,"-1-1.html",sep = "") # 输入数字序号再粘贴即可
    web = read_html(url)
    
    # 根据网页源代码特征，读取属性 class=“typeoption”的div节点的第一个table节点的summary属性的值“offer n”
    # 并用parse\_number函数提取其中的数字n，减1得到该学生的offer数
    offernum = web %>% 
      html_nodes(xpath ='//div[@class="typeoption"]/table[1]') %>% html_attr(name = "summary") %>%
      parse_number() - 1
    
    
    ## 获取该学生的个人情况信息
    
    result = apply_for = gpa = TOEFL = GRE = from = NA                   # 每次循环都将重置一次
    
    j = offernum + 1            # 最后一个栏目是学生的个人信息，也就是第j个table节点
    
    # 不同网页在个人信息栏目的源码格式并不完全相同，但由于相同内容的th元素的内容是相同的
    # 可以先获得在该table下的tbody中tr节点的个数
    
    path = paste('//div[@class="typeoption"]/table[',as.character(j),']/tbody/tr',sep = "")
    m = length(web %>% html_nodes(xpath = path)) - 1
    
    if( m==0 ){next()}          # 对于空栏目网页的处理
    
    # 然后通过循环，对每个tr节点，分别读取其th，用if进行匹配，记录匹配到的对应数据
    for(t in 1:m)
    {
      
      patht = paste('//div[@class="typeoption"]/table[',as.character(j),']/tbody/tr[',as.character(t),']/th',sep = "")
      title = web %>% html_nodes(xpath = patht) %>% html_text()
      
      pathc = paste('//div[@class="typeoption"]/table[',as.character(j),']/tbody/tr[',as.character(t),']/td',sep = "") 
      content = web %>% html_nodes(xpath = pathc) %>% html_text(trim = TRUE)
      
      if(title=="TOEFL:"&!is.na(str_extract(content,"[0-9]+[0-9]")))
      {

        TOEFL = parse_number(content)

      }
      
      if(title=="IELTS:"&!is.na(str_extract(content,"[0-9]")))
      {

             value = parse_number(content)          # 若为雅思成绩，
             TOEFL = min( value /7 * 100 , 120 )    # 则根据网上的换算方法，统一换算为TOEFL成绩

      }
      
      if(title=="GRE:"&!is.na(str_extract(content,"[0-9]+[0-9]")))
      {

        GRE = parse_number(content)
        
      }
      
      if(title=="本科学校档次:")
      {
        from = content
      }
      
      if(title=="本科成绩和算法、排名:")
      {
        value = parse_number(content) %>% as.numeric()        
        if(value<=5) {gpa = min(value*25*0.95, 97.5)}   # 若gpa算法为4分制，
        else {gpa = value}                              # 统一换算为百分制
      }
      
    }
    
    
    ## 获取该学生的申请结果信息
    
    for(k in 1:offernum)
    {
      
      # 由于不同网页在学生申请信息栏目的源码格式是相同的，所以直接读取
      path1 = paste('//div[@class="typeoption"]/table[',as.character(k),']/tbody/tr[1]/td',sep = "")
      apply_for = web %>% html_nodes(xpath = path1) %>% html_text(trim = TRUE)
      
      path2 = paste('//div[@class="typeoption"]/table[',as.character(k),']/tbody/tr[2]/td',sep = "")
      degree = web %>% html_nodes(xpath = path2) %>% html_text()    
      
      path3 = paste('//div[@class="typeoption"]/table[',as.character(k),']/tbody/tr[3]/td',sep = "")
      major = web %>% html_nodes(xpath = path3) %>% html_text(trim = TRUE)
      
      path4 = paste('//div[@class="typeoption"]/table[',as.character(k),']/tbody/tr[4]/td',sep = "")
      result = web %>% html_nodes(xpath = path4) %>% html_text()
      
      # 确认申请专业是否为统计以及学位是否为硕士，若满足条件，则添加数据
      if ( length(grep("tatistics",major))+length(grep("MS",degree))+length(grep("M.S.",degree))+ 
           length(grep("MA",degree))+length(grep("M.A.",degree)) == 2 )
      {
        
        newdata = data.frame("index"=indices[i],"result"=result,"apply_for"=apply_for,"major"=major,
                             "degree"=degree,"gpa"=gpa,"TOEFL"=TOEFL,"GRE"=GRE,"from"=from)
        
        rawdata = rbind(rawdata,newdata)
        
      }
      
    }
    
  }
  
  return(rawdata)
}

# 以下记录了我选取的网页的网址对应的数字序列
indices = c(2183945,2173329,2166634,2164306,2163661,2162099,2161829,2160326,2160291,2159937,2159749,
            2159114,2159105,2158221,2157375,2153668,2153400,2152386,2150439,2151027,2151838,2151277,
            2149991,2148173,2174431,2162268,2156375,2156365,2156333,2156007,2154907,2153395,2156373,
            2150384,2149422,2086191,2082757,2081567,2077634,2076475,2076377,2075775,2074054,2073453,
            2073289,2066331,2071189,2070334,2070912,2070677,2069872,2069728,2069164,2066426,2066073,
            2065955,2065833,2065831,2063150,2062370,2030581,1990221,1988653,1984994,1966911,1966699,
            1962935,1960815,1959745,1957927,1957839,1957623,1956399,1956323,1951490,1950873,1948965,
            1948771,1947076,1944614,1939644,1861546,1829873,1825984,1815685,1822444,1820625,1820160,
            1819685,1819492,1818338,1817899,1817163,1817129,1816922,1816629,1816517,1815901,1814306,
            1814210,1813038,1812755,1812450,1811602,1811575,1811134,1787809,1810260,1810257,1808444,
            1806270)
# table(indices)

rawdata = get_offer_detail(indices)   # 将数字序列输入到函数中，即可爬取数据，运行不到半分钟
nrow(rawdata) ; ncol(rawdata)         # 共245个样本单元，9个变量


## Organizing the rawdata

rawdata_revised = na.omit(rawdata)    # 去除缺省样本单元

nrow(rawdata_revised)                 # 剩余203个

# 将结果简化为Offer和Rejected两类
levels(rawdata_revised$result)[levels(rawdata_revised$result) %in% c("AD无奖","AD小奖","offer")] = "Offer"
levels(rawdata_revised$result)[levels(rawdata_revised$result) %in% c("被拒","Wailting list")] = "Rejected"

write.table(rawdata_revised, file = "rawdata_revised.csv", sep = ",", row.names = FALSE)


###--------------------------------------------------------------------------------------
### 2 Description
###--------------------------------------------------------------------------------------

data_all = read.csv(file = "郑昊亮_2016310868_SCEXAM.csv")    # 此处读取邮件附件中的最终数据
# 由于csv文件没有工作表，因此该文件中包含所有原始及修改数据，原始位于修改的左侧，以 _raw 标注

## 数字描述

# 整体
nrow(data_all) ; ncol(data_all)             # 数据中包含202个样本单元，12个特征

# result_raw
levels(data_all$result_raw)                 # result_raw 的5个水平
table(data_all$result_raw)/nrow(data_all)   # 统计比例           

# result
levels(data_all$result)                     # result 的2个水平
table(data_all$result)/nrow(data_all)       # 统计比例

# apply_for_raw
length(levels(data_all$apply_for_raw))      # 共63个水平，意味着有63所不同的美国学校
colleges = table(data_all$apply_for_raw)[table(data_all$apply_for_raw)>4]
colleges[order(colleges)]/nrow(data_all)    # 只选取申请大于4的学校予以展示

# apply_for
length(levels(data_all$apply_for))          # 经转化后仅剩7个水平
table(data_all$apply_for)/nrow(data_all)    # 统计比例


Offer = ( data_all$result == "Offer" )
Rejected = ( data_all$result == "Rejected" )
mysummary = function(data)
{
  summary = rbind(summary(data),summary(data[Offer]),summary(data[Rejected]))                          
  
  return(summary)
}

# gpa
mysummary(data_all$gpa)             # gpa的数字特征,从上至下依次为总体，被录取者和被拒绝者

# TOEFL
mysummary(data_all$TOEFL)           # TOEFL的数字特征,从上至下依次为总体，被录取者和被拒绝者

# GRE
mysummary(data_all$GRE)             # GRE的数字特征,从上至下依次为总体，被录取者和被拒绝者

# from
length(levels(data_all$from))               # 共5个水平
table(data_all$from)/nrow(data_all)         # 统计比例

## 图形描述

library(ggplot2)              # install.packages("ggplot2")

# result_raw
result_raw = as.data.frame(table(data_all$result_raw))
result_label = as.vector(result_raw$Var1)
result_label = paste(result_label, "(", round( result_raw$Freq / sum(result_raw$Freq) * 100), "%)", sep = "")

ggplot(result_raw, aes(x="",y=Freq, fill = Var1)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_fill_discrete(breaks = result_raw$Var1, labels = result_label) + 
  theme(axis.text.x = element_blank())

# gpa
ggplot(data_all,aes(x=gpa,fill=result)) +
  geom_histogram(bins = 9)

# TOEFL
ggplot(data_all,aes(x=TOEFL,fill=result)) +
  geom_histogram(bins = 6)

# GRE
ggplot(data_all,aes(x=GRE,fill=result)) +
  geom_histogram(bins = 6)

# from
from = as.data.frame(table(data_all$from))
form_label = as.vector(from$Var1)
form_label = paste(form_label, "(", round( from$Freq / sum(from$Freq) * 100), "%)", sep = "")

ggplot(from, aes(x="",y=Freq, fill = Var1)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) + 
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_fill_discrete(breaks = from$Var1, labels = form_label) + 
  theme(axis.text.x = element_blank())


###--------------------------------------------------------------------------------------
### 3 Modeling
###--------------------------------------------------------------------------------------

## My codes of NaiveBayes

myNaiveBayes = function( train, predict, ny, lambda = 0)
{
  ## 确认输入数据的正确性
  if(!is.data.frame(train)|!is.data.frame(predict))
  {
    stop("The class of training set and testing set should be dataframe")
  }
  
  if(ncol(train)!=(ncol(predict)+1))
  {
    stop("The column numbers of training set and testing set should be the same")
  }
  
  for(i in 1:ncol(train[,-ny]))
  {
    if(class(train[,-ny][,i])!=class(predict[,i]))
      {
      stop( paste("The class of column",as.character(i),"between train and predict differs!",sep="" ) )
      }
  }
  
  if(!is.factor(train[,ny]))
  {
    stop("The class of y should be factor")
  }
  
  if(length(levels(train[,ny]))!=2)
  {
    stop("There should be only 2 levels")
  }
  
  ## 处理输入的数据
  y1 = train[,ny]
  X1 = train[,-ny]

  X2 = predict
  
  c1 = levels(y1)[1]
  c2 = levels(y1)[2]
  
  m = dis = con = vector(mode="numeric", length=0)
  
  for(i in 1:ncol(X1))
  {
    if(is.factor(X1[1,i]))
    {
      dis = c(dis,i) # 统计离散型数据所在列
      mt = length(levels(X1[,i]))
      m = c(m,mt)    # 每类离散型数据的水平数
    }
    
    if(is.numeric(X1[1,i])){ con = c(con,i) } # 统计连续型数据所在列
  }
  
  mm = max(m)
  n = length(dis)
  
  
  ## 学习过程
  
  yc1 = y1==c1
  yc2 = y1==c2
  Pri1 = ( sum(yc1) + lambda ) / ( nrow(X1) + 2*lambda )    # 首先可以计算先验概率
  Pri2 = ( sum(yc2) + lambda ) / ( nrow(X1) + 2*lambda )
  Pri = c( Pri1 , Pri2 )
  
  # 对于条件概率，要分离散型特征和连续型特征来考虑
  
  # 离散型数据--多项式模型
  
  pos_dis = array(rep(NA,2*mm*n),c(2,mm,n))   # 创建一个数组,每一层代表不同的特征，在同一层中，
                                              # 行代表标记的类，列代表该特征的不同的水平，
                                              # 交点存储的便是该特征的某一水平对应的条件概率
  
  for(i in 1:n)       # 遍取n个离散型特征
  {
    for(j in 1:m[i])  # 考察每个特征的水平，第i个特征共m[i]个水平
    {
      tz = ( X1[,dis[i]] == levels(X1[,dis[i]])[j] )
      
      pos_dis[1,j,i] = ( sum(yc1&tz) + lambda ) / ( sum(yc1) + n*lambda )   # 填充数组
                                                                            # 构建一个条件概率表
      pos_dis[2,j,i] = ( sum(yc2&tz) + lambda ) / ( sum(yc2) + n*lambda )
    }
  }
  
  # 连续型数据--高斯模型
  
  n = length(con)
  
  mu = sig = matrix(rep(NA,2*n),2,n)
  
  mu[1,] = apply(X1[,con][yc1,],2,mean)     # 计算均值和标准差
  mu[2,] = apply(X1[,con][yc2,],2,mean)
  
  sig[1,] = apply(X1[,con][yc1,],2,sd)
  sig[2,] = apply(X1[,con][yc2,],2,sd)
                                            # 输入是类的取值以及特征的取值，
                                            # 输出则是对应的正态概率密度
  pos_con = function(yc,x)
  {
    x = as.numeric(x)
    mu = mu[yc,]
    sig = sig[yc,]
    prod( dnorm(x, mean = mu, sd = sig) )
  }
  
  
  ## 分类过程
  
  ypre = rep(NA,nrow(X2))
  
  
  for(i in 1:nrow(X2))
  {
    pos_dis_1 = pos_dis_2 = 1
    
    for(j in 1:length(dis))
    {
      k = which( X2[i,][,dis[j]] == levels(X2[,dis[j]]) )
      
      pos_dis_1 = pos_dis_1 * pos_dis[1,k,j]           # 对照离散型的条件概率表
      
      pos_dis_2 = pos_dis_2 * pos_dis[2,k,j]
    }
    
    P1 = pos_dis_1 * pos_con(1,X2[i,][,con]) * Pri[1]  # 后验概率是三个概率的乘积
    P2 = pos_dis_2 * pos_con(2,X2[i,][,con]) * Pri[2]
    
    if( P1 >= P2 ){ ypre[i] = levels(y1)[1] }          # 后验概率比大小
    else { ypre[i] = levels(y1)[2] }                   # 将大的类作为输出
    
  }
  
  
  result = list( "class" = ypre)
  return(result)
}


## Test and result

acc = function(test,predict)
{
  accuracy = sum( predict$class == test$result ) / nrow(test)
  return(accuracy)
}

set.seed(10)
sub = sample( 1:nrow(data_all) , round( 0.7*nrow(data_all) ) )

data_train = data_all[sub,c(3,5,8,9,10,12)]         # 取70%的数据做训练集
data_test = data_all[-sub,c(3,5,8,9,10,12)]         # 取30%的数据做测试集
data_predict = data_test[,-1]

result = myNaiveBayes(data_train,data_predict,1)
result

sum(data_test$result == "Offer")/nrow(data_test)    # 测试集中录取率为70.4%

acc(data_test,result)                               # 此时朴素贝叶斯法的准确率为83.6%


###--------------------------------------------------------------------------------------
### 4 Comparison
###--------------------------------------------------------------------------------------

## NaiveBayes in R Packages

library(klaR)                 # install.packages("klaR")

fit_NB = NaiveBayes(result~apply_for+gpa+TOEFL+GRE+from, data = data_train)

# 对比
predict_NB = predict(fit_NB,data_predict)

acc(data_test,predict_NB)     # R包的预测准确率与我的函数的准确率相同

# 预测正确与错误的位置
my_correct = (result$class == data_test$result)
Rpackage_correct = (as.numeric(predict_NB$class)==as.numeric(data_test$result))

my_correct == Rpackage_correct
prod( my_correct== Rpackage_correct ) # R包的预测与我的函数的预测也完全相同
sum( my_correct == Rpackage_correct ) == length(my_correct)


# 以下过程说明，改变训练集与测试集的划分，R包的预测与我的函数的预测也完全相同

for(i in 1:10)
{
  set.seed(i)
  sub = sample( 1:nrow(data_all) , round( 0.7*nrow(data_all) ) )
  data_train = data_all[sub,c(3,5,8,9,10)]
  data_test = data_all[-sub,c(3,5,8,9,10)]
  data_predict = data_test[,-1]
  
  result = myNaiveBayes(data_train,data_predict,1,1)
  
  fit_NB = NaiveBayes(result~apply_for+gpa+TOEFL+GRE, data = data_train,fL=1)
  predict_NB = predict(fit_NB,data_predict)
  
  my_correct = result$class == data_test$result
  Rpackage_correct = as.numeric(predict_NB$class)==as.numeric(data_test$result)
  
  if( prod(my_correct== Rpackage_correct) != 1 )
  {
    stop("我的预测与R包的预测不相同！")         # 没有出现报错，就说明完全一致
  }
}


## Compared with other methods

set.seed(48)
sub = sample( 1:nrow(data_all) , round( 0.7*nrow(data_all) ) )
data_train = data_all[sub,c(3,5,8,9,10,12)]
data_test = data_all[-sub,c(3,5,8,9,10,12)]

## Logistic Regression
#效果并不理想

fit_LR = glm(result~apply_for+gpa+TOEFL+GRE+from,data = data_test,family = binomial)
summary(fit_LR)

predict_LR = predict(fit_LR, newdata = data_test, type = "response")
predict_LR[predict_LR>=0.5] = "Offer"
predict_LR[predict_LR<0.5] = "Rejected"
predict_LR                                     # 预测结果

sum (predict_LR == data_test$result) / nrow(data_test)    # 准确率仅16.4%

## Decision Tree

library(rpart)                # install.packages("rpart")
library(rpart.plot)           # install.packages("rpart.plot")

fit_rp <- rpart(result~apply_for+gpa+TOEFL+GRE+from,data = data_train,method = "class")

prp(fit_rp, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")

predict_rp = predict(fit_rp,data_test)
predict_rp = predict_rp[,1]>=0.5
predict_rp[predict_rp == TRUE] = "Offer"
predict_rp[predict_rp == FALSE] = "Rejected"
predict_rp                                      # 预测结果

sum (predict_rp == data_test$result) / nrow(data_test)    # 准确率为72.1%

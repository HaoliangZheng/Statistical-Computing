##vector
#create
a=c(1,2,3,4);a
b=rep(1,4);b
d=rep(c(1,2,3,4),3);d
rep(c(1,2,3),length.out=4)
seq(1,10)
seq(1,10,by=2)
seq(1,10,length.out=6)
#add
c=c(a,b)
c
#index
c[2]
c[-1]
#func
which(c!=1)
length(c)
class(c)
typeof(c)
sqrt(c)
prod(c)
sum(c)
max(c)
min(c)
#transfor
cbind(a,b)            #to matrix
rbind(a,b)
matrix(b,nrow=2,ncol=2)
array(b,dim=c(2,2))       
array(d,dim=c(2,2,3)) #to array
as.data.frame(b)      #to dataframe
#features
typeof(a)
a[1]="test"
typeof(a)             #only the same type

##matrix
#create
mat1=matrix(1:16,4);mat1
mat2=diag(4);mat2
#add
cbind(b,mat1);cbind(mat1,b)
rbind(b,mat1);rbind(mat1,b)
#index
mat1[1,1]
mat1[1,]
mat1[,1]
#func
dim(mat1)
nrow(mat1);ncol(mat1)
mat=mat1+mat2
mat1*mat2
mat%*%mat2
t(mat)
det(mat)
solve(mat)
eigen(mat)
qr(mat)$rank
#transfor
mat[1,]            #to vector
#features
typeof(mat)
mat[1,1]="test"
typeof(mat)          #only the same type

##array
#create
arr=array(1:24,c(2,3,4));arr
#add
library(abind)
abind(arr,arr,along=1)
abind(arr,arr,along=2)
abind(arr,arr,along=3)
#index
arr[1,1,1]
arr[,,1]
arr[1,2:3,2:3]
#func
apply(arr,1,sum)
apply(arr,2,sum)
#transfor
arr[,1,1]             #to vector
arr[,,1]              #to matrix
as.data.frame(arr)    #to dataframe
#features
typeof(arr)
arr[1,1,1]="test"
typeof(arr)           #only the same type

##dataframe
#create
name=c("zhang3","li4","Wang5")
ID=c(868,869,900)
score=c(100,99,50)
data=data.frame(name,ID,score,stringsAsFactors = FALSE)
data
#add
class=c("A","B","A")
data2=cbind(data,class);data2
#index
data[1,]
data[,1]
data$name
data[c("ID","name")]
ID[1]
#func
data2=data.frame(ID2=c(868,869,900),course=c("English","Math","Chinese"))
merge(data,data2,by.x="ID",by.y="ID2")  #merge two dataframes by the key
#transfor
as.matrix(cbind(data[,2],data[,3])) #to matrix
#features
typeof(data$name)
typeof(data$ID)       #can be different between columns
data$ID[1]="test"
typeof(data$ID)       #must be the same in one column

##list
#create
info1=info2=list(zhang3=17,li4=28,wang5=99);info
#add
info2$zhang3=list(zhao6=100,state="new")
info2
#index
info1[1]
info[[1]]
info2$li4
info2$zhang3$state
#func
length(info1)
names(info1)
unlist(info1)
lapply(info1,log)
rapply(info1,sqrt)
rapply(info1,function(x) x^2,how="replace")
#transfor
unlist(info1)       #to vector
do.call(rbind,info2)#to matrix
#features
class(info2$zhang3)
class(info2$zhang3$zhao6)
class(info2$zhang3$state)  #can be different
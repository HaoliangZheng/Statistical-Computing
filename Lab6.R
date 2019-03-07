loglikelihood = function(x,mu,sig)
{
  sum(dnorm(x,mu,sig,log = TRUE))
}

#鈥斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€?

MLE = function(x,mu,sig,n)     # x涓轰竴缁勮娴嬪€?,mu涓哄潎鍊煎彲鑳界殑鍙栧€艰寖鍥?
{                              # sig涓烘爣鍑嗗樊鍙兘鐨勫彇鍊艰寖鍥达紝n涓哄惊鐜鏁?
  f1 = rep(NULL,length(mu))
  fr1 = c(rep(NULL,n));
  f2 = rep(NULL,length(sig))   # f鐢ㄦ潵璁板綍姣忎釜鍧囧€兼垨鏍囧噯宸搴旂殑loglikelihood
  fr2 = c(3200,rep(NULL,n))    # fr鐢ㄦ潵璁板綍鍏朵腑鐨勬渶浼樺€煎搴旂殑鍧囧€兼垨鏍囧噯宸?
                               # 3200涓虹涓€娆¤緭鍏ョ殑鐢ㄦ潵姹俶u鐨剆ig鍊?
  for(i in 1:n)
  {
    for(j in 1:length(mu))
    {
      f1[j] = loglikelihood(x,mu[j],fr2[i])
    }
    fr1[i] = mu[which.max(f1)]
    
    for(k in 1:length(sig))
    {
      f2[k] = loglikelihood(x,fr1[i],sig[k])
    }
    fr2[i+1] = sig[which.max(f2)]
  }
  data.frame(mu = fr1,sig = fr2[1:n])
}

x = c(20738,19967,18889,15889,15872,13488,13309,12943,12528,11918)
mu = seq(15000,16000,0.1)
sig = seq(3000,3500,0.1)
n = 10
MLE(x,mu,sig,n)
# 鎴戜滑鍙戠幇绗簩娆＄粨鏋滀箣鍚庣殑鎵€鏈塵u鍜宻ig閮戒笌绗簩娆＄粨鏋滅浉鍚?
# 杩欐牱鐨勬儏鍐靛苟闈炴槸宸у悎锛岃€屾槸蹇呯劧鍙戠敓鐨?
# 鍥犱负閫氳繃姝ｆ€佸瘑搴﹀嚱鏁版眰瀵规暟鍐嶆眰鍜岀殑琛ㄨ揪寮忓彲浠ュ彂鐜?
# mu鐨勬渶浼樺€煎ぇ灏忎笌sig鏃犲叧锛屼篃灏辨槸璇达紝杈撳叆浠绘剰鐨剆ig锛岄兘灏嗗緱鍒板悓涓€涓猰u
# 鍦╩u鍙栧埌鏈€浼樺€煎浐瀹氫笅鏉ヤ箣鍚庯紝sig涔熷氨鍥犺€屽浐瀹氫笅鏉?
# 鎵€浠ュ悗闈㈢殑寰幆瀹為檯涓婃槸鏃犳晥鐨?
# 鐢ㄨ窛绂诲皬浜庢煇涓€鍊兼潵涓寰幆鐨勬柟娉曚篃鏄棤鏁堢殑锛屽洜涓哄樊蹇呭皢涓?0

#鈥斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€斺€?

# 鍙互閫氳繃鏀瑰彉姝ラ暱鏉ュ睍鐜板嚭鍙樺寲锛屼綋鐜板惊鐜殑浣滅敤
MLEcs = function(x,mu,sig,n)     
{ 
  stepm = rep(NULL,length(n));stepm[1] = mu[2]-mu[1]
  steps = rep(NULL,length(n));steps[1] = sig[2]-sig[1]          # 璁板綍鏈€鍒濈殑姝ラ暱
  f1 = rep(NULL,length(mu))
  fr1 = c(rep(NULL,n));
  f2 = rep(NULL,length(sig))   
  fr2 = c(3200,rep(NULL,n))    
  
  for(i in 1:n)
  {
    for(j in 1:length(mu))
    {
      f1[j] = loglikelihood(x,mu[j],fr2[i])
    }
    fr1[i] = mu[which.max(f1)]
    
    stepm[i+1] = (stepm[i]/10)                                  # 鏇存柊姝ラ暱
    mu = rep(NULL,length(mu)) 
    mu = seq(fr1[i]-10*stepm[i],fr1[i]+10*stepm[i],stepm[i+1])  # 鏇存柊鍙栧€艰寖鍥? 
    
    for(k in 1:length(sig))
    {
      f2[k] = loglikelihood(x,fr1[i],sig[k])
    }
    fr2[i+1] = sig[which.max(f2)]
    
    steps[i+1] = (steps[i]/10)
    sig = rep(NULL,length(sig))
    sig = seq(fr2[i]-10*steps[i],fr2[i]+10*steps[i],steps[i+1])
  }
  data.frame(mu = fr1,sig = fr2[1:n])
}

mu2 = seq(15000,16000,1000)
sig2 = seq(3000,3500,100)
n2 = 8
MLEcs(x,mu2,sig2,n2)

# 3.已知各自的起手牌，pre_flop阶段，求各自胜率 ------------------------------
m_p2win=matrix(0L,nr=choose(169,2),nc=2)
m_bd5=t(combn(48,5))
v_id71=vector('integer',nrow(bd5))
v_id72=vector('integer',nrow(bd5))
n=1
for(i in 1:19){
  v_p1=m_c2sam[i,1:2]
  for(j in (i+1):20){
    v_p2=m_c2sam[j,1:2]
    bd5=matrix(v_52[-c(v_p1,v_p2)][m_bd5],nc=5)
    
    for(k in 1:nrow(bd5)){
      v_id71[k]=match_mc7(.Internal(qsort(c(v_p1,bd5[k,]),F)))
      v_id72[k]=match_mc7(.Internal(qsort(c(v_p2,bd5[k,]),F)))
    } 
    
    i1=m_c5[v_c7[v_id71],7]
    i2=m_c5[v_c7[v_id72],7]
    m_p2win[n,1]=sum(i1<i2)
    m_p2win[n,2]=sum(i1==i2)
    n=n+1
  }
}


f_p2win=function(x){
  m_p2win=matrix(0L,nr=choose(169,2),nc=2)
  m_bd5=t(combn(48,5))
  v_id71=vector('integer',nrow(bd5))
  v_id72=vector('integer',nrow(bd5))
  n=1
  for(i in 1:(nrow(x)-1)){
    v_p1=x[i,]
    for(j in (i+1):nrow(x)){
      v_p2=x[j,]
      bd5=matrix(v_52[-c(v_p1,v_p2)][m_bd5],nc=5)
      
      for(k in 1:nrow(bd5)){
        v_id71[k]=match_mc7(.Internal(qsort(c(v_p1,bd5[k,]),F)))
        v_id72[k]=match_mc7(.Internal(qsort(c(v_p2,bd5[k,]),F)))
      } 
      
      i1=m_c5[v_c7[v_id71],7]
      i2=m_c5[v_c7[v_id72],7]
      m_p2win[n,1]=sum(i1<i2)
      m_p2win[n,2]=sum(i1==i2)
      n=n+1
    }
  }
}


library(foreach)
# 启用parallel作为foreach并行计算的后端
library(doParallel)
func=function(x){
  return (x+1);
}
cl <- makeCluster(4)
registerDoParallel(cl)
# 并行计算方式
tt=Sys.time()
x <- foreach(x=matrix(1:100,nc=2),.combine='rbind') %dopar% func(x)
Sys.time()-tt
stopCluster(cl)


m_p2win[1:6,1]/1712304
m_p2win[1:6,2]/1712304
1-(m_p2win[1:6,1]/1712304+m_p2win[1:6,2]/1712304)
100*(m_p2win[1:6,1]/1712304+m_p2win[1:6,2]/1712304/2)



# 1.已知各自的起手牌，pre_flop阶段，求各自胜率 ------------------------------
n=sort(unique(as.vector(m_c2sam[,1:2])))

# m_c2sam中共26张
m_p26in5=matrix(0L,nr=249900,nc=26)# 每张牌在m_c5中的哪一行出现
for(j in 1:26)# 每张牌在m_c5中的哪一行、列出现过（只保留行标）
  m_p26in5[,j]=which(m_c5[,1:5]==n[j],arr.ind=T)[,1]

# 将每张牌的行标扩充至2598960，出现为1，否则为0
m=matrix(0L,nrow=2598960,ncol=26)
for(i in 1:26) m[m_p26in5[,i],i]=1

m_c2in5=matrix(0L,nr=19600,nc=169)# c2在m_c5里的行标，长度均为19600
for(i in 1:169){
  j=m_c2sam[i,1:2]
  m_c2in5[,i]=which(m[,j[1]==n] & m[,j[2]==n])
}

rm(m,n,m_p26in5)

# 根据m_c2in5里每列的行标，得出对应的type和rank，19600x169
m_c2in5_type=apply(m_c2in5,2,function(x) m_c5[x,6])
m_c2in5_type=apply(m_c2in5_type,2,sort)
# 不同的c2对应的类型个数不同，pair:5，suit:(9,7)，offsuit:(7,6)
apply(m_c2in5_type,2,function(x) (length(unique(x))))

m_c2in5_rank=apply(m_c2in5,2,function(x) m_c5[x,7])
# apply(m_c2in5_rank, 2, is.unsorted)
# m_c2in5_rank进行排序
m_c2in5_rank=apply(m_c2in5_rank,2,sort)

# k_p:pair, k_s:suit, k_o:offsuit
k_p=m_c2in5_rank[,1:13]
k_s=m_c2in5_rank[,14:91]
k_o=m_c2in5_rank[,92:169]

# pair 454种rank（13个），suit 620（78个，offsuit 455（78个））
unique(apply(k_p,2,function(x) (length(unique(x)))))
unique(apply(k_s,2,function(x) (length(unique(x)))))
unique(apply(k_o,2,function(x) (length(unique(x)))))





------------------------------
i1=m_c2in5_rank[,1]
i2=m_c2in5_rank[,7]
i=1;j=1;k=vector('integer',0)
while(i<=length(i1) && j<=length(i2)){
  if(i1[i]<i2[j]){
    k=append(k,1);i=i+1
  }else if(i1[i]==i2[j]){
    k=append(k,NA);i=i+1;j=j+1
  }else{
    k=append(k,0);j=j+1
  }
}

if(i<=length(i1)){
  k=append(k,rep(0,length(i1)-i+1))
}else if (j<=length(i2)){
  k=append(k,rep(1,length(i2)-j+1))
}

c(3,1,1)
c(2,1,2)
c(1,1,3)
c(0,1,4)
c(0,0,5)

k=1
for(i in 1:51)
  for(j in (i+1):52)
    {print(k);k=k+1}


i1_300=m_c2in5_rank[,c(1,300)]
i1_300=apply(i1_300, 2, unique)
i1_300=do.call(cbind,i1_300)







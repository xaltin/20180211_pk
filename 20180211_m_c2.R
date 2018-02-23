# 已知各自的起手牌，pre_flop阶段，求各自胜率 ------------------------------
n=sort(unique(as.vector(m_c2sam[,1:2])))

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






# tag ---------------------------------------------------------------------
i=m_c5[,1:2]
j=data.frame(table(i[,2],i[,1]))







# 1.将m_c2lite的因子加入到d_wp_my
# 2.按照每种类型（9列）对169种m_c2lite进行绘图，根据因子进行着色
# 3.


d_wp_my_pct=d_wp_my[,2:10]*100/N
rownames(d_wp_my_pct)=d_wp_my$name

d_wp_my_odd=round(N/d_wp_my[,2:10],2)
rownames(d_wp_my_odd)=d_wp_my$name




d_c5stat$tp_5in7_cnt=colSums(d_wp_my[,2:10])
d_c5stat$tp_5in7_pct=d_c5stat$tp_5in7_cnt*100/(169*N)
d_c5stat$tp_5in7_odd=round(169*N/d_c5stat$tp_5in7_cnt,2)



# 1.monte'carlo for m_c2's winning probability ----------------------
v_52=1:52
N=1000
# v_oper=1# 暂未使用
v_op2=vector('integer',2)# 对手2张手牌
v_bd5=vector('integer',5)# 对手2张手牌

m_my=matrix(nr=N,nc=12)
m_op=matrix(nr=N,nc=12)

for(i in 1:nrow(m_c2)){
  
  tt=Sys.time()
  v_my2=m_c2[i,1:2]# 己方2张手牌
  for(j in 1:N){
    v_op2=sample(v_52[-v_my2],2)# 对手2张手牌
    v_bd5=sample(v_52[-c(v_my2,v_op2)],5)# 5张公共牌
    
    v_my5=seq5in7(v_my2,v_bd5)
    v_op5=seq5in7(v_op2,v_bd5)
    
    m_my[j,]=d_pk$name[c(v_my2,v_bd5,v_my5)]
    m_op[j,]=d_pk$name[c(v_op2,v_bd5,v_op5)]
  }# for 2
  Sys.time()-tt
  
}#for 1


# 2.monte'carlo for m_c2's winning probability -----------------------
v_52=1:52
N=10000
# v_oper=1# 暂未使用
v_my2=vector('integer',2)# 己方2张手牌
v_op2=vector('integer',2)# 对手2张手牌
v_bd5=vector('integer',5)# 5张公共牌
v_my5=vector('integer',5)# 己方5张成牌
v_op5=vector('integer',5)# 对手5张手牌

m_test=matrix(nr=N,nc=2+5+2+2+2)

tt=Sys.time()
v_my2=as.vector(m_c2[i,1:2])# 己方2张手牌,as.vector取消name
for(j in 1:N){
  v_op2=sample(v_52[-v_my2],2)# 对手2张手牌
  v_bd5=sample(v_52[-c(v_my2,v_op2)],5)# 5张公共牌
  
  v_my5=seq5in7(v_my2,v_bd5)#1  2  4 34 35,2134
  v_op5=seq5in7(v_op2,v_bd5)#4 13 19 34 35,798970
  
  v_myrank=m_c5[match_mc5(v_my5),6:7]
  v_oprank=m_c5[match_mc5(v_op5),6:7]
  
  m_test[j,]=c(v_my2,v_bd5,v_op2,v_myrank,v_oprank)
  
}# for 2
Sys.time()-tt

colnames(m_test)=1:13

w_win=sum(m_test[,11]<m_test[,13])/N
w_equal=sum(m_test[,11]==m_test[,13])/N
w_lost=1-w_win-w_equal





m11=m_c5[1:249900,1:5]
m12=m11[1:19600,]
m13=m12[1:1176,]

c12=data.frame(table(m11[,2]))
c13=data.frame(table(m12[,3]))
c14=data.frame(table(m13[,4]))

m21=m_c5[249900+1:230300,1:5]
m22=m21[1:18424,]
m23=m22[1:1128,]

c22=data.frame(table(m21[,2]))
c23=data.frame(table(m22[,3]))
c24=data.frame(table(m23[,4]))





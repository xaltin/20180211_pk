# 1. 己方2张手牌的wp ----------------------------------------------
v_52=1:52
N=100000
# 记录169种起手牌的9种牌型的出现次数，以及胜、平次数，运行169*2*10w=3380w次
m_wp_my=matrix(nr=nrow(m_c2sam),nc=11)

tt=Sys.time()
for(i in 1:nrow(m_c2sam)){
  v_my2=as.vector(m_c2sam[i,1:2])# 己方2张手牌,as.vector取消name
  
  v_wp_my=vector('integer',11)# 记录9种牌型的出现次数，以及胜、平次数
  for(j in 1:N){# for 2
    v_op2=sample(v_52[-v_my2],2)# 对手2张手牌
    v_bd5=sample(v_52[-c(v_my2,v_op2)],5)# 5张公共牌
    
    rn_my=seq5in7(v_my2,v_bd5)# 返回v_my5的行索引
    rn_op=seq5in7(v_op2,v_bd5)# 返回v_op5的行索引
    
    # 命中某种类型，则某种类型+1
    v_wp_my[m_c5[rn_my,6]]=v_wp_my[m_c5[rn_my,6]]+1
    # 记录胜、平次数
    if(m_c5[rn_my,7]<m_c5[rn_op,7])
      v_wp_my[10]=v_wp_my[10]+1
    else if(m_c5[rn_my,7]==m_c5[rn_op,7])
      v_wp_my[11]=v_wp_my[11]+1
  }# for 2
  m_wp_my[i,]=v_wp_my
}
Sys.time()-tt

d_wp_my=data.frame(paste(d_pk$name[m_c2sam[,1]],d_pk$name[m_c2sam[,2]],
                         c(rep('p',13),rep('s',78),rep('o',78)),sep=''),
                   m_wp_my,N-m_wp_my[,10]-m_wp_my[,11],
                   stringsAsFactors=F)

colnames(d_wp_my)=c('name','rflush','four','house','flush','straight',
                    'three','tpair','pair','high','win','equal','lost')

# # 169种起手牌，己方和对手起手牌各运行10w次，耗时77.7 mins
# saveRDS(d_wp_my,'data/d_wp_my_169_10w.RData')
# write.csv(d_wp_my,'data/d_wp_my_169_10w.csv')


# 2. 对d_wp_my 进行统计分析-------------------------------------------
# 加载
d_wp_my=readRDS('data/d_wp_my_169_10w.RData')

# 1. 13x13 matrix for 2 people's 169's wp
i=round((d_wp_my$win+d_wp_my$equal/2)*100/N,1)

m_p2_13x13=diag(i[1:13],13,13)
m_p2_13x13[lower.tri(m_p2_13x13)]=i[14:91]
m_p2_13x13=t(m_p2_13x13)
m_p2_13x13[lower.tri(m_p2_13x13)]=i[92:169]
rownames(m_p2_13x13)=c('A','K','Q','J','T',9:2)
colnames(m_p2_13x13)=c('A','K','Q','J','T',9:2)
# write.csv(m_p2_13x13,'data/m_p2_13x13_10w.csv')

# 1. matrix for 169's wp
d_wp_my_pct=d_wp_my[,2:10]*100/N
rownames(d_wp_my_pct)=d_wp_my$name

d_wp_my_odd=round(N/d_wp_my[,2:10],2)
rownames(d_wp_my_odd)=d_wp_my$name


# 3. 7张牌中各种牌型的出现概率，100m次，耗时45.3 mins----------------
v_52=1:52
N=100000000
v_type_5in7=vector('integer',9)
names(v_type_5in7)=c('rflush','four','house','flush','straight',
                    'three','tpair','pair','high')

tt=Sys.time()
for(i in 1:N){# for 2
  v_7=sample(v_52,7)# 随机7张牌
  rn_5=seq5in7(v_7[1:2],v_7[3:7])#己方5张成牌
  v_type_5in7[m_c5[rn_5,6]]=v_type_5in7[m_c5[rn_5,6]]+1
}
Sys.time()-tt

# saveRDS(v_type_5in7,'data/v_type_5in7_100m.RData')

v_type_5in7=readRDS('data/v_type_5in7_100m.RData')

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





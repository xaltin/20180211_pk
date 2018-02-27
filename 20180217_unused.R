# m_c5lite ----------------------------------------------------------------
m_1459=t(combn(1:13,5))
m_1459_id=cumsum(table(m_1459[,1]))
v_15=c(c(0,m_1459_id[-9])+1,m_1459_id[1])
v_49=(1:choose(13,5))[-v_15]
m_1=cbind(m_1459[v_15,],1,1:10,4)
m_4=cbind(m_1459[v_49,],4,1:1277+322,4)
m_5=cbind(m_1459[v_15,],5,1:10+1599,1020)
m_9=cbind(m_1459[v_49,],9,1:1277+6185,1020)

i=vector('integer',156*2);j=0
for(k in 1:13)
  for(l in (1:13)[-k]){i[j+1:2]=c(k,l);j=j+2}

m_2=matrix(i,156,2,byrow=T)
m_3=cbind(m_2[,c(rep(1,3),2,2)],3,1:156+166,24)
m_2=cbind(m_2[,c(rep(1,4),2)],2,1:156+10,4)

i=vector('integer',858*3);j=0
for(k in 1:12)
  for(l in (k+1):13)
    for(m in (1:13)[-c(k,l)]){i[j+1:3]=c(k,l,m);j=j+3}

m_7=matrix(i,858,3,byrow=T)
m_6=m_7[order(m_7[,3],m_7[,1],m_7[,2]),c(3,1,2)]
m_6=cbind(m_6[,c(rep(1,3),2,3)],6,1:858+1609,64)
m_7=cbind(m_7[,c(rep(1:2,each=2),3)],7,1:858+2467,144)

i=vector('integer',2860*4);j=0
for(k in 1:11)
  for(l in (k+1):12)
    for(m in (l+1):13)
      for(n in (1:13)[-c(k,l,m)]){i[j+1:4]=c(k,l,m,n);j=j+4}
m_8=matrix(i,2860,4,byrow=T)
m_8=cbind(m_8[order(m_8[,4],m_8[,1],m_8[,2],m_8[,3]),c(4,4,1:3)],8,1:2860+3325,384)

# 每一种组合未排序
m_c5lite=do.call(rbind,mget(paste('m_',1:9,sep='')))
colnames(m_c5lite)=c(paste('c5_',1:5,sep=''),'type','rank','rank_cnt')

# 对每一种组合进行排序
m_c5lite[,1:5]=t(apply(m_c5lite[,1:5],1,sort))

# m_c5_in2 ----------------------------------------------------------------
# 计算5comb中2comb（10个）各自的prime_prod，2598960*10
i=combn(1:5,2)
for(j in 1:10){
  assign(paste('k',j,sep=''),
         (d_pk$prime[m_c5[,i[1,j]]])*(d_pk$prime[m_c5[,i[2,j]]]))
}
m_c2_in5=do.call(cbind,mget(paste('k',1:10,sep='')))
rm(i,j,list=paste('k',1:10,sep=''))
# apply(m_c2_in5[1:100,],1,is.unsorted) # 每行并未排序

# !!!指定levels和labels的性能提升6-7倍，只指定levels性能提升会稍高, about 11 seconds
m_c5_in2=factor(m_c2_in5,levels=m_c2[,'prm_prd'],labels=1:1326,ordered=T)
# !!!m_c5在m_c2中的序号,19600*1326, 利用names映射进行查找，about 13 seconds
names(m_c5_in2)=rep(1:2598960,10)
# attributes(m_c5_in2)
m_c5_in2=matrix(as.integer(names(sort(m_c5_in2))),19600,1326)
m_c5_in2=apply(m_c5_in2,2,sort)
dimnames(m_c5_in2)[[2]]=dimnames(m_c2)[[1]]

i=cbind(m_c5_in2[,1:6],matrix(m_c5[,6][m_c5_in2[,1:6]],19600,6),
        matrix(m_c5[,7][m_c5_in2[,1:6]],19600,6))
j=t(apply(i[,7:12], 1, unique))
k=t(apply(i[,13:18], 1, unique))

# sort是很快的
# sort(m_c2_in5)

# !!!comb2在comb5中的序号, 2598960*10，m_c2_in5目前用不着，但利用映射进行查找的思想值得推荐
# m_c2_in5=matrix(as.integer(m_cmb5_in2),nrow=2598960,ncol=10)
rm(m_c2_in5)

# m_c2lite ----------------------------------------------------------------
m_c2lite=rbind(matrix(rep(1:13,each=2),13,2,byrow=T),
               t(combn(13,2))[rep(1:78,2),])
i=m_c2lite[,2]-m_c2lite[,1]
brg=brg1=brg2=brg3=vector('integer',169)
brg[which(i==1)]=1;brg1[which(i==2)]=1
brg2[which(i==3)]=1;brg3[which(i==4)]=1
m_c2lite=cbind(m_c2lite,rep(c(6,4,12),c(13,78,78)),
               rep(c(1,0,0),c(13,78,78)),rep(c(0,1,0),c(13,78,78)),
               rep(c(0,0,1),c(13,78,78)),brg,brg1,brg2,brg3)
m_c2lite=cbind(m_c2lite,(m_c2lite[,5]&(m_c2lite[,7]|m_c2lite[,8]|
                                         m_c2lite[,9]|m_c2lite[,10])))
i=c('A','K','Q','J','T',9:2)  
rownames(m_c2lite)=paste(i[m_c2lite[,1]],i[m_c2lite[,2]],
                         {j=ifelse(m_c2lite[,5]==1,'s','o')
                         j[which(m_c2lite[,4]==1)]='p';j},sep='')
colnames(m_c2lite)=c('c2_1','c2_2','type_cnt','pair','suit','offsuit',
                     'brg','brg1','brg2','brg3','suit_brg')
rm(brg,brg1,brg2,brg3,i,j)

# seq5in7 -----------------------------------------------------------------
# seq5in7=function(hand2,board5){
#   cmb=combn(sort(c(hand2,board5)),5)
#   i=apply(cmb,2,function(x) match_mc5(x))
#   i[which.min(m_c5[i,7])]
# }

# seq5in7 这段代码有问题--------------------------------------------------
# seq5in7=function(hand2,board5){
#   m_413=matrix(0,4,13)# 13*4矩阵，计算可能的成牌及成牌的牌力
#   v_7=sort(c(hand2,board5))# 为排序取前5个元素使用sort()
#   m_413[v_7]=1
#   
#   v_sum_c_fc=colSums(m_413)
#   v_sum_r_st=rowSums(m_413)
#   v_seq_r_fc=which(v_sum_c_fc!=0)
#   
#   v_5=0
#   if(length(v_seq_r_fc)>=5){# if牌型>=5，判断是否是同花或顺子
#     i=which(v_sum_r_st>=5)
#     
#     if(length(i)==1){# 如果是同花
#       v_5=v_7[(v_7%%4)==(i%%4)][1:5]
#     } else {# 是否是顺子
#       j=diff(v_seq_r_fc)
#       
#       if(all(j[1:4]==1)){# if j前四个元素==1
#         v_5=v_seq_r_fc[1:5]+c(0:3,0)
#       } else if(length(j)>=5){# if j长度>=5
#         if(all(j[2:5]==1)) v_5=v_seq_r_fc[2:6]+c(0:3,0)
#       } else if(length(j)==6){# if j长度==6
#         if(all(j[3:6]==1)) v_5=v_seq_r_fc[3:7]+c(0:3,0)
#       } else {# 以上都未命中，if j后四个元素==c(9,1,1,1)
#         if(all(j[-3:0+length(j)]==c(9,1,1,1)))
#           v_5=v_seq_r_fc[-4:0+length(v_seq_r_fc)]+c(0:3,0)
#       }
#     }
#   }
#   
#   # 这段代码有问题，对于金刚中的4条和对子处理不正确
#   if(length(v_5)==1){# 既不是同花也不是顺子,v_5未被赋值
#     k=factor(cut(v_7,seq(0,52,4),labels=1:13),
#              levels=order(v_sum_c_fc,decreasing=T),ordered=T)
#     v_5=sort(v_7[order(k)[1:5]])
#   }
#   
#   v_5
# }

# 功能与combn()相同 ------------------------------------------------------------
# m_c5_1=matrix(0L,nr=2598960,nc=5)
# n=1
# for(i in 1:48)
#   for(j in (i+1):49)
#     for(k in (j+1):50)
#       for(l in (k+1):51)
#         for(m in (l+1):52){
#           m_c5_1[n,]=c(i,j,k,l,m)
#           n=n+1
#         }
#           
# all(m_c5_1==m_c5[,1:5])
# 
# 
# 
# 
# write.csv(m_c5[,1:5],'m_c5.csv',row.names=F)

# 已知各自的起手牌，pre_flop阶段，求各自胜率 ------------------------------
# 已验证只有169种起手对子，因此该段代码不用（1326种全组合）
m_p52in5=matrix(0L,nr=249900,nc=52)# 每张牌在m_c5中的哪一行出现
for(i in 1:52)# 每张牌在m_c5中的哪一行、列出现过（只保留行标）
  m_p52in5[,i]=which(m_c5[,1:5]==i,arr.ind=T)[,1]

# 将每张牌的行标扩充至2598960，出现为1，否则为0
n=matrix(0L,nrow=2598960,ncol=52)
for(i in 1:52) n[m_p52in5[,i],i]=1

m_c2in5=matrix(0L,nr=19600,nc=1326)# c2在m_c5里的行标，长度均为19600
k=1
l=vector('character',1326)
for(i in 1:51)
  for(j in (i+1):52){
    m_c2in5[,k]=which(n[,i] & n[,j])
    l[k]=paste(i,j,sep='_')
    k=k+1
  }
colnames(m_c2in5)=l

# 根据m_c2in5里每列的行标，得出对应的type和rank，19600x1326
m_c2in5_type=apply(m_c2in5,2,function(x) m_c5[x,6])
m_c2in5_rank=apply(m_c2in5,2,function(x) m_c5[x,7])
# apply(m_c2in5_rank, 2, is.unsorted)
# m_c2in5_rank进行排序
m_c2in5_rank=apply(m_c2in5_rank,2,sort)

# 使用which可以保留rownames
k_p=m_c2in5_rank[,which(m_c2[,'pair']==1)]
k_s=m_c2in5_rank[,which(m_c2[,'suit']==1)]
k_o=m_c2in5_rank[,which(m_c2[,'offsuit']==1)]

# pair 454种rank（78个），offsuit 455（936个），suit 620（312个）
unique(apply(k_p,2,function(x) (length(unique(x)))))
unique(apply(k_s,2,function(x) (length(unique(x)))))
unique(apply(k_o,2,function(x) (length(unique(x)))))

# 验证哪些c2的rank是一样的，然后去重
# for(i in 1:13){# pair，通过
#   j=k_p[,1:6+(i-1)*6]
#   apply(j, 1, function(x) stopifnot(x==x[1]))
# }
k_plite=k_p[,seq(1,by=6,length.out=13)]

i=t(sapply(strsplit(dimnames(k_s)[[2]],'_'), as.integer))
i=ceiling(i/4)
j=order(i[,1],i[,2])
# for(l in 0:77){# suit，通过
#   m=k_s[,j[1:4+l*4]]
#   apply(m, 1, function(x) stopifnot(x==x[1]))
# }
k_slite=k_s[,j[seq(1,by=4,length.out=78)]]

i=t(sapply(strsplit(dimnames(k_o)[[2]],'_'), as.integer))
i=ceiling(i/4)
is.unsorted(i[,1])# i[,1]已排序
j=order(i[,1],i[,2])
# for(l in 0:77){# offsuit，通过
#   m=k_o[,j[1:12+l*12]]
#   apply(m, 1, function(x) stopifnot(x==x[1]))
# }
k_olite=k_o[,j[seq(1,by=12,length.out=78)]]

# ---------------20180227---------------
# 防止data.frame导致读取性能降低，单独赋值成vector ----------------------------
p_face=d_pk$face
p_suit=rep(1:4,13)
m=matrix(nr=7,nc=5)# 提前创建m，function中使用[,]赋值，seq5in7提升7.8%

# 1: seq5in7 速度较原来快1倍--------------------------------------------------
# 这段代码有问题，没有实现同花顺的判断
seq5in7=function(hand2,board5){
  v_7=sort(c(hand2,board5))
  m[,]=cbind(v_7,0,0,p_face[v_7],p_suit[v_7])
  
  i=1;len=0
  while(i<=7){# 实现了3个功能1.table 2.length 3.unique
    j=i+1
    while(j<=7 && m[j,4]==m[i,4]) j=j+1
    if(j==(i+1)) m[i,2]=1 else m[i:(j-1),2]=j-i
    len=len+1; m[len,3]=i; i=j
  }
  
  v_5=0
  # len>=5，则同花最大，顺子次之
  if(len>=5){# if 1
    n=0
    for(i in 1:4){# 同花
      j=(m[,5]==i); k=sum(j)
      if(k>=5) {v_5=m[j,1][1:5]; break}
      else n=n+k
      
      if(n>=3) break
    }
    
    if(k<5){# 顺子
      i=m[1:len,3]; j=diff(m[i,4])
      
      if(all(j[1:4]==1)) v_5=m[i[1:5],1]
      else if(len>=6) {if(all(j[2:5]==1)) v_5=m[i[2:6],1]}
      else if(len==7) {if(all(j[3:6]==1)) v_5=m[i[3:7],1]}
      else {if(all(j[-3:0+(len-1)]==c(9,1,1,1))) v_5=m[i[-4:0+len],1]}
    }
  }# if 1
  
  # 既不是同花也不是顺子,v_5未被赋值
  if(length(v_5)==1){# if 2
    i=order(m[,2],decreasing=T)
    if(m[i[1],2]==4){# 对金刚（4条+对子+单牌）需区别对待
      j=which(m[,2]==4)
      if(j[1]!=1) v_5=m[c(1,j),1] else v_5=m[1:5,1]
    } else v_5=m[sort(i[1:5]),1]# todo::是否有可能改进
  }# if 2
  
  match_mc5(v_5)# 返回行索引
}


# p_fc=rep(1:13,each=4)
v_7=vector('integer',7)
p_st=rep(1:4,13)
v_7st=vector('integer',7)
m413=matrix(0L,nr=4,nc=13)# 提前创建m，function中使用[,]赋值，性能提升7.8%
m_fc=vector('integer',13)
m_st=vector('integer',4)
v_7fc=vector('integer',7)

# 2: seq5in7 速度较原来快1倍------------------------------------------------
seq5in7=function(hd2,bd5){
  v_7=sort(c(hd2,bd5))
  v_7st=p_st[v_7]
  m413[]=0L
  m413[v_7]=1
  
  m_fc=colSums(m413)
  m_st=rowSums(m413)
  m_sq=which(m_fc>0)
  
  v_5=0
  l_sq=length(m_sq)
  
  # 牌型如果>4，先做是否是同花或顺子的判定；
  # 只要是顺子或同花，则将索引赋值给v_5
  if(l_sq==5){# v_5有可能被赋值
    max_st=which.max(m_st)
    if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
    else{# 如果不是同花，则判定是否是顺子
      if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
        v_5=v_7[cumsum(m_fc[m_sq])]
    }
  }
  
  if(l_sq==6){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
      }
    }
  }
  
  if(l_sq==7){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else if(m_st[max_st]==7){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_5[3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_5[c(1,4:7)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_7[cumsum(m_fc[m_sq])][3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
      }
    }
  } 
  
  # 既不是同花也不是顺子,v_5未被赋值（v_5被赋值的概率为6.6%）
  if(length(v_5)==1){
    v_7fc=rep(m_fc[m_sq],m_fc[m_sq])
    v_7fc_od=order(v_7fc,decreasing=T)
    
    if(v_7fc[v_7fc_od[1]]==4 && l_sq==3){# （4条+对子+单牌）需区别对待
      v_5=sort(c(v_7[v_7fc_od[1:4]],min(v_7[v_7fc_od[5:7]])))
    } else v_5=sort(v_7[v_7fc_od[1:5]])
  }
  
  match_mc5(v_5)# 返回行索引
}

# 3: seq5in7 速度较原来快1倍-----------------------------------------------
seq5in7=function(hd2,bd5){
  v_7=sort(c(hd2,bd5))
  v_7st=p_st[v_7]
  m413[]=0L
  m413[v_7]=1
  
  m_fc=colSums(m413)
  m_st=rowSums(m413)
  m_sq=which(m_fc>0)
  
  v_5=0
  l_sq=length(m_sq)
  
  # 牌型>4的比例超过97%，l_sq==6比例44%，l_sq==5比例35，l_sq==7比例18%
  # 牌型如果>4，先做是否是同花或顺子的判定；
  # 只要是顺子或同花，则将索引赋值给v_5
  if(l_sq==6){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
      }
    }
  } else if(l_sq==5){# v_5有可能被赋值
    max_st=which.max(m_st)
    if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
    else{# 如果不是同花，则判定是否是顺子
      if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
        v_5=v_7[cumsum(m_fc[m_sq])]
    }
  } else if(l_sq==7){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else if(m_st[max_st]==7){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_5[3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_5[c(1,4:7)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_7[cumsum(m_fc[m_sq])][3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
      }
    }
  } 
  
  # 既不是同花也不是顺子,v_5未被赋值（v_5被赋值的概率为6.6%）
  if(length(v_5)==1){
    v_7fc=rep(m_fc[m_sq],m_fc[m_sq])
    v_7fc_od=order(v_7fc,decreasing=T)
    
    if(v_7fc[v_7fc_od[1]]==4 && l_sq==3){# （4条+对子+单牌）需区别对待
      v_5=sort(c(v_7[v_7fc_od[1:4]],min(v_7[v_7fc_od[5:7]])))
    } else v_5=sort(v_7[v_7fc_od[1:5]])
  }
  
  match_mc5(v_5)# 返回行索引
}

# 4: seq5in7 速度较原来快1倍-------------------------------------------------
p_st=rep(1:4,13)
m413=matrix(0L,nr=4,nc=13)# 提前创建，function中使用[]赋值，性能提升7.8%
seq5in7=function(hd2,bd5){
  v_7=sort(c(hd2,bd5))
  v_7st=p_st[v_7]# v_7's suit
  m413[]=0L; m413[v_7]=1# 将v_7在m413中的位置置为1
  
  m_fc=colSums(m413)# 为牌型准备数据
  m_st=rowSums(m413)# 为同花准备数据
  m_sq=which(m_fc>0)# 为顺子准备数据
  
  v_5=0; l_sq=length(m_sq)
  
  # 牌型>4的比例超过97%，l_sq==6的比例44%，==5的比例35%，==7的比例18%
  # 牌型如果>4，先判定是否是同花或顺子；如果是顺子或同花，赋值给v_5
  if(l_sq>4){
    max_st=which.max(m_st)
    
    if(l_sq==6){# 如果牌型长度为6
      if(m_st[max_st]==5){# 如果有5个同花，直接赋值给v_5
        v_5=v_7[v_7st==max_st]
      } else if(m_st[max_st]==6){# 如果有6个同花，判断是否有顺子
        v_5=v_7[v_7st==max_st]
        if((m_sq[1]+4)==m_sq[5]){# 如果前5个同花是顺子
          v_5=v_5[1:5]
        } else if((m_sq[2]+4)==m_sq[6]){# 如果后5个同花是顺子
          v_5=v_5[2:6]
        } else if(all(m_sq[c(1,3)]==c(1,10))){# 如果是A开头的顺子
          v_5=v_5[c(1,3:6)]
        } else {# 如果没有顺子，则只保留v_5的前5个元素
          v_5=v_5[1:5]
        }
      } else{# 如果没有同花，v_5未被赋值，则判断是否有顺子
        if((m_sq[1]+4)==m_sq[5]){# 前5个组成顺子
          v_5=v_7[cumsum(m_fc[m_sq])][1:5]
        } else if((m_sq[2]+4)==m_sq[6]){# 后5个组成顺子
          v_5=v_7[cumsum(m_fc[m_sq])][2:6]
        } else if(all(m_sq[c(1,3)]==c(1,10))){# 如果是A开头的顺子
          v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
        }
      }
    } else if(l_sq==5){# 如果牌型长度为5
      if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]## 如果同花，给v_5赋值
      else{# 如果没有同花，则判定是否是顺子
        if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
          v_5=v_7[cumsum(m_fc[m_sq])]
      }
    } else{# l_sq==7
      if(m_st[max_st]==5){# 如果有5个同花，直接赋值给v_5
        v_5=v_7[v_7st==max_st]
      } else if(m_st[max_st]==6){# 如果有6个同花，判断是否有顺子
        v_5=v_7[v_7st==max_st]
        if((m_sq[1]+4)==m_sq[5]){# 如果前5个同花是顺子
          v_5=v_5[1:5]
        } else if((m_sq[2]+4)==m_sq[6]){# 如果后5个同花是顺子
          v_5=v_5[2:6]
        } else if(all(m_sq[c(1,3)]==c(1,10))){# 如果是A开头的顺子
          v_5=v_5[c(1,3:6)]
        } else {# 如果没有顺子，则只保留v_5的前5个元素
          v_5=v_5[1:5]
        }
      } else if(m_st[max_st]==7){# 如果有7个同花，判断是否有顺子
        v_5=v_7[v_7st==max_st]
        if((m_sq[1]+4)==m_sq[5]){# 如果前5个同花是顺子
          v_5=v_5[1:5]
        } else if((m_sq[2]+4)==m_sq[6]){# 如果中间5个同花是顺子
          v_5=v_5[2:6]
        } else if((m_sq[3]+4)==m_sq[7]){# 如果后5个同花是顺子
          v_5=v_5[3:7]
        } else if(all(m_sq[c(1,4)]==c(1,10))){# 如果是A开头的顺子
          v_5=v_5[c(1,4:7)]
        } else {# 如果没有顺子，则只保留v_5的前5个元素
          v_5=v_5[1:5]
        }
      } else{# 如果没有同花，则判定是否是顺子
        if((m_sq[1]+4)==m_sq[5]){# 前5个组成顺子
          v_5=v_7[cumsum(m_fc[m_sq])][1:5]
        } else if((m_sq[2]+4)==m_sq[6]){# 中间5个组成顺子
          v_5=v_7[cumsum(m_fc[m_sq])][2:6]
        } else if((m_sq[3]+4)==m_sq[7]){# 后5个组成顺子
          v_5=v_7[cumsum(m_fc[m_sq])][3:7]
        } else if(all(m_sq[c(1,4)]==c(1,10))){# 如果是A开头的顺子
          v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
        }
      }
    }
  } 
  
  if(length(v_5)==1){# 不是同花或顺子,则v_5未被赋值（被赋值概率为6.6%）
    v_7fc=rep(m_fc[m_sq],m_fc[m_sq])# 对v_7的face进行计数
    v_7fc_od=order(v_7fc,decreasing=T)# 根据v_7fc获得order
    
    if(v_7fc[v_7fc_od[1]]==4 && l_sq==3){# 4条+对子+单牌，需区别对待
      v_5=sort(c(v_7[v_7fc_od[1:4]],min(v_7[v_7fc_od[5:7]])))
    } else v_5=sort(v_7[v_7fc_od[1:5]])
  }
  
  match_mc5(v_5)# 返回行索引
}
# ---------------20180227---------------


# program test and validation -----------------------------------------
# 1. 验证match_mc5，通过
j=vector('logical',nrow(m_c5))
system.time(for(i in 1:nrow(m_c5)){
  # stopifnot(i==match_mc5(m_c5[i,1:5]))# 32.62 seconds
  j[i]=(i==match_mc5(m_c5[i,1:5]))# 17.15 seconds
})
all(j)

N=10000
m=matrix(nr=N,nc=7)
for(i in 1:N)
  m[i,]=sample(1:52,7)

system.time(for(i in 1:N) # about 1.3s
  seq5in7(m[i,1:2],m[i,3:7]))

m=t(combn(13,7))
n=matrix(nr=nrow(m),nc=5)
for(i in 1:nrow(m))
  n[i,]=seq5in7(m[i,1:2],m[i,3:7])

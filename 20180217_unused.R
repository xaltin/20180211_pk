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

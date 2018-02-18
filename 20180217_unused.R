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

# unused ------------------------------------------------------------------



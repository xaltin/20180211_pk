tt=Sys.time()
# 设定d_pk数据框
d_pk=data.frame(name=rep(c('A','K','Q','J','T',9:2),each=4),face=rep(1:13,each=4),
                   suit=rep(c(1,5,25,125),13))

m_c5=combn(1:52,5)# 设定扑克牌排列矩阵
m_c5_face=matrix(data=d_pk$face[m_c5],ncol=5,byrow=T)# 设定扑克牌face排列矩阵
m_c5_suit=matrix(data=d_pk$suit[m_c5],ncol=5,byrow=T)# 设定扑克牌suit排列矩阵
v_len_face=colSums(diff(t(m_c5_face))!=0)+1# face排列矩阵中相异的牌（每行）的数量
v_len_suit=rowSums(m_c5_suit)
v_len_suit[v_len_suit%in%c(5,25,125,625)]=1# suit排列矩阵中相同的牌（每行设为1

#【同花顺】和【同花】的索引
v14=which(v_len_face==5 & v_len_suit==1)#【同花顺】和【同花】，face=5，suit=1
tmp=diff(t(m_c5_face[v14,]))# diff是按列计算的
v1_1=(colSums(tmp==1)==4)# 【同花顺】A->2的索引
v1_2=(colSums(tmp==c(9,1,1,1))==4)# 【同花顺】A、2->5的索引
v1=c(v14[v1_1],v14[v1_2])# 【同花顺】的索引(已排序)
v4=v14[!(v1_1|v1_2)]# 【同花】的索引(已排序)
tmp=m_c5_face[v4,]
v4=v4[order(tmp[,2],tmp[,3],tmp[,4],tmp[,5])]
# all(diff(v4)>0)

# 【金刚】和【葫芦】的索引
v23=which(v_len_face==2)# 【金刚】和【葫芦】的索引，face=2
v2_1=apply(diff(t(m_c5_face[v23,])),2,function(x) all(x[2:3]==0))
v2=v23[v2_1]# 【金刚】的索引
v3=v23[!v2_1]# 【葫芦】的索引
v2=v2[order(m_c5_face[v2,2])]# 排序后【金刚】的索引
v3=v3[order(m_c5_face[v3,3],m_c5_face[v3,4])]# 排序后【葫芦】的索引

#【顺子】和【高牌】的索引
v59=which(v_len_face==5 & v_len_suit!=1)#【顺子】和【高牌】，face=5，suit!=1
tmp=diff(t(m_c5_face[v59,]))
v5_1=(colSums(tmp==1)==4)# 【顺子】A->2的索引
v5_2=(colSums(tmp==c(9,1,1,1))==4)# 【顺子】A、2->5的索引
v5=c(v59[v5_1],v59[v5_2])# 【顺子】的索引(已排序)
v9=v59[!(v5_1|v5_2)]# 【高牌】的索引
tmp=m_c5_face[v9,]
v9=v9[order(tmp[,1],tmp[,2],tmp[,3],tmp[,4],tmp[,5])]

# 【三条】和【两对】的索引
v67=which(v_len_face==3)# 【三条】和【两对】，face=3
tmp=diff(t(m_c5_face[v67,]))
v6_1=(tmp[1,]==0 & tmp[2,]==0)|(tmp[2,]==0 & tmp[3,]==0)|(tmp[3,]==0 & tmp[4,]==0)
v6=v67[v6_1]# 【三条】的索引
v7=v67[!v6_1]# 【两对】的索引
tmp=m_c5_face[v6,]
v6=v6[order(tmp[,1],tmp[,2],tmp[,3],tmp[,4],tmp[,5])]
tmp=m_c5_face[v7,]
v7=v7[order(tmp[,1],tmp[,2],tmp[,3],tmp[,4],tmp[,5])]

# 【对子】的索引
v8=which(v_len_face==4)
tmp=m_c5_face[v8,]
v8=v8[order(tmp[,1],tmp[,2],tmp[,3],tmp[,4],tmp[,5])]

# 补足type和rank
m_c5_type_rank=matrix(nr=2598960,nc=2)
m_c5_type_rank[c(v1,v2,v3,v4,v5,v6,v7,v8,v9),1]=
  rep(1:9,times=c(40,624,3744,5108,10200,54912,123552,1098240,1302540))
m_c5_type_rank[c(v1,v2,v3,v4,v5,v6,v7,v8,v9),2]=
  c(rep(1:10,each=4),rep(11:166,each=4),rep(167:322,each=24),rep(323:1599,each=4),
    rep(1600:1609,each=1020),rep(1610:2467,each=64),rep(2468:3325,each=144),
    rep(3326:6185,each=384),rep(6186:7462,each=1020))

# 最终的m_c5
m_c5=cbind(t(m_c5),m_c5_type_rank)

Sys.time()-tt


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


# d_c5stat ----------------------------------------------------------------
d_c5stat=data.frame(cnt=c(4*10,
                          13*48,
                          13*choose(4,3)*12*choose(4,2),
                          4*choose(13,5)-40,
                          10*4^5-40,
                          13*choose(4,3)*48*44/prod(2:1),
                          13*choose(4,2)*12*choose(4,2)/prod(2:1)*44,
                          13*choose(4,2)*48*44*40/prod(3:1),
                          52*48*44*40*36/prod(5:1)-10*4^5-4*choose(13,5)+40))
rownames(d_c5stat)=c('sflush','four','fhouse','flush','straight',
                     'three','twopair','pair','high')
d_c5stat$cnt_cum=cumsum(d_c5stat$cnt)
d_c5stat$pct=round(d_c5stat$cnt*100/sum(d_c5stat$cnt),4)
d_c5stat$pct_cum=round(d_c5stat$cnt_cum*100/sum(d_c5stat$cnt),4)
d_c5stat$ranks=as.vector(table(m_c5lite[,'type']))
d_c5stat$rank_cnt=as.vector(tapply(m_c5lite[,'rank_cnt'],m_c5lite[,'type'],unique))
d_c5stat$ranks_cum=cumsum(d_c5stat$ranks)

rm(list=ls()[-c(1,2,19,23)])
gc()

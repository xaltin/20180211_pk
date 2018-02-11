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

rm(list=ls()[-c(1,2)])
gc()

# m_c5 --------------------------------------------------------------------
tt=Sys.time()
# 设定d_pk数据框
d_pk=data.frame(name=rep(c('A','K','Q','J','T',9:2),each=4),face=rep(1:13,each=4),
                   suit=rep(c(1,5,25,125),13),stringsAsFactors=F)

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
d_c5stat$ranks=c(10,156,156,1277,10,858,858,2860,1277)
d_c5stat$rank_cnt=c(4,4,24,4,1020,64,144,384,1020)
d_c5stat$ranks_cum=cumsum(d_c5stat$ranks)

rm(list=ls()[-c(1:3)])


# m_c2sam ----------------------------------------------------------------
# 13种对子，78种同花，78种杂花
c2_pair=cbind(seq(1,by=4,length.out=13),seq(1,by=4,length.out=13)+1)
c2_suit=t(combn(seq(1,by=4,length.out=13),2))

c2_offs=matrix(nr=0,nc=2)
n=0
for(i in seq(1,by=4,length.out=12)){
  c2_offs=rbind(c2_offs,cbind(i,seq(6+n*4,by=4,length.out=12-n)))
  n=n+1
}
m_c2sam=rbind(c2_pair,c2_suit,c2_offs)


m_c2sam=cbind(m_c2sam,rep(c(6,4,12),c(13,78,78)),0,0,0,0,0,0,0,0)
m_c2sam[1:13,4]=1
m_c2sam[14:91,5]=1
m_c2sam[92:169,6]=1
i=m_c2sam[,2]-m_c2sam[,1]
m_c2sam[i==4|i==5,7]=1
m_c2sam[i==8|i==9,8]=1
m_c2sam[i==12|i==13,9]=1
m_c2sam[i==16|i==17,10]=1
m_c2sam[,11]=(m_c2sam[,5]&(m_c2sam[,7]|m_c2sam[,8]|
                             m_c2sam[,9]|m_c2sam[,10]))

colnames(m_c2sam)=c('c2_1','c2_2','type_cnt','pair','suit','offsuit',
                     'brg','brg1','brg2','brg3','suit_brg')
rm(c2_pair,c2_suit,c2_offs,i,n)

# d_c2stat ----------------------------------------------------------------
d_c2stat=data.frame(type_cnt=c(78,6,4*78,12*78,
                               sum(m_c2sam[,'type_cnt']*m_c2sam[,'brg']),
                               sum(m_c2sam[,'type_cnt']*m_c2sam[,'brg1']),
                               sum(m_c2sam[,'type_cnt']*m_c2sam[,'brg2']),
                               sum(m_c2sam[,'type_cnt']*m_c2sam[,'brg3']),
                               sum(m_c2sam[,'type_cnt']*m_c2sam[,'suit_brg'])))
rownames(d_c2stat)=c('pair','pair_1','suit','offsuit','brg','brg1','brg2',
                     'brg3','suit_brg')
d_c2stat$pct=round(100*d_c2stat$type_cnt/1326,2)
d_c2stat$odds=round(1326/d_c2stat$type_cnt,2)


# m_c2 --------------------------------------------------------------------
m_c2=combn(52,2)
i=d_pk$face[m_c2[2,]]-d_pk$face[m_c2[1,]]
j=d_pk$suit[m_c2[2,]]-d_pk$suit[m_c2[1,]]
pair=(i==0)
suit=(j==0)
offsuit=xor(!suit,pair)
brg=(i==1)
brg1=(i==2)
brg2=(i==3)
brg3=(i==4)
suit_brg=(suit&(brg|brg1|brg2|brg3))
m_c2=cbind(t(m_c2),pair,suit,offsuit,brg,brg1,brg2,brg3,suit_brg)

rm(i,j,pair,suit,offsuit,brg,brg1,brg2,brg3,suit_brg)
gc()

# m_c5id ------------------------------------------------------------------
m_c5id=cbind(c(table(m_c5[,1]),0,0,0,0),
             c(0,table(m_c5[1:249900,2]),0,0,0),
             c(0,0,table(m_c5[1:19600,3]),0,0),
             c(0,0,0,table(m_c5[1:1176,4]),0),
             c(0,0,0,0,table(m_c5[1:48,5])))
dimnames(m_c5id)=NULL



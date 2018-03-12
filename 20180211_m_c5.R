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

# 1. 己方2张手牌的wp，1个对手，耗时0.67 h(0312-0.67h)-------------------------------
# N=10000# 30w与1w的百分率相差1.1以内，耗时6 mins
# # 记录169种起手牌的9种牌型的出现次数，以及胜、平次数，运行169*2*30w=1.014亿次
# v_52=1:52
# N=300000
# m_rn_169_my=matrix(nr=N,nc=169)
# m_rn_169_op=matrix(nr=N,nc=169)
# 
# tt=Sys.time()
# for(i in 1:nrow(m_c2sam)){
#   v_my2=as.vector(m_c2sam[i,1:2])# 己方2张手牌,as.vector取消name
# 
#   for(j in 1:N){# for 2
#     v_op2=sample(v_52[-v_my2],2)# 对手2张手牌
#     v_bd5=sample(v_52[-c(v_my2,v_op2)],5)# 5张公共牌
# 
#     m_rn_169_my[j,i]=seq5in7(c(v_my2,v_bd5))# 返回v_my5的行索引
#     m_rn_169_op[j,i]=seq5in7(c(v_op2,v_bd5))# 返回v_op5的行索引
#   }
# }
# Sys.time()-tt
# 
# colnames(m_rn_169_my)=rownames(m_c2sam)
# colnames(m_rn_169_op)=rownames(m_c2sam)
# 
# saveRDS(m_rn_169_my,'data/m_rn_169_my_30w_0311.RData')
# saveRDS(m_rn_169_op,'data/m_rn_169_op_30w_0311.RData')

# readRDS -----------------------------------------------------------------
# m_rn_169_my=readRDS('data/m_rn_169_my_30w_0311.RData')
# m_rn_169_op=readRDS('data/m_rn_169_op_30w_0311.RData')

## 13x13 matrix for 2 people's 169's wp-------------------------------------
# v_wp_my=vector('integer',169)
# for(i in 1:169){
#   j=m_c5[m_rn_169_my[,i],7]
#   k=m_c5[m_rn_169_op[,i],7]
# 
#   v_wp_my[i]=round((sum(j<k)+sum(j==k)/2)*100/nrow(m_rn_169_my),1)
# }
# 
# m_p2_13x13=diag(v_wp_my[1:13],13,13)
# m_p2_13x13[lower.tri(m_p2_13x13)]=v_wp_my[14:91]
# m_p2_13x13=t(m_p2_13x13)
# m_p2_13x13[lower.tri(m_p2_13x13)]=v_wp_my[92:169]
# rownames(m_p2_13x13)=c('A','K','Q','J','T',9:2)
# colnames(m_p2_13x13)=c('A','K','Q','J','T',9:2)
# 
# write.csv(m_p2_13x13,'data/m_p2_13x13_30w.csv')

# 2. 己方2张手牌的wp，2个对手，169*3*10w=5070w次耗时0.3 h(0312-0.3h)-----------
# v_52=1:52
# N=100000
# m_rn_169_my3=matrix(nr=N,nc=169)
# m_rn_169_op1=matrix(nr=N,nc=169)
# m_rn_169_op2=matrix(nr=N,nc=169)
# 
# tt=Sys.time()
# for(i in 1:nrow(m_c2sam)){
#   v_my2=as.vector(m_c2sam[i,1:2])# 己方2张手牌,as.vector取消name
# 
#   for(j in 1:N){# for 2
#     v_op12=sample(v_52[-v_my2],2)# 对手1的2张手牌
#     v_op22=sample(v_52[-c(v_my2,v_op12)],2)# 对手2的2张手牌
#     v_bd5=sample(v_52[-c(v_my2,v_op12,v_op22)],5)# 5张公共牌
# 
#     m_rn_169_my3[j,i]=seq5in7(c(v_my2,v_bd5))# 返回v_my5的行索引
#     m_rn_169_op1[j,i]=seq5in7(c(v_op12,v_bd5))# 返回v_op5的行索引
#     m_rn_169_op2[j,i]=seq5in7(c(v_op22,v_bd5))# 返回v_op5的行索引
#   }
# }
# Sys.time()-tt
# 
# colnames(m_rn_169_my3)=rownames(m_c2sam)
# colnames(m_rn_169_op1)=rownames(m_c2sam)
# colnames(m_rn_169_op2)=rownames(m_c2sam)
# 
# saveRDS(m_rn_169_my3,'data/m_rn_169_my3_10w_0312.RData')
# saveRDS(m_rn_169_op1,'data/m_rn_169_op1_10w_0312.RData')
# saveRDS(m_rn_169_op2,'data/m_rn_169_op2_10w_0312.RData')

# readRDS -----------------------------------------------------------------
# m_rn_169_my3=readRDS('data/m_rn_169_my3_10w_0312.RData')
# m_rn_169_op1=readRDS('data/m_rn_169_op1_10w_0312.RData')
# m_rn_169_op2=readRDS('data/m_rn_169_op2_10w_0312.RData')

# # 13x13 matrix for 3 people's 169's wp-------------------------------------
# v_wp_my=vector('integer',169)
# for(i in 1:169){
#   j=m_c5[m_rn_169_my3[,i],7]
#   k1=m_c5[m_rn_169_op1[,i],7]
#   k2=m_c5[m_rn_169_op2[,i],7]
# 
#   win=sum((j<k1)&(j<k2))
#   los=sum((j>k1)|(j>k2))
# 
# 
#   v_wp_my[i]=100-round(100*los/N,1)
# }
# 
# m_p2_13x13=diag(v_wp_my[1:13],13,13)
# m_p2_13x13[lower.tri(m_p2_13x13)]=v_wp_my[14:91]
# m_p2_13x13=t(m_p2_13x13)
# m_p2_13x13[lower.tri(m_p2_13x13)]=v_wp_my[92:169]
# rownames(m_p2_13x13)=c('A','K','Q','J','T',9:2)
# colnames(m_p2_13x13)=c('A','K','Q','J','T',9:2)
# 
# write.csv(m_p2_13x13,'data/m_p3_13x13_10w_0312.csv')

# 3. 1person,7张牌中各种牌型的出现概率，100m次，耗时0.63 h(0312-0.63h)----------
# v_52=1:52
# N=100000000
# v_rn_5=vector('integer',N)
# 
# tt=Sys.time()
# for(i in 1:N){# for 2
#   v_7=sample(v_52,7)# 随机7张牌
#   v_rn_5[i]=seq5in7(v_7)#己方5张成牌
# }
# Sys.time()-tt
# 
# saveRDS(v_rn_5,'data/v_rn_5_100m_0311.RData')

# 4. 2person,7张牌中各种牌型的出现概率，100m次，耗时1.26 h(0312-1.26h)-----------
# v_52=1:52
# N=100000000
# v_rn_5_1=vector('integer',N)
# v_rn_5_2=vector('integer',N)
# 
# tt=Sys.time()
# for(i in 1:N){
#   v_my2=sample(52,2)
#   v_op2=sample(v_52[-v_my2],2)
#   v_bd5=sample(v_52[-c(v_my2,v_op2)],5)
#   v_rn_5_1[i]=seq5in7(c(v_my2,v_bd5))#己方5张成牌
#   v_rn_5_2[i]=seq5in7(c(v_op2,v_bd5))#己方5张成牌
# }
# Sys.time()-tt
# 
# saveRDS(v_rn_5_1,'data/v_rn_5_100m_0311_p2_1.RData')
# saveRDS(v_rn_5_2,'data/v_rn_5_100m_0311_p2_2.RData')

# 5. 3person,7张牌中各种牌型的出现概率，50m次，耗时1.05 h(0312-1.05h)-----------
# v_52=1:52
# N=50000000
# v_rn_5_31=vector('integer',N)
# v_rn_5_32=vector('integer',N)
# v_rn_5_33=vector('integer',N)
# 
# tt=Sys.time()
# for(i in 1:N){
#   v_my2=sample(52,2)
#   v_op1=sample(v_52[-v_my2],2)
#   v_op2=sample(v_52[-c(v_my2,v_op1)],2)
#   
#   v_bd5=sample(v_52[-c(v_my2,v_op1,v_op2)],5)
#   v_rn_5_31[i]=seq5in7(c(v_my2,v_bd5))#己方5张成牌
#   v_rn_5_32[i]=seq5in7(c(v_op1,v_bd5))#己方5张成牌
#   v_rn_5_33[i]=seq5in7(c(v_op2,v_bd5))#己方5张成牌
# }
# Sys.time()-tt
# 
# saveRDS(v_rn_5_31,'data/v_rn_5_50m_0311_p3_31.RData')
# saveRDS(v_rn_5_32,'data/v_rn_5_50m_0311_p3_32.RData')
# saveRDS(v_rn_5_33,'data/v_rn_5_50m_0311_p3_33.RData')

# readRDS -----------------------------------------------------------------
v_rn_5=readRDS('data/v_rn_5_100m_0311.RData')
v_rn_5_1=readRDS('data/v_rn_5_100m_0311_p2_1.RData')
v_rn_5_2=readRDS('data/v_rn_5_100m_0311_p2_2.RData')
v_rn_5_31=readRDS('data/v_rn_5_50m_0311_p3_31.RData')
v_rn_5_32=readRDS('data/v_rn_5_50m_0311_p3_32.RData')
v_rn_5_33=readRDS('data/v_rn_5_50m_0311_p3_33.RData')






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

# # readRDS，对p1/p2/p3成牌概率进行统计--------------------------------------
# v_rn_5=readRDS('data/v_rn_5_100m_0311.RData')
# v_rn_5_1=readRDS('data/v_rn_5_100m_0311_p2_1.RData')
# v_rn_5_2=readRDS('data/v_rn_5_100m_0311_p2_2.RData')
# v_rn_5_31=readRDS('data/v_rn_5_50m_0311_p3_31.RData')
# v_rn_5_32=readRDS('data/v_rn_5_50m_0311_p3_32.RData')
# v_rn_5_33=readRDS('data/v_rn_5_50m_0311_p3_33.RData')
# 
# df_v_rn_5=data.frame(table(m_c5[v_rn_5,6]))
# df_v_rn_5_1=data.frame(table(m_c5[v_rn_5_1,6]))
# df_v_rn_5_2=data.frame(table(m_c5[v_rn_5_2,6]))
# df_v_rn_5_31=data.frame(table(m_c5[v_rn_5_31,6]))
# df_v_rn_5_32=data.frame(table(m_c5[v_rn_5_32,6]))
# df_v_rn_5_33=data.frame(table(m_c5[v_rn_5_33,6]))
# 
# df_123p_stat=cbind(df_v_rn_5,df_v_rn_5_1$Freq,df_v_rn_5_2$Freq,
#                    df_v_rn_5_31$Freq,df_v_rn_5_32$Freq,df_v_rn_5_33$Freq)
# rm(df_v_rn_5,df_v_rn_5_1,df_v_rn_5_2,df_v_rn_5_31,df_v_rn_5_32,df_v_rn_5_33)
# 
# # 无论1/2/3人，个人成牌概率不随人数而变化
# df_123p_stat=cbind(df_123p_stat[,1],
#                    apply(df_123p_stat[,2:7],2,function(x) 100*round(x/sum(x),4)))
# 
# # p2的最终成牌统计
# i=data.frame(table(apply(cbind(m_c5[v_rn_5_1,6],m_c5[v_rn_5_2,6]),1,min)))
# v_p2_stat=100*round(i$Freq/sum(i$Freq),4)
# 
# # p3的最终成牌统计
# i=data.frame(table(apply(cbind(m_c5[v_rn_5_31,6],m_c5[v_rn_5_32,6],m_c5[v_rn_5_33,6]),1,min)))
# v_p3_stat=100*round(i$Freq/sum(i$Freq),4)
# 
# # 人数不同，最终的整体成牌概率发生变化
# df_123p_stat=cbind(df_123p_stat[,-2],df_123p_stat[,2],v_p2_stat,v_p3_stat)
# d_c7stat=cbind(df_123p_stat,cumsum(df_123p_stat[,7]),
#                    cumsum(df_123p_stat[,8]),cumsum(df_123p_stat[,9]))
# colnames(d_c7stat)=c('type','p2_1','p2_2','p3_1','p3_2','p3_3',
#                      'p1stat','p2stat','p3stat','p1cum','p2cum','p3cum')
# rm(df_123p_stat,v_p2_stat,v_p3_stat,i)
# 
# saveRDS(d_c7stat,'data/d_c7stat.RData')



# readRDS m_c7-------------------------------------------------------------
# m_c7=t(combn(52,7))
# saveRDS(m_c7,'data/m_c7.RData')
# m_c7=readRDS('data/m_c7.RData')
# m_c7id ------------------------------------------------------------------
# m_c7id=cbind(c(table(m_c7[,1]),0,0,0,0,0,0),
#              c(0,table(m_c7[1:18009460,2]),0,0,0,0,0),
#              c(0,0,table(m_c7[1:2118760,3]),0,0,0,0),
#              c(0,0,0,table(m_c7[1:211876,4]),0,0,0),
#              c(0,0,0,0,table(m_c7[1:17296,5]),0,0),
#              c(0,0,0,0,0,table(m_c7[1:1081,6]),0),
#              c(0,0,0,0,0,0,table(m_c7[1:46,7])))
# dimnames(m_c7id)=NULL
# saveRDS(m_c7id,'data/m_c7id.RData')
m_c7id=readRDS('data/m_c7id.RData')
m_c7idcum=apply(m_c7id, 2, cumsum)

# time consumed 36 mins
# v_c7=vector('integer',nrow(m_c7))
# tt=Sys.time()
# for(i in 1:nrow(m_c7)) v_c7[i]=seq5in7(m_c7[i,])
# Sys.time()-tt
# saveRDS(v_c7,'data/v_c7.RData')

v_c7=readRDS('data/v_c7.RData')
# d_v_c7=data.frame(table(v_c7))
v_c7uni=sort(unique(v_c7))#length=1148466

m_c5abs=m_c5[v_c7uni,]

length(unique(m_c5abs[,7]))#4872，m_c5为7462

l_c5abs=tapply(m_c5abs[,7],m_c5abs[,6],table)
lapply(l_c5abs, length)
lapply(l_c5abs, table)

i=table(m_c5[v_c7,6])
round(100*i/sum(i),2)# 与d_c7stat[,2]一致

choose(48,5)


133784560/1712304
choose(52,7)/choose(48,5)

k=0
for(i in 1:1325)
  for(j in (i+1):1326)
    k=k+1

choose(1326,2)




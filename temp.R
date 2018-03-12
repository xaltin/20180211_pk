# 数据统计 --------------------------------------------------------------------
v_rn_5_uni=unique(v_rn_5)
# 1148442 rows，有2598960-1148429=1450518未出现，理论每行出现1亿/2598960=38.48次
length(v_rn_5_uni)
range(v_rn_5_uni)#1 2598958
#有哪些行没有出现，对应的type和rank是什么？
i=vector('integer',2598960)
i[v_rn_5_uni]=1L
m_c5_disappear=m_c5[i==0,]
m_c5_disappear=m_c5_disappear[order(m_c5_disappear[,7]),]
table(m_c5_disappear[,6])
# 2      6      7      8      9 
# 30  18112  11196 533769 887411 

m_c5_appear=m_c5[i==1,]
m_c5_appear=m_c5_appear[order(m_c5_appear[,7]),]
table(m_c5_appear[,6])
# 1      2      3      4      5      6      7      8      9 
# 40    594   3744   5108  10200  36800 112356 564471 415129 

m_c5_disappear_face=m_c5_disappear
m_c5_disappear_face[,1:5]=ceiling(m_c5_disappear_face[,1:5]/4)
# 结论：实际上，有140多万种情况是不可能出现的

# 出现了7462个rank中的4872
v_rn_5_rank_uni=unique(m_c5[v_rn_5,7])
v_rn_5_rank_uni_len=length(v_rn_5_rank_uni)# 4872 ranks
range(v_rn_5_rank_uni)# 1 7414（出现的最小rank）
range(m_c5[,7])# 1 7462

m_c5rank=m_c5[order(m_c5[,7]),]
m_c5[match_mc5(c(24,28,32,40,43)),]# 9 7414(最小的rank)
ceiling(c(24,28,32,40,43)/4)#6  7  8 10 11 最小rank组合的face

# 为d_c5stat补充100M次模拟7选5的相关数据统计
# v_rn_5_type_tbl=data.frame(table(m_c5[v_rn_5,6]))
# d_c5stat$m100=v_rn_5_type_tbl[,2]
# d_c5stat$m100_pct=round(100*d_c5stat$m100/sum(d_c5stat$m100),3)
# d_c5stat$m100_pct_ratio=round(d_c5stat$m100_pct/d_c5stat$pct,2)
# saveRDS(d_c5stat,'data/d_c5stat.RData')
# write.csv(d_c5stat,'data/d_c5stat_0312.csv')

#高牌减少65%，对子增加4%，其余增加2.3-19.3倍
d_c5stat=readRDS('data/d_c5stat.RData')
#st+flu:0.03 four:0.2 house:2.8 flush:5.8 strt:10.44 three:15.3 2p:38.8 p:82.6 hi:100
round(cumsum(d_c5stat$m100_pct),2)





































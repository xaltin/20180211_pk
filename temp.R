# program test and validation -----------------------------------------
# 1. 验证match_mc5，通过
j=vector('logical',nrow(m_c5))
system.time(for(i in 1:nrow(m_c5)){
  # stopifnot(i==match_mc5(m_c5[i,1:5]))# 32.62 seconds
  j[i]=(i==match_mc5(m_c5[i,1:5]))# 16.8 seconds
})
all(j)

# 2. seq5in7性能测试，每秒1w
N=100000
m=matrix(nr=N,nc=7)
for(i in 1:N)
  m[i,]=sample(1:52,7)

system.time(for(i in 1:N)
  seq5in7(m[i,1:2],m[i,3:7]))

# 3. 验证seq5in7的正确性
# 建立测试数据集
# m_c5rank=m_c5[order(m_c5[,7]),]
# range(m_c5rank[,7])
# i=data.frame(table(m_c5rank[,7]))
# j=cumsum(i[,2])
# k=vector('integer',7462*2)
# k[seq(1,by=2,length.out=7462)]=c(0,j[1:7461])+1
# k[seq(2,by=2,length.out=7462)]=j
# m_test5=m_c5rank[k,]

# 建立测试数据集
m_c5type=m_c5[order(m_c5[,6]),]
i=data.frame(table(m_c5type[,6]))
j=cumsum(i[,2])
k=vector('integer',9*2)
k[seq(1,by=2,length.out=9)]=c(0,j[1:8])+1
k[seq(2,by=2,length.out=9)]=j

m_test5=m_c5type[k,]
v2_test=c(2,3,
     1,2,
     9,10,
     46,47,
     7,9,#5
     44,39,
     25,26,
     33,34,
     2,19,
     37,1,#10
     13,26,
     2,33,
     10,17,
     1,7,
     22,52,#15
     5,19,
     26,51)

m_test51=cbind(m_test5[1:17,1:5],matrix(v2_test,nr=17,nc=2,byrow=T))

m_test_rev=matrix(nrow=17,ncol=5)
for(i in 1:17){
  m_test_rev[i,]=seq5in7_test(m_test51[i,1:2],m_test51[i,3:7])
}


# 数据统计 --------------------------------------------------------------------
v_rn_5_uni=unique(v_rn_5)
# 1148429 rows，有2598960-1148429=1450531未出现，理论每行出现1亿/2598960=38.48次
v_rn_5_uni_len=length(v_rn_5_uni)
range(v_rn_5_uni)#1 2598958
#有哪些行没有出现，对应的type和rank是什么？
i=vector('integer',2598960)
i[sort(v_rn_5_uni)]=1
m_c5_disappear=m_c5[i==0,]
m_c5_disappear=m_c5_disappear[order(m_c5_disappear[,7]),]
# 2      6      7      8      9 
# 37  18112  11196 533768 887418
table(m_c5_disappear[,6])
m_c5_disappear_face=m_c5_disappear
m_c5_disappear_face[,1:5]=ceiling(m_c5_disappear_face[,1:5]/4)
# 结论：实际上，有140多万种情况是不可能出现的



v_rn_5_rank_uni=unique(m_c5[v_rn_5,7])
v_rn_5_rank_uni_len=length(v_rn_5_rank_uni)# 4872 ranks
range(v_rn_5_rank_uni)# 1 7414
range(m_c5[,7])# 1 7462

m_c5rank=m_c5[order(m_c5[,7]),]
m_c5[match_mc5(c(24,28,32,40,43)),]# 9 7414(最小的rank)
(7462-7414)*1020# 48960，有这么多row是不可能出现的，余下2598960-48960=255w
ceiling(c(24,28,32,40,43)/4)#6  7  8 10 11 最小rank组合的face


# v_rn_5_type_tbl=data.frame(table(m_c5[v_rn_5,6]))
# d_c5stat$m100=v_rn_5_type_tbl[,2]
# d_c5stat$m100_pct=100*d_c5stat$m100/sum(d_c5stat$m100)
# d_c5stat$m100_pct_ratio=round(d_c5stat$m100_pct/d_c5stat$pct,2)
# saveRDS(d_c5stat,'data/d_c5stat.RData')
# write.csv(d_c5stat,'data/d_c5stat.csv')
d_c5stat=readRDS('data/d_c5stat.RData')




# 查找5维数组 ------------------------------------------------------------------
a_5type=array(0L,dim=c(48,52,52,52,52))
a_5rank=array(0L,dim=c(48,52,52,52,52))
n=1
for(i in 1:48)
  for(j in (i+1):49)
    for(k in (j+1):50)
      for(l in (k+1):51)
        for(m in (l+1):52){
          a_5type[i,j,k,l,m]=m_c5[n,6]
          a_5rank[i,j,k,l,m]=m_c5[n,7]
          n=n+1
        }


# 空间利用率低
length(which(a_5type==0))#348358608
length(a_5rank)-length(which(a_5type==0))#2598960
(length(a_5rank)-length(which(a_5type==0)))/length(a_5rank)#0.00740534


# 查找速度快
i=1:5
i=c(1:4,16)
system.time({#0.15s
  for(j in 1:1000000)
    a_5[i[1],i[2],i[3],i[4],i[5]]
})
system.time({#5.46s
  for(j in 1:1000000)
    match_mc5(i)
})

k=1:7
system.time({#89.86s
  for(j in 1:1000000)
    seq5in7_test(k[1:2],k[3:7])
})

































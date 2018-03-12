# program test and validation -----------------------------------------
# 1. 验证match_mc5，通过
j=vector('logical',nrow(m_c5))
system.time(for(i in 1:nrow(m_c5)){
  # stopifnot(i==match_mc5(m_c5[i,1:5]))# 32.62 seconds
  j[i]=(i==match_mc5(m_c5[i,1:5]))# 16.8 seconds
})
all(j)

# 2. seq5in7性能测试，每秒1w
N=1000000
m=matrix(nr=N,nc=7)
for(i in 1:N)
  m[i,]=sample(1:52,7)

n1=vector('integer',N)
system.time(for(i in 1:N)
  n1[i]=seq5in7(m[i,1:2],m[i,3:7]))

# 2. seq5in7-20180311性能测试，每秒5.5w
n2=vector('integer',N)#18.25s，每秒5.5w
system.time(for(i in 1:N)
  n2[i]=seq5in7(m[i,]))

# 2. seq5in7-20180311性能测试，每秒6.1w
n1=vector('integer',N)#16.47s，每秒6.1w
system.time(for(i in 1:N)
  n1[i]=seq5in7(m[i,]))


all(n1==n2)
sum(n1!=n2)
i=which(n1!=n2)
i1=n1[i]
i2=n2[i]
j=1
m[i[j],]
m_c5[i1[j],]
m_c5[i2[j],]


all(m_c5[n1,7]==m_c5[n2,7])
sum(m_c5[n1,7]!=m_c5[n2,7])
i=which(m_c5[n1,7]!=m_c5[n2,7])
i1=n1[i]
i2=n2[i]
j=1
m[i[j],]
m_c5[i1[j],]
m_c5[i2[j],]

pface[m[i[j],]]
pface[m_c5[i1[j],1:5]]
pface[m_c5[i2[j],1:5]]

psuit[m[i[j],]]
psuit[m_c5[i1[j],1:5]]
psuit[m_c5[i2[j],1:5]]


# 2. 验证seq5in7的正确性
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

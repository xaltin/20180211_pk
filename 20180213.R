# 模拟测试7张牌在13*4矩阵中的分布 ----------------------------------------------------------

# 数据准备
v_52=1:52

N=100000
m_sum_rs=matrix(nr=N,nc=4)
m_sum_cf=matrix(nr=N,nc=13)

for(i in 1:N){
  # 生成13*4矩阵，用于计算可能的成牌及成牌的牌力
  m_413=matrix(0,4,13)
  
  # 手牌
  v_2=sample(v_52,2)
  m_413[v_2]=1
  # v_sum_cf2=colSums(m_413)
  # v_sum_rs2=rowSums(m_413)
  
  
  # 翻牌
  v_5=sample(v_52[-v_2],3)
  m_413[v_5]=1
  # v_sum_cf5=colSums(m_413)
  # v_sum_rs5=rowSums(m_413)
  
  # 转牌
  v_6=sample(v_52[-c(v_2,v_5)],1)
  m_413[v_6]=1
  
  # 河牌
  v_7=sample(v_52[-c(v_2,v_5,v_6)],1)
  m_413[v_7]=1
  
  
  m_sum_cf[i,]=colSums(m_413)
  m_sum_rs[i,]=rowSums(m_413)
}
m_sum_cf=t(apply(m_sum_cf,1,function(x) sort(x,dec=T)))
m_sum_cf=m_sum_cf[order(m_sum_cf[,1],m_sum_cf[,2],m_sum_cf[,3],decreasing=TRUE),1:7]

m_sum_rs=t(apply(m_sum_rs,1,function(x) sort(x,dec=T)))
m_sum_rs=m_sum_rs[order(m_sum_rs[,1],m_sum_rs[,2],m_sum_rs[,3],decreasing=TRUE),]

order(m_sum_cf[,1],m_sum_cf[,2],m_sum_cf[,3],decreasing = TRUE)



# 金刚的概率/outs
length(which(m_sum_c==4)%%N)/N
N/length(which(m_sum_c==4)%%N)
# 形成同花色（royal flush和flush）的概率/outs
length(which(m_sum_r>=5)%%N)/N
N/length(which(m_sum_r>=5)%%N)



# 每种牌型的概率 -----------------------------------------------------------------
# 任取5张牌，形成同花色（royal flush和flush）的概率
(13/52)*(12/51)*(11/50)*(10/49)*(9/48)*4
# 从52张牌中取7张，同花色（5或以上）的概率分布公式
p_flush=vector('integer',3)
for(i in 5:7){
  p_flush[i-4]=4*choose(13,i)*choose(39,7-i)/choose(52,7)
}
sum(p_flush)

# 从52张牌中取7张，金刚的概率
p_kind4=13*choose(4,4)*choose(48,3)/choose(52,7)



# probability test --------------------------------------------------------
N=1000000
m_sum_r=matrix(nr=N,nc=13)
m_sum_c=matrix(nr=N,nc=4)

for(i in 1:N){
  # 生成13*4矩阵，用于计算可能的成牌及成牌的牌力
  m_413=matrix(0,13,4)
  
  # 手牌
  v_5=sample(v_52,5)
  m_413[v_face[v_5][1],v_suit[v_5][1]]=1
  m_413[v_face[v_5][2],v_suit[v_5][2]]=1
  m_413[v_face[v_5][3],v_suit[v_5][3]]=1
  m_413[v_face[v_5][4],v_suit[v_5][4]]=1
  m_413[v_face[v_5][5],v_suit[v_5][5]]=1
  
  m_sum_r[i,]=rowSums(m_413)
  m_sum_c[i,]=colSums(m_413)
}

length(which(m_sum_c==5)%%N)/N
length(which(m_sum_r==4)%%N)/N


# 7 cards' search ---------------------------------------------------------
# 数据准备
v_52=vector(1:52)
# 生成4*13矩阵，用于计算可能的成牌及成牌的牌力
m_413=matrix(0,4,13,dimnames=list(1:4,1:13))
# 手牌2
v_2=sample(v_52,2)
# 公共牌5
v_5=sample(v_52[-v_2],5)
# 在m_413中为7张手牌和公共牌赋值为1
m_413[c(v_2,v_5)]=c(v_2,v_5)
# sum列获得同牌计数
(v_sum_cf7=sort(colSums(m_413),decreasing=TRUE))
# sum行获得同花计数
(v_sum_rs7=sort(rowSums(m_413),decreasing=TRUE))



# 手牌2张同花，成同花的概率
choose(11,3)*choose(39,2)/choose(50,5)+
  choose(11,4)*choose(39,1)/choose(50,5)+
  choose(11,5)/choose(50,5)

# 手牌2张同花，翻牌2张同花，成同花的概率
choose(9,1)*choose(38,1)/choose(47,2)+
                                  choose(9,2)/choose(47,2)

# 7张牌中成同花的可能性
(choose(13,5)*choose(39,2)/choose(52,7)+
  choose(13,6)*choose(39,1)/choose(52,7)+
  choose(13,7)/choose(52,7))*4
















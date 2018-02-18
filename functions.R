# seq5in7 -----------------------------------------------------------------
seq5in7=function(hand2,board5){
  m_413=matrix(0,4,13)# 13*4矩阵，计算可能的成牌及成牌的牌力
  v_7=sort(c(hand2,board5))# 为排序取前5个元素使用sort()
  m_413[v_7]=1
  
  v_sum_c_fc=colSums(m_413)
  v_sum_r_st=rowSums(m_413)
  v_seq_r_fc=which(v_sum_c_fc!=0)
  
  v_5=0
  if(length(v_seq_r_fc)>=5){# if牌型>=5，判断是否是同花或顺子
    i=which(v_sum_r_st>=5)
    
    if(length(i)==1){# 如果是同花
      v_5=v_7[(v_7%%4)==(i%%4)][1:5]
    } else {# 是否是顺子
      j=diff(v_seq_r_fc)
      
      if(all(j[1:4]==1)){# if j前四个元素==1
        v_5=v_seq_r_fc[1:5]*4-1:5%%4
      } else if(length(j)>=5){# if j长度>=5
        if(all(j[2:5]==1)) v_5=v_seq_r_fc[2:6]*4-1:5%%4
      } else if(length(j)==6){# if j长度==6
        if(all(j[3:6]==1)) v_5=v_seq_r_fc[3:7]*4-1:5%%4
      } else {# 以上都未命中，if j后四个元素==c(9,1,1,1)
        if(all(j[-3:0+length(j)]==c(9,1,1,1)))
          v_5=v_seq_r_fc[-4:0+length(v_seq_r_fc)]*4-1:5%%4
      }
    }
  }
  
  if(length(v_5)==1){# 既不是同花也不是顺子,v_5未被赋值
    k=factor(cut(v_7,seq(0,52,4),labels=1:13),
             levels=order(v_sum_c_fc,decreasing=T),ordered=T)
    v_5=sort(v_7[order(k)[1:5]])
  }
  
  v_5
}

# seq5in6 -----------------------------------------------------------------
seq5in6=function(card6){
  m_413=matrix(0,4,13)# 13*4矩阵，计算可能的成牌及成牌的牌力
  card6=sort(card6)# 为排序取前5个元素使用sort()
  m_413[card6]=1
  
  v_sum_c_fc=colSums(m_413)
  v_sum_r_st=rowSums(m_413)
  v_seq_r_fc=which(v_sum_c_fc!=0)
  
  v_5=0
  if(length(v_seq_r_fc)>=5){# if牌型>=5，判断是否是同花或顺子
    i=which(v_sum_r_st>=5)
    
    if(length(i)==1){# 如果是同花
      v_5=card6[(card6%%4)==(i%%4)][1:5]
    } else {# 是否是顺子
      j=diff(v_seq_r_fc)
      
      if(all(j[1:4]==1)){# if j前四个元素==1
        v_5=v_seq_r_fc[1:5]*4-1:5%%4
      } else if(length(j)==5){# if j长度>=5
        if(all(j[2:5]==1)) v_5=v_seq_r_fc[2:6]*4-1:5%%4
      } else {# 以上都未命中，if j后四个元素==c(9,1,1,1)
        if(all(j[-3:0+length(j)]==c(9,1,1,1)))
          v_5=v_seq_r_fc[-4:0+length(v_seq_r_fc)]*4-1:5%%4
      }
    }
  }
  
  if(length(v_5)==1){# 既不是同花也不是顺子,v_5未被赋值
    k=factor(cut(card6,seq(0,52,4),labels=1:13),
             levels=order(v_sum_c_fc,decreasing=T),ordered=T)
    v_5=sort(card6[order(k)[1:5]])
  }
  
  v_5
}

# seq5in5 -----------------------------------------------------------------
seq5in5=function(hand2,board5){
  m_413=matrix(0,4,13)# 13*4矩阵，计算可能的成牌及成牌的牌力
  v_7=sort(c(hand2,board5))# 为排序取前5个元素使用sort()
  m_413[v_7]=1
  
  v_sum_c_fc=colSums(m_413)
  v_sum_r_st=rowSums(m_413)
  v_seq_r_fc=which(v_sum_c_fc!=0)
  
  v_5=0
  if(length(v_seq_r_fc)>=5){# if牌型>=5，判断是否是同花或顺子
    i=which(v_sum_r_st>=5)
    
    if(length(i)==1){# 如果是同花
      v_5=v_7[(v_7%%4)==(i%%4)][1:5]
    } else {# 是否是顺子
      j=diff(v_seq_r_fc)
      
      if(all(j[1:4]==1)){# if j前四个元素==1
        v_5=v_seq_r_fc[1:5]*4-1:5%%4
      } else if(length(j)>=5){# if j长度>=5
        if(all(j[2:5]==1)) v_5=v_seq_r_fc[2:6]*4-1:5%%4
      } else if(length(j)==6){# if j长度==6
        if(all(j[3:6]==1)) v_5=v_seq_r_fc[3:7]*4-1:5%%4
      } else {# 以上都未命中，if j后四个元素==c(9,1,1,1)
        if(all(j[-3:0+length(j)]==c(9,1,1,1)))
          v_5=v_seq_r_fc[-4:0+length(v_seq_r_fc)]*4-1:5%%4
      }
    }
  }
  
  if(length(v_5)==1){# 既不是同花也不是顺子,v_5未被赋值
    k=factor(cut(v_7,seq(0,52,4),labels=1:13),
             levels=order(v_sum_c_fc,decreasing=T),ordered=T)
    v_5=sort(v_7[order(k)[1:5]])
  }
  
  v_5
}

# match_mc5 ---------------------------------------------------------------
match_mc5=function(id){
  i=c(1,id[1:4]+1)
  j=id-1
  flag=(i<=j)
  
  sum(c(sum(m_c5id[i[1]:j[1],1]),sum(m_c5id[i[2]:j[2],2]),
        sum(m_c5id[i[3]:j[3],3]),sum(m_c5id[i[4]:j[4],4]),
        sum(m_c5id[i[5]:j[5],5]))[flag])+1
}



# 功能与combn()相同 ------------------------------------------------------------
# m_c5_1=matrix(0L,nr=2598960,nc=5)
# n=1
# for(i in 1:48)
#   for(j in (i+1):49)
#     for(k in (j+1):50)
#       for(l in (k+1):51)
#         for(m in (l+1):52){
#           m_c5_1[n,]=c(i,j,k,l,m)
#           n=n+1
#         }
#           
# all(m_c5_1==m_c5[,1:5])
# 
# 
# 
# 
# write.csv(m_c5[,1:5],'m_c5.csv',row.names=F)



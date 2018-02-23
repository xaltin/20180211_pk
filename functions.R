# match_mc5 ---------------------------------------------------------------
match_mc5=function(id){
  i=c(1,id[1:4]+1); j=id-1; flag=(i<=j)
  
  sum(c(sum(m_c5id[i[1]:j[1],1]),sum(m_c5id[i[2]:j[2],2]),
        sum(m_c5id[i[3]:j[3],3]),sum(m_c5id[i[4]:j[4],4]),
        sum(m_c5id[i[5]:j[5],5]))[flag])+1
}


# 防止data.frame导致读取性能降低，单独赋值成vector ----------------------------
p_face=d_pk$face
p_suit=rep(1:4,13)
m=matrix(nr=7,nc=5)# 提前创建m，function中使用[,]赋值，seq5in7提升7.8%

# seq5in7 速度较原来快1倍------------------------------------------------------
seq5in7=function(hand2,board5){
  v_7=sort(c(hand2,board5))
  m[,]=cbind(v_7,0,0,p_face[v_7],p_suit[v_7])
  
  i=1;len=0
  while(i<=7){# 实现了3个功能1.table 2.length 3.diff
    j=i+1
    while(j<=7 && m[j,4]==m[i,4]) j=j+1
    if(j==(i+1)) m[i,2]=1 else m[i:(j-1),2]=j-i
    len=len+1; m[len,3]=i; i=j
  }
  
  v_5=0
  # len>=5，则同花最大，顺子次之
  if(len>=5){# if 1
    n=0
    for(i in 1:4){# 同花
      j=(m[,5]==i); k=sum(j)
      if(k>=5) {v_5=m[j,1][1:5]; break}
      else n=n+k
      
      if(n>=3) break
    }
    
    if(k<5){# 顺子
      i=m[1:len,3]; j=diff(m[i,4])
      
      if(all(j[1:4]==1)) v_5=m[i[1:5],1]
      else if(len>=6) {if(all(j[2:5]==1)) v_5=m[i[2:6],1]}
      else if(len==7) {if(all(j[3:6]==1)) v_5=m[i[3:7],1]}
      else {if(all(j[-3:0+(len-1)]==c(9,1,1,1))) v_5=m[i[-4:0+len],1]}
    }
  }# if 1

  # 既不是同花也不是顺子,v_5未被赋值
  if(length(v_5)==1){# if 2
    i=order(m[,2],decreasing=T)
    if(m[i[1],2]==4){# 对金刚（4条+对子+单牌）需区别对待
      j=which(m[,2]==4)
      if(j[1]!=1) v_5=m[c(1,j),1] else v_5=m[1:5,1]
    } else v_5=m[sort(i[1:5]),1]# todo::是否有可能改进
  }# if 2

    match_mc5(v_5)# 返回行索引
}

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
# 这段代码有问题，没有实现同花顺的判断
seq5in7=function(hand2,board5){
  v_7=sort(c(hand2,board5))
  m[,]=cbind(v_7,0,0,p_face[v_7],p_suit[v_7])
  
  i=1;len=0
  while(i<=7){# 实现了3个功能1.table 2.length 3.unique
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


# outs and odds -----------------------------------------------------------

p_fc=rep(1:13,each=4)
p_st=rep(1:4,13)
v_7=vector('integer',7)
v_7fc=vector('integer',7)
v_7st=vector('integer',7)
m413=matrix(0L,nr=4,nc=13)# 提前创建m，function中使用[,]赋值，seq5in7提升7.8%
m_fc=vector('integer',13)
m_st=vector('integer',4)

v_7=c(1,5,6,37,41,45,49)
v_7=c(1,5,9,38,44,48,49)
# seq5in7 速度较原来快1倍------------------------------------------------------
seq5in7=function(hd2,bd5){
  v_7=sort(c(hd2,bd5))
  v_7fc=p_fc[v_7]
  v_7st=p_st[v_7]
  m413[]=0L
  m413[v_7]=1
  
  m_fc=colSums(m413)
  m_st=rowSums(m413)
  m_sq=which(m_fc>0)
  
  v_5=0
  l_sq=length(m_sq)
  
  # 牌型如果>4，先做是否是同花或顺子的判定；
  # 只要是顺子或同花，则将索引赋值给v_5
  if(l_sq==5){# v_5有可能被赋值
    max_st=which.max(m_st)
    if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
    else{# 如果不是同花，则判定是否是顺子
      if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
        v_5=v_7[cumsum(m_fc[m_sq])]
    }
  }
  
  if(l_sq==6){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
      }
    }
  }
  
  if(l_sq==7){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else if(m_st[max_st]==7){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_5[3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_5[c(1,4:7)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_7[cumsum(m_fc[m_sq])][3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
      }
    }
  } 
  
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

# seq5in7 速度较原来快1倍------------------------------------------------------
seq5in7=function(hd2,bd5){
  v_7=sort(c(hd2,bd5))
  v_7fc=p_fc[v_7]
  v_7st=p_st[v_7]
  m413[]=0L
  m413[v_7]=1
  
  m_fc=colSums(m413)
  m_st=rowSums(m413)
  m_sq=which(m_fc>0)
  
  v_5=0
  l_sq=length(m_sq)
  
  # 牌型>4的比例超过97%，l_sq==6比例44%，l_sq==5比例35，l_sq==7比例18%
  # 牌型如果>4，先做是否是同花或顺子的判定；
  # 只要是顺子或同花，则将索引赋值给v_5
  if(l_sq==6){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
      }
    }
  } else if(l_sq==5){# v_5有可能被赋值
    max_st=which.max(m_st)
    if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
    else{# 如果不是同花，则判定是否是顺子
      if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
        v_5=v_7[cumsum(m_fc[m_sq])]
    }
  } else if(l_sq==7){
    max_st=which.max(m_st)
    if(m_st[max_st]==5){
      v_5=v_7[v_7st==max_st]
    } else if(m_st[max_st]==6){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if(all(m_sq[c(1,3)]==c(1,10))){
        v_5=v_5[c(1,3:6)]
      } else {
        v_5=v_5[1:5]
      }
    } else if(m_st[max_st]==7){
      v_5=v_7[v_7st==max_st]
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_5[1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_5[2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_5[3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_5[c(1,4:7)]
      } else {
        v_5=v_5[1:5]
      }
    } else{
      if((m_sq[1]+4)==m_sq[5]){
        v_5=v_7[cumsum(m_fc[m_sq])][1:5]
      } else if((m_sq[2]+4)==m_sq[6]){
        v_5=v_7[cumsum(m_fc[m_sq])][2:6]
      } else if((m_sq[3]+4)==m_sq[7]){
        v_5=v_7[cumsum(m_fc[m_sq])][3:7]
      } else if(all(m_sq[c(1,4)]==c(1,10))){
        v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
      }
    }
  } 
  
  # 既不是同花也不是顺子,v_5未被赋值（v_5被赋值的概率为6.6%）
  if(length(v_5)==1){
    i=m_fc[m_sq]
    
    
    i=order(m[,2],decreasing=T)
    if(m[i[1],2]==4){# 对金刚（4条+对子+单牌）需区别对待
      j=which(m[,2]==4)
      if(j[1]!=1) v_5=m[c(1,j),1] else v_5=m[1:5,1]
    } else v_5=m[sort(i[1:5]),1]# todo::是否有可能改进
  }# if 2
  
  match_mc5(v_5)# 返回行索引
}




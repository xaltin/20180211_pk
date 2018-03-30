# match_mc5 ---------------------------------------------------------------
match_mc5=function(id){
  i=c(1,id[1:4]+1); j=id-1; flag=(i<=j)
  
  sum(c(sum(m_c5id[i[1]:j[1],1]),sum(m_c5id[i[2]:j[2],2]),
        sum(m_c5id[i[3]:j[3],3]),sum(m_c5id[i[4]:j[4],4]),
        sum(m_c5id[i[5]:j[5],5]))[flag])+1
}

# match_mc7 ---------------------------------------------------------------
match_mc7=function(id){
  i=id-1;flag=(c(1,id[1:6]+1)<=i)
  
  sum(c(ifelse(i[1]==0,0,m_c7idcum[i[1],1]),m_c7idcum[i[2],2]-m_c7idcum[id[1],2],
        m_c7idcum[i[3],3]-m_c7idcum[id[2],3],m_c7idcum[i[4],4]-m_c7idcum[id[3],4],
        m_c7idcum[i[5],5]-m_c7idcum[id[4],5],m_c7idcum[i[6],6]-m_c7idcum[id[5],6],
        m_c7idcum[i[7],7]-m_c7idcum[id[6],7])[flag])+1
}

# seq5in7-20180311 快6.1倍，6.1w/s-----------------------------------------
pface=rep(1:13,each=4)
psuit=rep(1:4,13)
v7fa=vector('integer',7)# face
v7su=vector('integer',7)# suit
v7cn=vector('integer',7)# face的count
v7id=vector('integer',7)# unique face的id

seq5in7=function(v7){
  v7=.Internal(sort(v7,F))# shell sort, the fastest
  
  v7fa=pface[v7]
  v7su=psuit[v7]
  
  i=1
  len=0
  while(i<=7){# 实现了3个功能1.table 2.length 3.unique
    j=i+1
    while(j<=7 && v7fa[j]==v7fa[i]) j=j+1
    if(j==(i+1)) v7cn[i]=1
    else v7cn[i:(j-1)]=j-i
    len=len+1
    v7id[len]=i
    i=j
  }
  v7idf=v7fa[v7id[1:len]]# unique face的id对应的face
  
  v5=0L
  n=0L
  for(i in 1:4){# 先判断flush，face5个以上flush最大
    j=(v7su==i)
    k=sum(j)
    if(k>4) {
      if(k==5) v5=v7[j]
      else if(k==6){
        l=which.min(c(m_c5[match_mc5(v7[j][1:5]),7],
                      m_c5[match_mc5(v7[j][2:6]),7],
                      m_c5[match_mc5(v7[j][c(1,3:6)]),7]))
        v5=v7[j][matrix(c(1:5,2:6,1,3:6),nr=3,byrow=T)[l,]]
      }else if(k==7){
        l=which.min(c(m_c5[match_mc5(v7[j][1:5]),7],
                      m_c5[match_mc5(v7[j][2:6]),7],
                      m_c5[match_mc5(v7[j][3:7]),7],
                      m_c5[match_mc5(v7[j][c(1,4:7)]),7]))
        v5=v7[j][matrix(c(1:5,2:6,3:7,1,4:7),nr=4,byrow=T)[l,]]
      }
      break#k>4意味着已发现5个flush，退出
    }
    else n=n+k
    
    if(n>2) break#n>2意味着后面不可能出现5个flush，退出
  }
  
  if(length(v5)==1){# 再判断straight，face5个以上straight次大
    if(len==5){
      if((v7idf[1]+4)==v7idf[5] || (v7idf[1]+9)==v7idf[2])
        v5=v7[v7id[1:5]]
    }else if(len==6){
      if((v7idf[1]+4)==v7idf[5]) v5=v7[v7id[1:5]]
      else if((v7idf[2]+4)==v7idf[6]) v5=v7[v7id[2:6]]
      else if((v7idf[1]+9)==v7idf[3]) v5=v7[v7id[c(1,3:6)]]
    }else if(len==7){
      if((v7idf[1]+4)==v7idf[5]) v5=v7[v7id[1:5]]
      else if((v7idf[2]+4)==v7idf[6]) v5=v7[v7id[2:6]]
      else if((v7idf[3]+4)==v7idf[7]) v5=v7[v7id[3:7]]
      else if((v7idf[1]+9)==v7idf[4]) v5=v7[v7id[c(1,4:7)]]
    }
  }
  
  if(length(v5)==1){#以上v5出现概率为7.64%，未出现则继续判断
    if(len>4){
      if(len==7) v5=v7[1:5]
      else{
        i=which(v7cn==1)
        j=length(i)
        v5=v7[-i[c(j-1,j)]]
      }
    }else if(len==4){
      i=which(v7cn==1)
      j=length(i)
      if(j>1) v5=v7[-i[c(j-1,j)]]
      else v5=v7[-i][-6]
    }else if(len==3){
      i=which(v7cn==3)
      j=length(i)
      if(j==6) v5=v7[i[1:5]]
      else if(j==3){
        i=which(v7cn==2)
        v5=v7[-i[3:4]]
      } else{
        i=which(v7cn %in% c(1,2))
        v5=v7[-i[2:3]]
      }
    }else{
      i=which(v7cn==3)
      v5=v7[-i[2:3]]
    }
  }
  
  match_mc5(v5)
}


# # seq5in7-20180311 快5倍-------------------------------------------------
# pface=rep(1:13,each=4)
# psuit=rep(1:4,13)
# v52=vector("integer",52)
# v7fa=vector('integer',7)# face
# v7su=vector('integer',7)# suit
# v7cn=vector('integer',7)# face的count
# v7id=vector('integer',7)# unique face的id
# 
# seq5in7=function(v7){
#   v52[]=0L
#   v52[v7]=1L
#   v7=which(v52==1L)# 排序，空间换时间，快5倍
#   
#   v7fa=pface[v7]
#   v7su=psuit[v7]
#   
#   i=1
#   len=0
#   while(i<=7){# 实现了3个功能1.table 2.length 3.unique
#     j=i+1
#     while(j<=7 && v7fa[j]==v7fa[i]) j=j+1
#     if(j==(i+1)) v7cn[i]=1
#     else v7cn[i:(j-1)]=j-i
#     len=len+1
#     v7id[len]=i
#     i=j
#   }
#   v7idf=v7fa[v7id[1:len]]# unique face的id对应的face
#   
#   v5=0L
#   n=0L
#   for(i in 1:4){# 先判断flush，face5个以上flush最大
#     j=(v7su==i)
#     k=sum(j)
#     if(k>4) {
#       if(k==5) v5=v7[j]
#       else if(k==6){
#         l=which.min(c(m_c5[match_mc5(v7[j][1:5]),7],
#                       m_c5[match_mc5(v7[j][2:6]),7],
#                       m_c5[match_mc5(v7[j][c(1,3:6)]),7]))
#         v5=v7[j][matrix(c(1:5,2:6,1,3:6),nr=3,byrow=T)[l,]]
#       }else if(k==7){
#         l=which.min(c(m_c5[match_mc5(v7[j][1:5]),7],
#                       m_c5[match_mc5(v7[j][2:6]),7],
#                       m_c5[match_mc5(v7[j][3:7]),7],
#                       m_c5[match_mc5(v7[j][c(1,4:7)]),7]))
#         v5=v7[j][matrix(c(1:5,2:6,3:7,1,4:7),nr=4,byrow=T)[l,]]
#       }
#       break#k>4意味着已发现5个flush，退出
#     }
#     else n=n+k
#     
#     if(n>2) break#n>2意味着后面不可能出现5个flush，退出
#   }
#   
#   if(length(v5)==1){# 再判断straight，face5个以上straight次大
#     if(len==5){
#       if((v7idf[1]+4)==v7idf[5] || (v7idf[1]+9)==v7idf[2])
#         v5=v7[v7id[1:5]]
#     }else if(len==6){
#       if((v7idf[1]+4)==v7idf[5]) v5=v7[v7id[1:5]]
#       else if((v7idf[2]+4)==v7idf[6]) v5=v7[v7id[2:6]]
#       else if((v7idf[1]+9)==v7idf[3]) v5=v7[v7id[c(1,3:6)]]
#     }else if(len==7){
#       if((v7idf[1]+4)==v7idf[5]) v5=v7[v7id[1:5]]
#       else if((v7idf[2]+4)==v7idf[6]) v5=v7[v7id[2:6]]
#       else if((v7idf[3]+4)==v7idf[7]) v5=v7[v7id[3:7]]
#       else if((v7idf[1]+9)==v7idf[4]) v5=v7[v7id[c(1,4:7)]]
#     }
#   }
#   
#   if(length(v5)==1){#以上v5出现概率为7.64%，未出现则继续判断
#     if(len>4){
#       if(len==7) v5=v7[1:5]
#       else{
#         i=which(v7cn==1)
#         j=length(i)
#         v5=v7[-i[c(j-1,j)]]
#       }
#     }else if(len==4){
#       i=which(v7cn==1)
#       j=length(i)
#       if(j>1) v5=v7[-i[c(j-1,j)]]
#       else v5=v7[-i][-6]
#     }else if(len==3){
#       i=which(v7cn==3)
#       j=length(i)
#       if(j==6) v5=v7[i[1:5]]
#       else if(j==3){
#         i=which(v7cn==2)
#         v5=v7[-i[3:4]]
#       } else{
#         i=which(v7cn %in% c(1,2))
#         v5=v7[-i[2:3]]
#       }
#     }else{
#       i=which(v7cn==3)
#       v5=v7[-i[2:3]]
#     }
#   }
#   
#   match_mc5(v5)
# }

# 6: seq5in7_test的测试版，去除match_mc5，返回v_5----------------------------
# p_st=rep(1:4,13)
# m413=matrix(0L,nr=4,nc=13)
# seq5in7_test=function(hd2,bd5){
#   v_7=sort(c(hd2,bd5)); v_7st=p_st[v_7]
#   m413[]=0L; m413[v_7]=1
#   
#   m_fc=colSums(m413)
#   m_st=rowSums(m413)
#   m_sq=which(m_fc>0)
#   
#   v_5=0; l_sq=length(m_sq)
#   
#   if(l_sq>4){
#     max_st=which.max(m_st)
#     
#     if(l_sq==6){
#       if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
#       else if(m_st[max_st]==6){
#         v_5=v_7[v_7st==max_st]
#         if((m_sq[1]+4)==m_sq[5]) v_5=v_5[1:5]
#         else if((m_sq[2]+4)==m_sq[6]) v_5=v_5[2:6]
#         else if(all(m_sq[c(1,3)]==c(1,10))) v_5=v_5[c(1,3:6)]
#         else v_5=v_5[1:5]
#       } else{
#         if((m_sq[1]+4)==m_sq[5]) v_5=v_7[cumsum(m_fc[m_sq])][1:5]
#         else if((m_sq[2]+4)==m_sq[6]) v_5=v_7[cumsum(m_fc[m_sq])][2:6]
#         else if(all(m_sq[c(1,3)]==c(1,10))) v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
#       }
#     } else if(l_sq==5){
#       if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
#       else{
#         if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
#           v_5=v_7[cumsum(m_fc[m_sq])]
#       }
#     } else{
#       if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
#       else if(m_st[max_st]==6){
#         v_5=v_7[v_7st==max_st]
#         if((m_sq[1]+4)==m_sq[5]) v_5=v_5[1:5]
#         else if((m_sq[2]+4)==m_sq[6]) v_5=v_5[2:6]
#         else if(all(m_sq[c(1,3)]==c(1,10))) v_5=v_5[c(1,3:6)]
#         else v_5=v_5[1:5]
#       } else if(m_st[max_st]==7){
#         v_5=v_7[v_7st==max_st]
#         if((m_sq[1]+4)==m_sq[5]) v_5=v_5[1:5]
#         else if((m_sq[2]+4)==m_sq[6]) v_5=v_5[2:6]
#         else if((m_sq[3]+4)==m_sq[7]) v_5=v_5[3:7]
#         else if(all(m_sq[c(1,4)]==c(1,10))) v_5=v_5[c(1,4:7)]
#         else v_5=v_5[1:5]
#       } else{
#         if((m_sq[1]+4)==m_sq[5]) v_5=v_7[cumsum(m_fc[m_sq])][1:5]
#         else if((m_sq[2]+4)==m_sq[6]) v_5=v_7[cumsum(m_fc[m_sq])][2:6]
#         else if((m_sq[3]+4)==m_sq[7]) v_5=v_7[cumsum(m_fc[m_sq])][3:7]
#         else if(all(m_sq[c(1,4)]==c(1,10))) v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
#       }
#     }
#   } 
#   
#   if(length(v_5)==1){
#     v_7fc=rep(m_fc[m_sq],m_fc[m_sq])
#     v_7fc_od=order(v_7fc,decreasing=T)
#     
#     if(v_7fc[v_7fc_od[1]]==4 && l_sq==3){
#       v_5=sort(c(v_7[v_7fc_od[1:4]],min(v_7[v_7fc_od[5:7]])))
#     } else v_5=sort(v_7[v_7fc_od[1:5]])
#   }
#   
#   v_5
# }


# 查找5维数组(虽然速度快，但瓶颈在seq5in7)-------------------------------------------------
# a_5type=array(0L,dim=c(48,52,52,52,52))
# a_5rank=array(0L,dim=c(48,52,52,52,52))
# n=1
# for(i in 1:48)
#   for(j in (i+1):49)
#     for(k in (j+1):50)
#       for(l in (k+1):51)
#         for(m in (l+1):52){
#           a_5type[i,j,k,l,m]=m_c5[n,6]
#           a_5rank[i,j,k,l,m]=m_c5[n,7]
#           n=n+1
#         }
# 
# 
# # 空间利用率低
# length(which(a_5type==0))#348358608
# length(a_5rank)-length(which(a_5type==0))#2598960
# (length(a_5rank)-length(which(a_5type==0)))/length(a_5rank)#0.00740534
# 
# 
# 查找速度快

# system.time({#0.15s
#   for(j in 1:1000000)
#     i=.Internal(sort(sample(52,5),F))
#     a_5rank[i[1],i[2],i[3],i[4],i[5]]
# })
# system.time({#5.46s
#   for(j in 1:1000000)
#     i=.Internal(sort(sample(52,5),F))
#     match_mc5(i)
# })
# 
# k=1:7
# system.time({#89.86s
#   for(j in 1:1000000)
#     seq5in7_test(k[1:2],k[3:7])
# })

# a_7type=array(0L,dim=c(48,52,52,52,52,52,52))
a_7rank=array(NA,dim=c(48,52,52,52,52))
n=1
for(i in 1:48)
  for(j in (i+1):49)
    for(k in (j+1):50)
      for(l in (k+1):51)
        for(m in (l+1):52){
          # a_7type[i,j,k,l,m]=m_c5[n,6]
          a_7rank[i,j,k,l,m]=m_c5[n,7]
          n=n+1
        }


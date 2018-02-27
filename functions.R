# match_mc5 ---------------------------------------------------------------
match_mc5=function(id){
  i=c(1,id[1:4]+1); j=id-1; flag=(i<=j)
  
  sum(c(sum(m_c5id[i[1]:j[1],1]),sum(m_c5id[i[2]:j[2],2]),
        sum(m_c5id[i[3]:j[3],3]),sum(m_c5id[i[4]:j[4],4]),
        sum(m_c5id[i[5]:j[5],5]))[flag])+1
}



# 5: 4的非注释版，较1快近10个百分点----------------------------------------------
p_st=rep(1:4,13)
m413=matrix(0L,nr=4,nc=13)
seq5in7=function(hd2,bd5){
  v_7=sort(c(hd2,bd5)); v_7st=p_st[v_7]
  m413[]=0L; m413[v_7]=1
  
  m_fc=colSums(m413)
  m_st=rowSums(m413)
  m_sq=which(m_fc>0)
  
  v_5=0; l_sq=length(m_sq)
  
  if(l_sq>4){
    max_st=which.max(m_st)
    
    if(l_sq==6){
      if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
      else if(m_st[max_st]==6){
        v_5=v_7[v_7st==max_st]
        if((m_sq[1]+4)==m_sq[5]) v_5=v_5[1:5]
        else if((m_sq[2]+4)==m_sq[6]) v_5=v_5[2:6]
        else if(all(m_sq[c(1,3)]==c(1,10))) v_5=v_5[c(1,3:6)]
        else v_5=v_5[1:5]
      } else{
        if((m_sq[1]+4)==m_sq[5]) v_5=v_7[cumsum(m_fc[m_sq])][1:5]
        else if((m_sq[2]+4)==m_sq[6]) v_5=v_7[cumsum(m_fc[m_sq])][2:6]
        else if(all(m_sq[c(1,3)]==c(1,10))) v_5=v_7[cumsum(m_fc[m_sq])][c(1,3:6)]
      }
    } else if(l_sq==5){
      if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
      else{
        if((m_sq[1]+4)==m_sq[5] | all(m_sq[1:2]==c(1,10)))
          v_5=v_7[cumsum(m_fc[m_sq])]
      }
    } else{
      if(m_st[max_st]==5) v_5=v_7[v_7st==max_st]
      else if(m_st[max_st]==6){
        v_5=v_7[v_7st==max_st]
        if((m_sq[1]+4)==m_sq[5]) v_5=v_5[1:5]
        else if((m_sq[2]+4)==m_sq[6]) v_5=v_5[2:6]
        else if(all(m_sq[c(1,3)]==c(1,10))) v_5=v_5[c(1,3:6)]
        else v_5=v_5[1:5]
      } else if(m_st[max_st]==7){
        v_5=v_7[v_7st==max_st]
        if((m_sq[1]+4)==m_sq[5]) v_5=v_5[1:5]
        else if((m_sq[2]+4)==m_sq[6]) v_5=v_5[2:6]
        else if((m_sq[3]+4)==m_sq[7]) v_5=v_5[3:7]
        else if(all(m_sq[c(1,4)]==c(1,10))) v_5=v_5[c(1,4:7)]
        else v_5=v_5[1:5]
      } else{
        if((m_sq[1]+4)==m_sq[5]) v_5=v_7[cumsum(m_fc[m_sq])][1:5]
        else if((m_sq[2]+4)==m_sq[6]) v_5=v_7[cumsum(m_fc[m_sq])][2:6]
        else if((m_sq[3]+4)==m_sq[7]) v_5=v_7[cumsum(m_fc[m_sq])][3:7]
        else if(all(m_sq[c(1,4)]==c(1,10))) v_5=v_7[cumsum(m_fc[m_sq])][c(1,4:7)]
      }
    }
  } 
  
  if(length(v_5)==1){
    v_7fc=rep(m_fc[m_sq],m_fc[m_sq])
    v_7fc_od=order(v_7fc,decreasing=T)
    
    if(v_7fc[v_7fc_od[1]]==4 && l_sq==3){
      v_5=sort(c(v_7[v_7fc_od[1:4]],min(v_7[v_7fc_od[5:7]])))
    } else v_5=sort(v_7[v_7fc_od[1:5]])
  }
  
  match_mc5(v_5)
}



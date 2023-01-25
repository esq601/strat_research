source('hex_setup.R')
source('hex_conflict.R')

library(foreach)
library(doParallel)
library(tidyverse)
library(data.table)


cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
cores
#cbind(data.table(),"s" = sample(q_df_pos[s == '060202']$a,100,replace = TRUE))


actions <- function(players,adj,search = 2000) {
  dt <- data.table()
 for(p in players){
   dt_temp <- data.table(sample(q_df_pos[s == p]$a,search,replace = TRUE))
   colnames(dt_temp) <- p
   dt<- cbind(dt,dt_temp)
 } 
  unique(dt)
}


actions_samp <- function(players,adj) {
  
  dt <- players[adj, on = "s",nomatch=0][,.SD[sample(.N,1)],by=s]
  
  while(uniqueN(dt$sp) != nrow(dt)){
    
    dt <- players[adj, on = "s",nomatch=0][,.SD[sample(.N,1)],by=s]
    
  }
  
  dt[,list(id,s,a,str)]
  
}




reward_new <- function(trans){
  
  rew_conf <- 0
  
  r <- sum(unique(trans[[3]][,list(s,r)])[trans[[1]], on = 's == sp']$r)
  
 #target[,sp := s] # this command it to add sp column when calculating reward
                    #assumes the 'target' doesn't move.  may not work with actual calcs

  if(trans[[4]] == TRUE){
    
    rconf <- reward_conf(trans)
    
    r <- r + rconf
  }
  
  r
}
# reward_new(tout)

# unique(tout[[3]][,list(s,r)])[tout[[1]], on = 's == sp']



search_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {

  state_real <- state_init
  a_init <- actions(state_init$s,q_dft,10000)
  state_base <- colnames(a_init)
  
  foreach(k=1:nrow(a_init), .combine = rbind,.packages = c('data.table','tidyverse'),
          .errorhandling = 'remove',
          .export = c('actions_samp','reward_new','transition_function','conf_check',
                      'conflict','reward_conf')) %dopar% {
            
    rew <- -Inf
    aout_init <- as.character(a_init[k,])
    
    sa_dt <- data.table(s = state_base,a = aout_init)[state_real, on = 's']
    
    state_init <- q_dft[sa_dt, on = .(s==s,a==a), list(id,s,a,sp,str)]
    
    target_init <- q_dft[target[,a:='adj0'], on = .(s==s,a==a), list(id,s,a,sp,str)]
    
    if(any(!(state_init[,.N, by = sp][N>1,]$sp %in% target_init$s)==TRUE)){
      break
    }
    
    if(any(state_init[s %in% sp,]$sp %in% target_init$s)==TRUE){
      break
    }
    
    trans_start <- transition_function(state_init,target_init,q_dft) #just base enemy stays here action. TODO
    
    
    rew_start <- reward_new(trans_start)
    
    for(i in 1:sims){
      
      df <- data.frame()
      q_dft_sim <- q_dft
      state <- state_init[,list(s = sp, id, str)]
      
      rew_t <- rew_start
      target_sim <- target_init
      
      for(j in 1:depth){
        
        aout <- actions_samp(state[,list(id,s,str)],q_dft_sim)
        
        state <- q_dft[aout, on = .(s==s,a==a), list(id,s,a,sp,str)]
        
        target_sim <- q_dft[target_sim, on = .(s==s,a==a), list(id,s,a,sp,str)]
        
        tran_sim <- transition_function(state,target_sim,q_dft)
        
        rew_t <- rew_t + (disc^j)*reward_new(tran_sim)
        
        if(nrow(tran_sim[[1]])>0){
          state$s <- tran_sim[[1]]$sp
          state$str <- tran_sim[[1]]$str
          state <- state[str>0.1]
        }
        
        
        if(nrow(tran_sim[[2]])>0) {
          target_sim$s <- tran_sim[[2]]$sp
          target_sim$str <- tran_sim[[2]]$str
          target_sim <- target_sim[str>0.1]
        }
        
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- state_init[,list(id,s,a,sp)]
        print(rew)
        print(pol)
        
      }
    }
          
    data.table(r= rew,pol,k)
  }
}




search_no_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {
  
  state_real <- state_init
  a_init <- actions(state_init$s,q_dft,10000)
  state_base <- colnames(a_init)
  rew <- -Inf
  #print(a_init)
  for (k in 1:nrow(a_init)) {
    

    aout_init <- as.character(a_init[k,])
    
    sa_dt <- data.table(s = state_base,a = aout_init)[state_real, on = 's']
    
    state_init <- q_dft[sa_dt, on = .(s==s,a==a), list(id,s,a,sp,str)]
    
    target_init <- q_dft[target[,a:='adj0'], on = .(s==s,a==a), list(id,s,a,sp,str)]
    
    if(any(!(state_init[,.N, by = sp][N>1,]$sp %in% target_init$s)==TRUE)){
      next
    }
    
    if(any(state_init[s %in% sp,]$sp %in% target_init$s)==TRUE){
      next
    }
    #print('pretrans')
    trans_start <- transition_function(state_init,target_init,q_dft) #just base enemy stays here action. TODO

    #(trans_start)
    rew_start <- reward_new(trans_start)
    
    for(i in 1:sims){
      
      df <- data.frame()
      q_dft_sim <- q_dft
      state <- state_init[,list(s = sp, id, str)]
      
      rew_t <- rew_start
      target_sim <- target_init
      
      for(j in 1:depth){
        
        aout <- actions_samp(state[,list(id,s,str)],q_dft_sim)
        
        state <- q_dft[aout, on = .(s==s,a==a), list(id,s,a,sp,str)]
        
        target_sim <- q_dft[target_sim, on = .(s==s,a==a), list(id,s,a,sp,str)]
        #print('preloopy')
        tran_sim <- transition_function(state,target_sim,q_dft)
        
        rew_t <- rew_t + (disc^j)*reward_new(tran_sim)
        
        if(nrow(tran_sim[[1]])>0){
          state$s <- tran_sim[[1]]$sp
          state$str <- tran_sim[[1]]$str
          state <- state[str>0.1]
        } #else {
          #break
        #}

        if(nrow(tran_sim[[2]])>0) {
          target_sim$s <- tran_sim[[2]]$sp
          target_sim$str <- tran_sim[[2]]$str
          target_sim <- target_sim[str>0.1]
        } #else {
          #
        #}

        
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- state_init[,list(id,s,a,sp)]
        print(rew)
        print(pol)
        
      }
    }
  }
  
  data.table(r= rew,pol)
  
}



gradient_function <- function(players,target){
  
  hexdf3 <- hexdf2 %>% 
    #filter(x<=6 & y >= 8) %>%
    mutate(type = case_when(
      pos %in% target ~ 'target',
      pos %in% players ~ 'player',
      T ~ 'none'
    ))
  #print(hexdf3)
  q_df <- adj_df %>%
    filter(s %in% hexdf3$pos | sp %in% hexdf3$pos) %>%
    mutate(r = 0)
  pos_df <- df2 %>%   
    mutate(pos = paste0(
      ifelse(nchar(x)<2,paste0(0,x),x),
      ifelse(nchar(z)<2,paste0(0,z),z),
      ifelse(nchar(y)<2,paste0(0,y),y)
    )) %>%
    ungroup() %>%
    select(pos,x_pos,y_pos)
  
  
  q_df_pos <- q_df %>%
    left_join(pos_df, by = c("s" = "pos"))
  
  tgts <- hexdf3 %>%
    filter(type == 'target') %>%
    select(pos,x_pos,y_pos) %>%
    distinct()
  
  for(i in 1:nrow(q_df_pos)){
    for(j in 1:nrow(tgts)){
      dist <- sqrt((q_df_pos[[i,5]]-tgts[[j,2]])^2 + (q_df_pos[[i,6]]-tgts[[j,3]])^2)
      
      if(dist > 0){
        q_df_pos[[i,4]] <- q_df_pos[[i,4]] - (dist)/10
      }
      
    }
  }
  data.table(q_df_pos)
}



conf_check <- function(players,target,q_df){
  
  conf_all <- data.table(player=character(),target=character())
  
  if(nrow(players[target , on = .(sp == s , s == sp),nomatch = 0]>0)){
    
    
    atkrs <- data.table(player = players[target , on = .(sp == s , s == sp),nomatch = 0]$id,
                        target = players[target , on = .(sp == s , s == sp),nomatch = 0]$i.id )
    
    target[id %in% atkrs$target ,]$sp <- target[id %in% atkrs$target,]$s
    players[id %in% atkrs$player,]$sp <- players[id %in% atkrs$player,]$s
    
    conf_all <- rbind(conf_all,atkrs)
  }
  
  if(nrow(players[target, on = 'sp', nomatch = 0]) > 0) {
    
    atkrs_same <- data.table(player = players[target, on = 'sp', nomatch = 0]$id,
                        target = players[target, on = 'sp', nomatch = 0]$i.id )
    
    conf_all <- rbind(conf_all, atkrs_same)
  }
  
  
  if(nrow(players[target , on = .(sp == s ),nomatch = 0]) > 0) {
    dfenders <- data.table(player = players[target , on = .(sp == s ),nomatch = 0]$id,
                           target = players[target , on = .(sp == s ),nomatch = 0]$i.id )
    
    conf_all <-rbind(conf_all, dfenders)
  }
  
  if(nrow(target[players , on = .(sp == s ),nomatch = 0]) > 0) {
    dfenders2 <- data.table(player = target[players , on = .(sp == s ),nomatch = 0]$i.id,
                            target = target[players , on = .(sp == s ),nomatch = 0]$id )
    
    conf_all <- rbind(conf_all, dfenders2)
  }
  
  unique(conf_all)
} 

transition_function <- function(players,target,q_df,conf_all){
  
  conf_all <- conf_check(players,target,q_df)
  
  if(nrow(conf_all)> 0){
    
    conf_occ <- TRUE
    players_noconf <- players[!(id %in% conf_all$player)]
    players_noconf[,str_old := str]
    
    target_noconf <- target[!(id %in% conf_all$target)]
    target_noconf[,str_old := str]
    
    conf_out <- conflict(conf_all,players,target)
    
    players_conf <- players[conf_out[[1]], on = "id == player"]
    players_conf[,c("str_old","str"):= .(str,str*mod)]
    players_conf[,mod:= NULL]
    players_conf[,sp := s]
    
    players_out <- rbind(players_noconf,players_conf)
    
    target_conf <- target[conf_out[[2]], on = "id == target"]
    target_conf[,c("str_old","str"):= .(str,str*mod)]
    target_conf[,mod:= NULL]
    target_conf[,sp := s]
    target_out <- rbind(target_noconf,target_conf)
    
    
    coinflip <- runif(1)
    #print('coinflip')
    
    if(coinflip > .5){
      
      target_out[sp %in% players_out$sp]$sp <- target_out[sp %in% players_out$sp]$s
      
    } else {
      #print('broken')
      players_out[sp %in% target_out$sp]$sp <- players_out[sp %in% target_out$sp]$s
      
    }
    
    if(nrow(target_out[str == 0])>0) {
      if(nrow(target_out[str>0]) == 0 ){
        q_df$r <- 0
      } else {
        print('transition grad')
        print(players_out[str>0])
        print(target_out[str>0])
        q_df <- gradient_function(players_out[str>0]$s,target_out[str>0]$s)
      }
      
    }
    
  } else {
    conf_occ <- FALSE
    
    players_out <- players[!(id %in% conf_all$player)]
    players_out[,str_old := str]
    
    target_out <- target[!(id %in% conf_all$target)]
    target_out[,str_old := str]
    
  }
  
  list(players_out,target_out,q_df,conf_occ,conf_all)

}
# 
# test1 <- data.table(
#   id = c('inf_1','inf_2'),
#   s = c('030811','040810'),
#   str = 1,
#   a = c('adj2','adj3'),
#   type = 'f'
# )
# 
# 
# test2 <- data.table(
#   id = c('inf_a','inf_b'),
#   s = c('040911','050809'),
#   str = 1,
#   a = c('adj5','adj1'),
#   type = 'e'
# )
# 
# 
# test3 <- q_df_pos[test1, on = .(s==s,a==a), list(id,s,a,sp,str)]
# test4 <- q_df_pos[test2, on = .(s==s,a==a), list(id,s,a,sp,str)]
# 
# conf_check(test3,test4,q_df_pos)
# 
# 
# tout <- transition_function(test3,test4,q_df_pos)
# tout
# 
# ppower <- tout[[4]][,by= player, .(engs_plr =uniqueN(target))]
# ppower[test3, on = 'player == id', atk_plr := str/engs_plr]
# ppower
# tpower <- tout[[4]][,by= target, .(engs_tgt =uniqueN(player))]
# tpower[test4, on = 'target == id', atk_tgt := str/engs_tgt]
# tpower
# 
# fight_tbl <- tout[[4]][tpower, on = "target"]
# fight_tbl <- fight_tbl[ppower, on = 'player']
# fight_tbl
# 
# fight_tbl[, by = player, .(mod = .85^(sum(atk_tgt)/mean(atk_plr)))]
# fight_tbl[, by = target, .(mod = .85^(sum(atk_plr)/mean(atk_tgt)))]
# 
# 



f_players <- data.table(
  id = c('inf_1','inf_2','inf_3'),
  s = c('020711','030912','010712'),
  str = 0.8,
  
  type = 'f'
)


e_target <- data.table(
  id = c('inf_a','inf_b'),
  s = c('050809','071211'),
  str = 1,
  
  #sp = c('071009','081008'),
  type = 'e'
)

eny_obj <- '040810'

q_df_pos <- gradient_function(f_players$s,e_target$s)

q_df_eny <- gradient_function(e_target$s,eny_obj)

#ls1 <- search_no_par(players,3,10,0.99,q_df_pos,target)
# 
# trans <- transition_function(ls1,target,q_df_pos)
# trans
#ls1 <- search_par(players,5,2,0.99,q_df_pos,target)

#players <- new$sp

#search_no_par(target,1,50,.95,q_df_eny,players)
i <- 0
#trans<- list(i = 1,j = data.frame(2))


while(i == 0 | (sum(f_players$str)>0 & sum(e_target$str) >0 & i < 20) ){
  print(i)
  
  if(i ==0) {
    tgt_out <- e_target[,list(id,s,a = 'adj0',str,type)]
    f_players_init <- f_players[,list(id,s,a = 'adj0',str,type)]
    table_out <- rbind(f_players_init,tgt_out)
    table_out[,turn:=i]
  }
  
  #print(tgt_out)
  ls1 <- search_par(f_players,2,50,.5,q_df_pos,tgt_out)
  ls_eny <- search_par(tgt_out,1,20,.5,q_df_eny,f_players)
  
  

  ls1 <- ls1[r == max(r)][,list(r,s,a,sp,id)][,.SD[1],id]
  ls_eny <- ls_eny[r==max(r)][,list(r,s,a,sp,id)][,.SD[1],id]
  print(ls_eny)
  ls1 <- ls1[f_players, on = 'id', list(s,a,sp,id,str)]
  ls_eny <- ls_eny[e_target, on = 'id', list(s,a,sp,id,str)]
  print(ls1)
  print(ls_eny)
  
  trans <- transition_function(ls1,ls_eny,q_df_pos)
  
  #p_trans <- trans
  print(trans[[1]])
  print(trans[[2]])
  f_players <- trans[[1]][,list(id,s=sp,a,str,type = "f")]
  e_target <- trans[[2]][,list(id,s=sp,a,str,type = "e")]
  q_df_pos <- gradient_function(f_players$s,e_target$s)
  i <- i + 1
  
  f_players <- f_players[str>0.1]
  e_target <- e_target[str>0.1]
  
  tgt_out <- e_target[,list(id,s,a,str,type)]
  table_out_temp <- rbind(f_players,tgt_out)
  table_out_temp[,turn := i]
  table_out <- rbind(table_out,table_out_temp)
  
  
}


write_csv(table_out,'test_3v2_24jan23.csv')
### Make sure the friendlies can't occupy the same square!!

test <- data.table(s = c('123','345'),
                   sp = c('567','123'))

tgt <- data.table(s = "568")

if(any(test[s %in% sp,]$sp %in% tgt$s)==TRUE){
  print("skip")
}

if(any(!(test[,.N, by = sp][N>1,]$sp %in% tgt$s)==TRUE)){
  print('skip')
}


#list.of.packages <- c("tidyverse", "foreach","doParallel","data.table")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages, repos = "https://mirror.las.iastate.edu/CRAN/")


library(foreach)
library(doParallel)
library(dplyr)
library(data.table)
#library(gganimate)
#library(ggimage)

source('DS/hex_setup.R')
source('DS/hex_conflict.R')

print(Sys.getenv("SLURM_NTASKS_PER_NODE"))
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

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


f_players <- data.table(
  id = c('inf_1','inf_2','inf_3','inf_4'),
  s = c('030710','030609','040507','060404'),
  str = 1,
  type = 'f'
)


e_target <- data.table(
  id = c('inf_a','inf_b','inf_c','inf_d'),
  s = c('071009','081008','100905','101309'),
  str = 1,
  sp = c('071009','081008','100905','101309'),
  type = 'e'
)

eny_obj <- '020610'

q_df_pos <- gradient_function(f_players$s,e_target$s)

q_df_eny <- gradient_function(e_target$s,eny_obj)

i <- 0

while(i == 0 | (sum(f_players$str)>0 & sum(e_target$str) >0 & i < 50) ){
  print(i)
  
  if(i ==0) {
    tgt_out <- e_target[,list(id,s,str,type)]
    table_out <- rbind(f_players,tgt_out)
    table_out[,turn:=i]
  }
  
  ls1 <- search_par(f_players,3,50,.5,q_df_pos,tgt_out)
  ls_eny <- search_par(tgt_out,1,20,.5,q_df_eny,f_players)
  
  
  
  ls1 <- ls1[r == max(r)][,list(r,s,a,sp,id)][,.SD[1],id]
  ls_eny <- ls_eny[r==max(r)][,list(r,s,a,sp,id)][,.SD[1],id]
  
  ls1 <- f_players[ls1, on = 'id', list(s,a,sp,id,str)]
  ls_eny <- e_target[ls_eny, on = 'id', list(s,a,sp,id,str)]
  print(ls1)
  print(ls_eny)
  
  trans <- transition_function(ls1,ls_eny,q_df_pos)
  
  print(trans[[1]])
  print(trans[[2]])
  f_players <- trans[[1]][,list(id,s=sp,str,type = "f")]
  e_target <- trans[[2]][,list(id,s=sp,str,type = "e")]
  q_df_pos <- gradient_function(f_players$s,e_target$s)
  i <- i + 1
  
  f_players <- f_players[str>0.1]
  e_target <- e_target[str>0.1]
  
  tgt_out <- e_target[,list(id,s,str,type)]
  table_out_temp <- rbind(f_players,tgt_out)
  table_out_temp[,turn := i]
  table_out <- rbind(table_out,table_out_temp)
  
  write_csv(x = table_out,file = paste0('out_table',Sys.Date(),'.csv'))
}


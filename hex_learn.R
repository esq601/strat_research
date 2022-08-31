source('hex_setup.R')
source('hex_conflict.R')

library(foreach)
library(doParallel)
library(tidyverse)
library(data.table)

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

#actions(players$s,q_df_pos,10000)


# actions <- function(players,adj,search = 1000) {
#   
#   a1 <- adj %>%
#     filter(s %in% players) %>%
#     select(s,a) %>%
#     nest(data = a)
#   
#   df_out <- data.frame()
#   
#   for(i in 1:search){
#     df_iter = data.frame(iter = i)
#     for(j in 1:nrow(a1)){
#       a <- sample(a1$data[[j]]$a,size = 1)
#       s <- a1$s[j]
#       df <- data.frame(a)
#       colnames(df) <- s
#       df_iter <- cbind(df_iter,df)
#     }
#     df_out <- rbind(df_out,df_iter)
#   }
#   
#   df_out %>%
#     select(-iter) %>%
#     distinct()
#}

# q_df_pos[s %in% c('061010','010611'),.SD[sample(.N,1)], by = s]

actions_samp <- function(players,adj) {

  
  dt <- players[adj, on = "s",nomatch=0][,.SD[sample(.N,1)],by=s]
  
  
  while(uniqueN(dt$sp) != nrow(dt)){
    #('here')
    dt <- players[adj, on = "s",nomatch=0][,.SD[sample(.N,1)],by=s]
    #print(dt)
  }
  #dt_adj<- dt[s %in% target$s, c('a','sp','r') := .('adj0',s,0)]
  #dt <- players[dt_adj, on = 's']
  dt[,list(id,s,a,str)]
  # dt[s %in% target, ]
  # dt[s %in% target, r := 0]
}



test1 <- data.table(s = c('030508','061010'),id = c('inf1','inf2'),str = 1)

test2 <- data.table(s = c('051212',id = 'infa',str = 1))

#dtest <- q_df_pos[s %in% test1$s,.SD[sample(.N,1)], by = id]

#test1[q_df_pos, on = "s",nomatch=0][,.SD[sample(.N,1)],by=s]
#q_df_pos[test1, on = "s"]

#dtest[,list(id,s,a,str)]


actions_samp(test1,test2,q_df_pos)

reward_new <- function(trans){
  
  rew_conf <- 0
  
  #print(trans[[1]])
  #print(trans[[3]])
  
  #print(unique(trans[[3]][,list(s,r)])[trans[[1]], on = 's == sp'])
  r <- sum(unique(trans[[3]][,list(s,r)])[trans[[1]], on = 's == sp']$r)
  
 #target[,sp := s] # this command it to add sp column when calculating reward
                    #assumes the 'target' doesn't move.  may not work with actual calcs

  if(trans[[4]] == TRUE){
    #print('conf')
    rconf <- reward_conf(trans)
    #print(rconf)
    r <- r + rconf
  }
  #r_conf <- reward_conf(trans)
  #print(r_conf)
  #group_bonus <- sa[sp %in% target & !(s %in% target), .(.N), by = .(sp)]
  #group_bonus <- group_bonus[,N := 5^N]
  #r <- r_loc + r_conf#sum(group_bonus$N)
  
  r
}
# reward_new(tout)

# unique(tout[[3]][,list(s,r)])[tout[[1]], on = 's == sp']


cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


search_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {

  state_real <- state_init
  a_init <- actions(state_init$s,q_dft,10000)
  state_base <- colnames(a_init)
  
  foreach(k=1:nrow(a_init), .combine = rbind,.packages = c('data.table','tidyverse'),
          .export = c('actions_samp','reward_new')) %dopar% {
            
    rew <- -Inf
    aout_init <- as.character(a_init[k,])
    
    sa_dt <- data.table(s = state_base,a = aout_init)[state_real, on = 's']
    
    state_init <- q_dft[sa_dt, on = .(s==s,a==a)]
    
    if(any(!(state_init[,.N, by = sp][N>1,]$sp %in% target$s)==TRUE)){
      print('skip')
      next
    }
    
    if(any(state_init[s %in% sp,]$sp %in% target$s)==TRUE){
      print("skip follower")
      next
    }
    
    rew_start <- reward_new(state_init,target)
    
    for(i in 1:sims){
      
      #df <- data.frame()
      q_dft_sim <- q_dft
      state <- state_init[,list(s = sp, id, str)]
      
      rew_t <- rew_start
      target_sim <- target
      
      for(j in 1:depth){
        
        
        aout <- actions_samp(state,q_dft_sim,target_sim)

        
        rew_t <- rew_t + (disc^j)*reward_new(aout,target_sim)
        #print(reward_new(aout,target))
        if(any(aout$sp %in% target_sim$s)){
          
          conf_out <- conflict(aout[sp %in% target_sim$s],target_sim[s %in% aout$sp])
          
          aout[sp %in% target_sim$s]$str <- conf_out[[1]]$str
          target_sim[s %in% aout$sp]$str <- conf_out[[2]]$str
          #print(conf_out)
          
          if(nrow(target_sim[str == 0])>0) {
            
            if(nrow(target_sim[str>0]) == 0 ){
              q_dft_sim$r <- 0
            } 
          }
          
          aout <- aout[str>0]
          aout[sp %in% target$s]$sp <- aout[sp %in% target$s]$s
          
          target_sim <- target_sim[str>0]
        }
        #print(rew_t)
        state$s <- aout$sp
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- unique(sa_dt)
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
    #print(sa_dt)
    state_init <- q_dft[sa_dt, on = .(s==s,a==a), list(id,s,a,sp,str)]
    #print(state_init)
    
    target_init <- q_dft[target[,a:='adj0'], on = .(s==s,a==a), list(id,s,a,sp,str)]
    #print(state_init)
    
    if(any(!(state_init[,.N, by = sp][N>1,]$sp %in% target_init$s)==TRUE)){
      #print('skip')
      next
    }
    
    if(any(state_init[s %in% sp,]$sp %in% target_init$s)==TRUE){
      #print("skip follower")
      next
    }
    
     #print(state_init)
    # print(target_init)
    trans_start <- transition_function(state_init,target_init,q_dft) #just base enemy stays here action. TODO
    #print(trans_start)
    rew_start <- reward_new(trans_start)
    # print(rew_start)
    for(i in 1:sims){
      
      df <- data.frame()
      q_dft_sim <- q_dft
      state <- state_init[,list(s = sp, id, str)]
      
      rew_t <- rew_start
      target_sim <- target_init
      # print('sim')
      # print(state)
      #print('first')
      #print(state)
      for(j in 1:depth){
        
        #print(state)
        aout <- actions_samp(state[,list(id,s,str)],q_dft_sim)
        
        #print('state')
        #print(aout)
        
        state <- q_dft[aout, on = .(s==s,a==a), list(id,s,a,sp,str)]
        
        
        target_sim <- q_dft[target_sim, on = .(s==s,a==a), list(id,s,a,sp,str)]
        
        #print(state)
        #print(target_sim)
        
        tran_sim <- transition_function(state,target_sim,q_dft)
        
 
        #print(tran_sim[[4]])
        rew_t <- rew_t + (disc^j)*reward_new(tran_sim)
        #print(rew_t)
        state$s <- tran_sim[[1]]$sp
        #print(state)
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- trans_start[[1]][,list(id,s,a,sp)]
        # print(rew)
        # print(pol)
      }
    }
  }
  
  data.table(r= rew,pol)
  
}


test <- search_no_par(f_players,2,50,.95,q_df_pos,e_target)
##############ls1 <- search_no_par(players,2,10,0.99,q_df_pos,target)

# Reward gradient
#players <- c('050809','051011')
#target <- '060706'

#gradient_function(stest,ttest)

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

#q_df_pos[ls1, on = .(s==s,a==a), list(id,s,sp,str)]
#ls_eny[ls1, on = "sp",nomatch = 0]


conf_check <- function(players,target,q_df){
  
    #print(players_move)
  #target <- q_df[target, on = .(s==s,a==a), list(id,s,sp,str)]
  #print(target)
  #print(players_move[target , on = .(sp == s , s == sp),nomatch = 0])
  conf_all <- data.table(player=character(),target=character())
  
  #print(players)
  
  
  if(nrow(players[target , on = .(sp == s , s == sp),nomatch = 0]>0)){
    
    
    atkrs <- data.table(player = players[target , on = .(sp == s , s == sp),nomatch = 0]$id,
                        target = players[target , on = .(sp == s , s == sp),nomatch = 0]$i.id )
    
    target[id %in% atkrs$target ,]$sp <- target[id %in% atkrs$target,]$s
    players[id %in% atkrs$player,]$sp <- players[id %in% atkrs$player,]$s
    
    conf_all <- rbind(conf_all,atkrs)
  }
  #print(target)
  #(players)
  if(nrow(players[target, on = 'sp', nomatch = 0]) > 0) {
    
    #print('attacking same')
    #print(players)
    #print(players[target, on = 'sp', nomatch = 0])
    atkrs_same <- data.table(player = players[target, on = 'sp', nomatch = 0]$id,
                        target = players[target, on = 'sp', nomatch = 0]$i.id )
    # print(atkrs_same)
    # print(conf_all)
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
  #print(conf_all)
  conf_all
} ### Stopped here 30aug

transition_function <- function(players,target,q_df,conf_all){
  #print(players)
  #print('pre func')
  #print(players)
  conf_all <- conf_check(players,target,q_df)
  
  
  if(nrow(conf_all)> 0){
    #print('conf')
    #print(conf_all)
    conf_occ <- TRUE
    players_noconf <- players[!(id %in% conf_all$player)]
    players_noconf[,str_old := str]
    #print(players_noconf)
    # print(target)
    # print(conf_all)
    target_noconf <- target[!(id %in% conf_all$target)]
    target_noconf[,str_old := str]
    # print(target_noconf)
    
    
    conf_out <- conflict(conf_all,players,target)
    # print(conf_out)
    
    
    players_conf <- players[conf_out[[1]], on = "id == player"]
    players_conf[,c("str_old","str"):= .(str,str*mod)]
    players_conf[,mod:= NULL]
    
    players_out <- rbind(players_noconf,players_conf)
    
    target_conf <- target[conf_out[[2]], on = "id == target"]
    target_conf[,c("str_old","str"):= .(str,str*mod)]
    target_conf[,mod:= NULL]
    
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
    # print(players)
    # print(target)
  } else {
    conf_occ <- FALSE
    #print(conf_occ)
    #print(players)
    players_out <- players[!(id %in% conf_all$player)]
    players_out[,str_old := str]
    
    target_out <- target[!(id %in% conf_all$target)]
    target_out[,str_old := str]
    
  }
  

  
  #print(players)
  list(players_out,target_out,q_df,conf_occ)

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
  id = c('inf_1','inf_2'),
  s = c('040810','050708'),
  str = 1,
  type = 'f'
)


e_target <- data.table(
  id = c('inf_a'),
  s = c('070908'),
  str = 1,
  #sp = c('071009','081008'),
  type = 'e'
)

eny_obj <- '020509'

q_df_pos <- gradient_function(f_players$s,e_target$s)

q_df_eny <- gradient_function(e_target$s,eny_obj)

# ls1 <- search_no_par(players,3,10,0.99,q_df_pos,target)
# 
# trans <- transition_function(ls1,target,q_df_pos)
# trans
#ls1 <- search_par(players,5,2,0.99,q_df_pos,target)

#players <- new$sp

#search_no_par(target,1,50,.95,q_df_eny,players)
i <- 0
#trans<- list(i = 1,j = data.frame(2))


while(i == 0 | (sum(f_players$str)>0 & sum(e_target$str) >0 & i < 15) ){
  print(i)
  
  if(i ==0) {
    tgt_out <- e_target[,list(id,s,str,type)]
    table_out <- rbind(f_players,tgt_out)
    table_out[,turn:=i]
  }
  
  #print(tgt_out)
  ls1 <- search_no_par(f_players,3,50,.95,q_df_pos,tgt_out)
  ls_eny <- search_no_par(tgt_out,1,20,.95,q_df_eny,f_players)
  
  

  ls1 <- ls1[r == max(r)][,list(r,s,a,sp,id,str)]
  ls_eny <- ls_eny[r==max(r)][,list(r,s,a,sp,id,str)]
  #print(ls_eny)
  ls1 <- f_players[ls1, on = 'id', list(s,a,sp,id,str)]
  ls_eny <- e_target[ls_eny, on = 'id', list(s,a,sp,id,str)]
  print(ls1)
  print(ls_eny)
  trans <- transition_function(ls1,ls_eny,q_df_pos)
  
  #p_trans <- trans
  print(trans[[1]])
  print(trans[[2]])
  f_players <- trans[[1]][,list(id,s=sp,str,type = "f")]
  e_target <- trans[[2]][,list(id,s=sp,str,type = "e")]
  q_df_pos <- trans[[3]]
  i <- i + 1
  
  tgt_out <- e_target[,list(id,s,str,type)]
  table_out_temp <- rbind(f_players,tgt_out)
  table_out_temp[,turn := i]
  table_out <- rbind(table_out,table_out_temp)
  
  
}



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



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




reward_new <- function(trans,grad){
  
  rew_conf <- 0
  
  r <- sum(unique(grad[,list(s,r)])[trans[[1]], on = 's == sp']$r)  #old one
  #r <- nrow(trans[[1]])*-0.05  #For MCTS
  #target[,sp := s] # this command it to add sp column when calculating reward
  #assumes the 'target' doesn't move.  may not work with actual calcs
  
  if(trans[[3]] == TRUE){
    
    rconf <- reward_conf(trans)
    
    r <- r + rconf
  }
  
  r
}




gradient_function <- function(players,target){
  print('grad!')
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
        q_df_pos[[i,4]] <- q_df_pos[[i,4]] - (dist)/500
      }
      
    }
  }
  data.table(q_df_pos)
}



conf_check <- function(players,target){
  
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

transition_function <- function(players,target){
  
  conf_all <- conf_check(players,target)
  
  if(nrow(conf_all)> 0){
    
    conf_occ <- TRUE
    players_noconf <- players[!(id %in% conf_all$player)]
    players_noconf[,str_old := str]
    
    target_noconf <- target[!(id %in% conf_all$target)]
    target_noconf[,str_old := str]
    
    conf_out <- conflict(conf_all,players,target)
    
    players_conf <- players[conf_out[[1]], on = "id == player"]
    players_conf[,c("str_old","str"):= .(str,str-mod)]
    players_conf[,mod:= NULL]
    players_conf[,sp := s]
    
    players_out <- rbind(players_noconf,players_conf)
    
    target_conf <- target[conf_out[[2]], on = "id == target"]
    target_conf[,c("str_old","str"):= .(str,str-mod)]
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
        #q_df$r <- 0  #modifying for mcts test
      } else {
        print('transition grad')
        print(players_out[str>0])
        print(target_out[str>0])
        #q_df <- gradient_function(players_out[str>0]$s,target_out[str>0]$s) # modifying for mcts test
      }
      
    }
    
  } else {
    conf_occ <- FALSE
    
    players_out <- players[!(id %in% conf_all$player)]
    players_out[,str_old := str]
    
    target_out <- target[!(id %in% conf_all$target)]
    target_out[,str_old := str]
    
  }
  players_out[order(-id)]
  target_out[order(id)]
  list(players_out,target_out,conf_occ,conf_all)
  
}



utility_func <- function(q_lst, state_vec){
  s_ind <- which(sapply(q_lst$s,identical, y = state_vec))
  return(max(unlist(q_lst$q[s_ind]))[1])
}

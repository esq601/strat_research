
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

trunc_func <- function(val,floor=-3, ceiling = 3){
  if(val > ceiling){
    out <- ceiling
  } else if (val < floor){
    out <- floor
  } else {
    out <- round(val)
  }
  
  return(out)
}

trunc_func(-1.72)
grad_func <- function(terr_df,unit_df){
  #print(terr_df)
  #print(terr_df_x)
  # print(unit_df)
  # print(terr_df)
  # 
  #v1 <- sapply(unit_df$lst[[1]], function(x) x - terr_df[[1]])
  v1 <- unit_df$x_pos - terr_df[[1]]
  v2 <- sapply(unit_df$lst[[2]], function(x) x - terr_df[[2]])
  v2 <- unit_df$y_pos - terr_df[[2]]
  # print('v1')
  # print(v1)
  # print('v2')
  # print(v2)
  unit_df$score <- unit_df$str / sqrt(v1^2 + v2^2) 
  
  #print(unit_df)
  # print(unit_df$score)
  #out <- ifelse(sum(unit_df[type == 'f']$score) - sum(unit_df[type == 'e']$score) >= 0,1,-1)
  out <- trunc_func(sum(unit_df[type == 'f']$score) - sum(unit_df[type == 'e']$score))
  # df <- merge(rbind(trans[[1]],trans[[2]]),grad, by.y = 'pos',by.x = 's')
  # 
  # r <- c*sum(sapply(grad$lst, grad_func,unit_df = df))/nrow(grad)
  #print(out)
  return(out)
}

grad_reward <- function(trans,grad,c = 1){
  
  #print(rbind(trans[[1]],trans[[2]]))
  
  transout <- rbind(trans[[1]],trans[[2]])
  #print('yo')
  
  df <- merge(transout,grad, by.y = 'pos',by.x = 's')
  #print(df)
  #print(df)
  r <- c*sum(sapply(grad$lst, grad_func,unit_df = df))/nrow(grad)
  #print(r)
  return(r)
}


reward_new <- function(trans,grad,grad_reward,c = 1){
  
  r <- grad_reward
  
  #print(trans)
  
  #r <- sum(unique(grad[,list(s,r)])[trans[[1]], on = 's == sp']$r)  #old one
  


  #print(r)
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
        q_df_pos[[i,4]] <- 0#q_df_pos[[i,4]] - (dist)/500
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
  
  if(
    any(
      players$sp %in% target$sp,
      players$s %in% target$sp & players$sp %in% target$s
    ) == TRUE
  ) {
    conf_all <- conf_check(players,target)
    
    
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
  } else {
    conf_occ <- FALSE
    conf_all <- data.table()
    
    players_out <- players[!(id %in% conf_all$player)]
    players_out[,str_old := str]
    
    target_out <- target[!(id %in% conf_all$target)]
    target_out[,str_old := str]
    
  }
  
  list(players_out,target_out,conf_occ,conf_all)
  
}



utility_func <- function(q_lst, state_vec){
  s_ind <- which(q_lst$s$s %in% state_vec)
  #print(s_ind)
  #print(unlist(q_lst$q[s_ind]))
  return(max(unlist(q_lst$q[s_ind]))[1])
}


prob_setup <- function(samep = .2, adjp = .175, adjbp = .125, backp = .05, stayp = 0.15){
  
  total <- samep + 2*adjp + 2*adjbp + backp + stayp
  print(total)
  if(total != 1){
    stop("Probability total not equal to 1")
    
  }
  adj1_tbl <- data.table(a = c(rep('adj1',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,samep,adjp,adjbp,backp,adjbp,adjp))
  adj2_tbl <- data.table(a = c(rep('adj2',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjp,samep,adjp,adjbp,backp,adjbp))
  adj3_tbl <- data.table(a = c(rep('adj3',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjbp,adjp,samep,adjp,adjbp,backp))
  adj4_tbl <- data.table(a = c(rep('adj4',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,backp,adjbp,adjp,samep,adjp,adjbp))
  adj5_tbl <- data.table(a = c(rep('adj5',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjbp,backp,adjbp,adjp,samep,adjp))
  adj6_tbl <- data.table(a = c(rep('adj6',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjp,adjbp,backp,adjbp,adjp,samep))
  adj0_tbl <- data.table(a = c(rep('adj0',7)), nexta = c(paste0("adj",0:6)), p = c(1/7))
  
  prob_tran <- rbind(adj0_tbl,adj1_tbl,adj2_tbl,adj3_tbl,adj4_tbl,adj5_tbl,adj6_tbl)
  return(prob_tran)
}
#prob_setup()


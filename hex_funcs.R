
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



reward_new <- function(trans,grad_reward,mode = 'mult'){
  
  r <- grad_reward
  
  #print(trans)
  
  #r <- sum(unique(grad[,list(s,r)])[trans[[1]], on = 's == sp']$r)  #old one
  


  #print(r)
  #r <- nrow(trans[[1]])*-0.05  #For MCTS
  #target[,sp := s] # this command it to add sp column when calculating reward
  #assumes the 'target' doesn't move.  may not work with actual calcs
  rdt <- data.table(id = trans[[1]]$id, val = 0)
  if(trans[[3]] == TRUE ){

    rconf <- reward_conf(trans, mode)
    # print(trans)
    # print(rconf)
    if(mode == 'mult') {
      r <- .2*r + .8*rconf[[1]]
      
      rdt <- rconf[[2]]
    } else {
      r <- .5* r + .5* rconf[[1]]
      rdt <- rconf[[2]]
    }

  }
  
  return(list(r,rdt))
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




conf_check2 <- function(players,target){
  
  out <- data.table()
  
  if(nrow(players) > 0 & nrow(target) > 0){
    
    f_vec <- lapply(players$sp,
                    function(x) c(as.integer(substr(x, 1, 2)), 
                                  as.integer(substr(x, 3, 4)),
                                  as.integer(substr(x, 5, 6))))
    
    e_vec <- lapply(target$sp,
                    function(x) c(as.integer(substr(x, 1, 2)), 
                                  as.integer(substr(x, 3, 4)),
                                  as.integer(substr(x, 5, 6))))
    
    # f_vec <- as.vector(sapply(players$sp,
    #                           function(x) sum(as.integer(substr(x, 1, 2)), 
    #                                           as.integer(substr(x, 3, 4)),
    #                                           as.integer(substr(x, 5, 6))))
    # )
    # 
    # f_lvl <- as.vector(sapply(players$sp,
    #                           function(x) as.integer(substr(x, 5, 6)))
    # )
    # 
    # e_vec <- as.vector(sapply(target$sp,
    #                           function(x) sum(as.integer(substr(x, 1, 2)), 
    #                                           as.integer(substr(x, 3, 4)), 
    #                                           as.integer(substr(x, 5, 6))))
    # )
    # 
    # e_lvl <- as.vector(sapply(target$sp,
    #                           function(x) as.integer(substr(x, 5, 6)))
    # )
    # print(f_vec)
    # print(f_lvl)
    # print(e_vec)
    # print(e_lvl)
    # matches <- lapply(f_vec,find_indices,vec  = e_vec)
    matches <- compare_lists(f_vec, e_vec)
    #print(matches)
    for(i in 1:length(matches)){
      
      if(length(matches[[i]])>0){
        
        for(j in 1:length(matches[[i]])){
          
          out_t <- data.table(player = players[i]$id,target = target[matches[[i]][[j]]]$id)
          
          out <- rbind(out,out_t)
          
        }
      }
      
    }
  }
  return(out)
}



find_indices <- function(num, vec) {
  # Find indices of elements in vec that are plus or minus 2 from num
  idx <- which(abs(vec - num) == 2 | vec == num)
  return(idx)
}




transition_function2 <- function(plrs,trgt,key_terrain){
  
  df_t <- data.table()
  
  t1 <- Sys.time()
  
  conf_all <- conf_check2(plrs,trgt)
  
  df_t <- rbind(df_t, data.table(event = 'conf_check2',t = Sys.time()-t1))
  #print(conf_all)
  plrs1 <- plrs[,str_old := str]

  trgt1 <- trgt[,str_old := str]
  
  if(nrow(conf_all) > 0  ) {
    t1 <- Sys.time()
    
    conf_occ <- TRUE 
    confout <- conflict(conf_all,plrs,trgt)
    
    df_t <- rbind(df_t, data.table(event = 'conflict',t = Sys.time()-t1))
    
    t1 <- Sys.time()
    plrvec <- paste0(plrs1$sp,plrs1$s)
    tgtvec <- paste0(trgt1$sp,trgt1$s)
    
    # Modify strength based on conflict
    plrs1[confout[[1]], on = c(id = 'player'),str := str-mod]
    
    # Dont allow negative strength
    plrs1[str < 0, str := 0]
    
    # Check for units that attacked into each other
    plrs1[paste0(s,sp)%in%tgtvec,sp:=s]
    
    # Check for units that attacked into stationary
    plrs1[sp %in% trgt1[s == sp,]$sp,sp := s]

    
    # Work on targets

    trgt1[confout[[2]], on = c(id = 'target'),str := str-mod]
    
    trgt1[str < 0, str := 0]
    trgt1[paste0(s,sp)%in%plrvec,sp:=s]
    
    
    trgt1[sp %in% plrs1[s == sp,]$sp,sp := s]
    
    
    # Check for units that ended on the same space

    df_t <- rbind(df_t, data.table(event = 'restofconf',t = Sys.time()-t1))
    
    

  }else{
    conf_occ <- FALSE
  }
  t1 <- Sys.time()
  # Check for units moving into the same space
  if(any(duplicated(c(plrs1$sp,trgt1$sp))) == TRUE){
    comb <- rbind(plrs1,trgt1)
    
    # Sample who gets the space according to strength ratios
    comb <- comb[, .SD[sample(.N, 1, prob = str_old)], by = sp]
    
    # If the unit is not selected by sample, it's returned to original space
    plrs1[,sp := ifelse(id %in% comb$id,sp,s)]
    trgt1[,sp := ifelse(id %in% comb$id,sp,s)]
    
    
    # psame <- plrs1[,.(un = uniqueN(.SD)),by = 'sp']
    # tsame <- trgt1[,.(un = uniqueN(.SD)),by = 'sp']
    allsame <- rbind(plrs1,trgt1)[,.(uno = uniqueN(.SD)),by = 'sp']
    # print(max(c(psame$un,tsame$un)))
    # print(psame)
    # print(tsame)
    while(max(c(allsame$uno))>1){
      
      psame <- plrs1[,.(un = uniqueN(.SD)),by = 'sp']
      tsame <- trgt1[,.(un = uniqueN(.SD)),by = 'sp']
      
      plrs1[sp %in% c(trgt1$sp,psame[un>1]$sp), sp := s]
      trgt1[sp %in% c(plrs1$sp,tsame[un>1]$sp), sp := s]
      
      
      # psame <- plrs1[,.(un = uniqueN(.SD)),by = 'sp']
      # tsame <- trgt1[,.(un = uniqueN(.SD)),by = 'sp']
      
      allsame <- rbind(plrs1,trgt1)[,.(uno = uniqueN(.SD)),by = 'sp']
      # print(psame)
      # print(tsame)
      # print(allsame)
    }
  } 

  df_t <- rbind(df_t, data.table(event = 'movesame_chk',t = Sys.time()-t1))
  # print(plrs1)
  # print(c(plrs1$sp,trgt1$sp))
  # print(key_terrain$s)
  # print(any(c(plrs1$sp,trgt1$sp) %in% key_terrain$s))
  if(any(c(plrs1$sp,trgt1$sp) %in% key_terrain$s) == TRUE){
    
    # print('yo')
    dt1 <- data.table(s = c(plrs1$sp,trgt1$sp),type = c(rep('f',length(plrs1$id)),
                                                        rep('e',length(trgt1$id))))
    # print(dt1)
    # print(key_terrain)
    territory <- merge(key_terrain,dt1, by = "s", all.x = TRUE, suffixes = c("", "_y"))
    
    changeval <- sum(territory[type_y == 'f' & type == 'e']$value) - sum(territory[type_y == 'e'  & type == 'f']$value)
    # print(territory)
    # print(changeval)
    territory[, type := ifelse(is.na(type_y), type, type_y)]
    territory[, type_y := NULL]
  } else {
    territory <- key_terrain
    changeval <- 0
  }

  # print(territory)
  return(list(plrs1,trgt1,conf_occ,conf_all,df_t,territory,changeval))
  
}



utility_func <- function(q_lst, state_vec){
  
  s_ind <- which(q_lst$s$s %in% state_vec)
  #print(s_ind)
  # print(unlist(q_lst$q[s_ind]))
  return(max(unlist(q_lst$q[s_ind]))[1])
}

prob_setup <- function(samep = .9, adjp = .025, adjbp = .01, backp = .01, stayp = 0.02){
#prob_setup <- function(samep = .575, adjp = .125, adjbp = .05, backp = .025, stayp = 0.05){
  
  total <- samep + 2*adjp + 2*adjbp + backp + stayp
  #print(total)
  if(total != 1){
    stop("Probability total not equal to 1")
    
  }
  adj1_tbl <- data.table(a = c(rep('adj1',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,samep,adjp,adjbp,backp,adjbp,adjp))
  adj2_tbl <- data.table(a = c(rep('adj2',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjp,samep,adjp,adjbp,backp,adjbp))
  adj3_tbl <- data.table(a = c(rep('adj3',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjbp,adjp,samep,adjp,adjbp,backp))
  adj4_tbl <- data.table(a = c(rep('adj4',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,backp,adjbp,adjp,samep,adjp,adjbp))
  adj5_tbl <- data.table(a = c(rep('adj5',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjbp,backp,adjbp,adjp,samep,adjp))
  adj6_tbl <- data.table(a = c(rep('adj6',7)), nexta = c(paste0("adj",0:6)), p = c(stayp,adjp,adjbp,backp,adjbp,adjp,samep))
  adj0_tbl <- data.table(a = c(rep('adj0',7)), nexta = c(paste0("adj",0:6)), p = c(.5,1/10,1/10,1/10,1/10,1/10,1/10))
  
  prob_tran <- rbind(adj0_tbl,adj1_tbl,adj2_tbl,adj3_tbl,adj4_tbl,adj5_tbl,adj6_tbl)
  return(prob_tran)
}
ptest <- prob_setup()


compare_lists <- function(list1, list2) {
  lapply(list1, function(x) {
    # create a boolean vector to store the comparison results for each element in list2
    which(sapply(list2, function(y) {
      # calculate the maximum difference between each element of x and y
      max_diff1 <- max(abs(x-y))
      #max_diff2 <- max(abs(diff(y)))
      
      # calculate the difference between the sums of x and y
      sum_diff <- abs(sum(x) - sum(y))
      
      # return a boolean indicating if the criteria are met
      sum_diff <= 2 & max_diff1 <=1
    }))
  })
}


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



reward_new <- function(trans,grad_reward,mode = 'mult',ft_wt = 0.75, tr_wt = 0.25){
  
  r <- tr_wt*grad_reward
  
  #print(trans)
  
  #r <- sum(unique(grad[,list(s,r)])[trans[[1]], on = 's == sp']$r)  #old one
  


  #print(r)
  #r <- nrow(trans[[1]])*-0.05  #For MCTS
  #target[,sp := s] # this command it to add sp column when calculating reward
  #assumes the 'target' doesn't move.  may not work with actual calcs
  rdt <- data.table(id = trans[[1]]$id,s=trans[[1]]$s,a=trans[[1]]$a, val = r)
  if(trans[[3]] == TRUE ){

    rconf <- reward_conf(trans,ft_wt,tr_wt)

    
    rdt <- rconf[[2]]
    #print(rdt)
  }
  #print(rdt)
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
  df_t <- data.table()
  # t1 <- Sys.time()
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

    # df_t <- rbind(df_t, data.table(event = 'conf1_vects',t = Sys.time()-t1))
    t1 <- Sys.time()
    # print(f_vec)
    # print(e_vec)
    matches <- compare_lists(f_vec, e_vec)
    matches1 <- compare_lists(e_vec, f_vec)
    # print('Lists')
    # print(matches)
    # print('list2')
    # print(matches1)
    # print('vectors')
    # print(unlist(matches))
    # 
    # print((unlist(matches1)))

    # df_t <- rbind(df_t, data.table(event = 'conf2_comp',t = Sys.time()-t1))
    # #print(matches)
    # t1 <- Sys.time()
    
    plt <- players[unlist(matches1),.(player = id,f_str = str)]
    plt$fmod <- 0.25
    # print(plt)
    plt[,f_str := fmod*(f_str / .N), by = player]
    tgt <- target[unlist(matches),.(target = id,e_str = str)]
    tgt$emod <- 0.25
    tgt[,e_str := emod*(e_str / .N), by = target]
    #print(plt)
    out <- cbind(plt,tgt)
    # print(out)
    
    eout <- out[, .(mod = floor(sum(f_str))), by = target]
    fout <- out[, .(mod = floor(sum(e_str))), by = player]
    # print(fout)
    # print(eout)
    
    # df_t <- rbind(df_t, data.table(event = 'conf3_loop',t = Sys.time()-t1))
    
    # for(i in 1:length(matches)){
    #   
    #   if(length(matches[[i]])>0){
    #     
    #     for(j in 1:length(matches[[i]])){
    #       
    #       out_t <- data.table(player = players[i]$id,target = target[matches[[i]][[j]]]$id)
    #       
    #       out <- rbind(out,out_t)
    #       
    #       df_t <- rbind(df_t, data.table(event = 'conf3_loop',t = Sys.time()-t1))
    #       
    #     }
    #   }
    #   
    # }
  }
  return(list(out,df_t,fout,eout))
}



find_indices <- function(num, vec) {
  # Find indices of elements in vec that are plus or minus 2 from num
  idx <- which(abs(vec - num) == 2 | vec == num)
  return(idx)
}




transition_function2 <- function(plrs,trgt,key_terrain){
  
  t0 <-Sys.time()
  
  df_t <- data.table()
  
  
  #print(conf_all)
  plrs$str_old <- plrs$str

  trgt$str_old <- trgt$str
  
  

  
  # df_t <- rbind(df_t, data.table(event = 'ex0_idfk',t = Sys.time()-t0))
  ob1 <-  Sys.time()-t0
  t1 <- Sys.time()
  # Check for units moving into the same space
  if(any(duplicated(c(plrs$sp,trgt$sp))) == TRUE){
    # print(plrs)
    # print(trgt)

    
    comb <- rbind(plrs,trgt)
    
    # Sample who gets the space according to strength ratios
    comb <- comb[, .SD[sample(.N, 1, prob = str_old)], by = sp]
    
    # If the unit is not selected by sample, it's returned to original space
    plrs[,sp := ifelse(id %in% comb$id,sp,s)]
    trgt[,sp := ifelse(id %in% comb$id,sp,s)]
    
    
    # psame <- plrs[,.(un = uniqueN(.SD)),by = 'sp']
    # tsame <- trgt[,.(un = uniqueN(.SD)),by = 'sp']
    #allsame <- rbind(plrs,trgt)[,.(uno = uniqueN(.SD)),by = 'sp']


# 
#     ck_out <- check_and_correct_moves_vec(c(plrs$s,trgt$s),c(plrs$sp,trgt$sp))
#     
#     plrs$sp <- ck_out[1:nrow(plrs)]
#     trgt$sp <- ck_out[(nrow(plrs)+1):length(ck_out)]
    # while(max(c(allsame$uno))>1){
    # 
    # 
    #   # print(allsame)
    #   # print(plrs)
    #   # print(trgt)
    #   t1a <- Sys.time()
    #   psame <- plrs[,.(un = uniqueN(.SD)),by = 'sp']
    #   tsame <- trgt[,.(un = uniqueN(.SD)),by = 'sp']
    # 
    #   plrs[sp %in% c(trgt$sp,psame[un>1]$sp), sp := s]
    #   trgt[sp %in% c(plrs$sp,tsame[un>1]$sp), sp := s]
    # 
    # 
    #   # psame <- plrs[,.(un = uniqueN(.SD)),by = 'sp']
    #   # tsame <- trgt[,.(un = uniqueN(.SD)),by = 'sp']
    # 
    #   allsame <- rbind(plrs,trgt)[,.(uno = uniqueN(.SD)),by = 'sp']
    # 
    #   df_t <- rbind(df_t, data.table(event = 'ex1a_same_loop',t = Sys.time()-t1a))
    # }
  } 
  ob2 <-  Sys.time()-t1
  t1 <- Sys.time()
  # df_t <- rbind(df_t, data.table(event = 'ex1_same_ck',t = Sys.time()-t1))
  # 
  # t1 <- Sys.time()
  

  # plrvec <- paste0(plrs$sp,plrs$s)
  # 
  # tgtvec <- paste0(trgt$sp,trgt$s)
  
  
  # Dont allow negative strength
  
  
  # Check for units that attacked into each other
  # plrs[paste0(s,sp)%in%tgtvec,sp:=s]
  # plrs[paste0(s,sp)%in%plrvec,sp:=s]
  # # Check for units that attacked into stationary
  # plrs[sp %in% trgt[s == sp,]$sp,sp := s]
  # plrs[sp %in% plrs[s == sp,]$sp,sp := s]
  # 
  # trgt[paste0(s,sp)%in%plrvec,sp:=s]
  # trgt[paste0(s,sp)%in%tgtvec,sp:=s]
  # 
  # trgt[sp %in% plrs[s == sp,]$sp,sp := s]
  # trgt[sp %in% trgt[s == sp,]$sp,sp := s]
  ck_out <- check_and_correct_moves_vec(c(plrs$s,trgt$s),c(plrs$sp,trgt$sp))
  
  
  
  #ck_out <- check_and_correct_moves(rbind(plrs,trgt))
  
  # print(ck_out)
  # print(plrs)
  # print(trgt)
  # print(length(ck_out))
  # print(1:nrow(plrs))
  # print((nrow(plrs)+1):length(ck_out))
  
  plrs$sp <- ck_out[1:nrow(plrs)]
  trgt$sp <- ck_out[(nrow(plrs)+1):length(ck_out)]
  # 
  # df_t <- rbind(df_t, data.table(event = 'ex2_preconf_ck',t = Sys.time()-t1))
  # 
  # t1 <- Sys.time()
  ob3 <-  Sys.time()-t1
  t1 <- Sys.time()
  
  if(any(c(plrs$sp,trgt$sp) %in% key_terrain$s) == TRUE){
    
    dt1 <- data.table(s = c(plrs$sp,trgt$sp),type = c(rep('f',length(plrs$id)),
                                                        rep('e',length(trgt$id))))
    
    territory <- merge(key_terrain,dt1, by = "s", all.x = TRUE, suffixes = c("", "_y"))
    
    changeval <- (sum(territory[type_y == 'f' & type == 'e']$value) - sum(territory[type_y == 'e'  & type == 'f']$value))/sum(key_terrain$value)
    # print(territory)
    # print(changeval)
    territory[, type := ifelse(is.na(type_y), type, type_y)]
    territory[, type_y := NULL]
  } else {
    territory <- key_terrain
    changeval <- 0
  }
  
  ob4 <-  Sys.time()-t1
  t1 <- Sys.time()
  
  conf_all <- conf_check2(plrs,trgt)
  
  #conf_all <- conf_all[[1]]
  
  ob5 <-  Sys.time()-t1
  t1 <- Sys.time()
  
  if(nrow(conf_all[[1]]) > 0 ) {

    
    conf_occ <- TRUE    # print(plrs)
    # print(trgt)
    # print(conf_all)
    
    #confout <- conflict(conf_all,plrs,trgt)
    
    # Modify strength based on conflict
    # print(conf_all[[3]])
    plrs[conf_all[[3]], on = c(id = 'player'),str := str-mod]
    
    # Work on targets
    
    trgt[conf_all[[4]], on = c(id = 'target'),str := str-mod]
    
    trgt[str < 0, str := 0]
    
    plrs[str < 0, str := 0]
    
  }else{
    conf_occ <- FALSE
  }
  ob6 <-  Sys.time()-t1

  print(c(ob1,ob2,ob3,ob4,ob5,ob6))
  print(sum(ob1,ob2,ob3,ob4,ob5,ob6))
  print(paste("total",(Sys.time()-t0)))
  print(paste('dif',(Sys.time()-t0) - sum(ob1,ob2,ob3,ob4,ob5,ob6)) )
  
  #print(paste("Total:", Sys.time()-t0))
  # df_t <- rbind(df_t, data.table(event = 'ex4_conflict',t = Sys.time()-t1))
  # df_t <- rbind(df_t, data.table(event = 'ex_all_wholetrans',t = Sys.time()-t0))
  # print(territory)
  return(list(plrs,trgt,conf_occ,conf_all,df_t,territory,changeval))
  
}
6.44+3.88+.445+.254+4.6+6.62


check_and_correct_moves_vec <- function(s, sp) {
  # Create a copy of sp to modify
  sp_new <- sp

  # Initialize a variable to track changes
  changes_made <- TRUE

  # Loop until no changes are made
  while(changes_made & any(duplicated(sp_new))) {
    changes_made <- FALSE
     # print(s)
     # print('sp below')
     # print(sp)
    # Get the indices and values where s == sp
    stationary_inds <- which(s == sp_new)
    stationary_values <- sp_new[stationary_inds]

    # Check if any sp equals those values
    moving_inds <- which(sp_new %in% stationary_values)

    # If so, set the new sp for that object back to s
    if(length(moving_inds) > 0) {
      sp_new[moving_inds] <- s[moving_inds]
      changes_made <- TRUE
    }
  }
  #print('yooo')
  ssp <- paste0(s,sp_new)
  sps <- paste0(sp_new,s)
  #print(sp_new)
  # Now check for the case where two objects attempt to switch places
  switch_inds <- which(ssp %in% sps & sps %in% ssp)
  sp_new[switch_inds] <- s[switch_inds]

  return(sp_new)
}






check_and_correct_moves <- function(dt) {
  # Convert to data.table if not already
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  
  # Set key for efficient joining
  setkey(dt, id, s, sp)
  
  # 1. Two objects attempt to switch places
  switch_rows <- dt[dt, on = .(s = sp, sp = s)]
  if (nrow(switch_rows) > 0) {
    dt[id %in% switch_rows$id, sp := s]
  }
  
  # 2. One object attempts to move into a stationary piece
  stationary_rows <- dt[dt, on = .(sp = s), nomatch = 0L]
  if (nrow(stationary_rows) > 0) {
    dt[id %in% stationary_rows$id, sp := s]
  }
  
  # 3. Two objects attempt to move into the same location
  same_sp_rows <- dt[, .N, by = sp][N > 1]
  if (nrow(same_sp_rows) > 0) {
    dt[sp %in% same_sp_rows$sp, sp := s]
  }
  
  return(dt)
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

library(data.table)

source('hex_setup.R')
source('hex_conflict.R')
source('hex_funcs.R')



execute_action <- function(state,actions,grad,c,
                           lasta,act_dt, probdf, depth,prob_b,
                           disc = 0.95,prob_lst,leg_lst,sing_lst,k_t,shrt_dist, ind_q_sel,leg_acts,lanc) {

  
  state <- state[order(id)]
  # print('state')
  # print(state)
  # print(lasta)
  # print(shrt_dist)
  t1 <- Sys.time()
  df_t <- data.table()
  sa_dt <- lasta[type == 'e',.(s)]
  sa_dt$a <- shrt_dist
  #print(sa_dt)
  
  eny_row <- nrow(lasta[type =='e'])
  eny_a <- lasta[type == 'e']
  lasta <- lasta[type == 'f']
  lasta <- lasta[order(id)]
  eny_a <- eny_a[order(id)]
  lasta$sid <- paste0(lasta$id,lasta$s)
  
  #### New select function ####
  
  t1 <- Sys.time()
  
  rand_select <- FALSE
  sel_num <- 0
  # print('lasta')
  # print(lasta)
  # print(depth)
  # print(eny_a)
  selecta <- rbindlist(apply(X = lasta[,.(id,s)], FUN = selfun3,
                             MARGIN = 1,lst = ind_q_sel))
  # print("select a!")
  # if(depth == 0) { 
  #   print('selectaaaaa')
  #   print(lasta)
  #   print(selecta)
  #   }
  # If (s,a) hasn't been explored, add large value to ensure selection
  selecta[,sum_qn := ifelse(n == 0, 1000, q + c*sqrt(log(sum(n))/n)), by = 's']
  # Make wide
  wide_dt <- dcast(selecta, id ~ sp, value.var = "sum_qn")
  # Replace NA values with large value to ensure non-selection
  wide_dt[is.na(wide_dt)] <- -100
  
  # Remove the 'id' column and convert the data.table to a matrix
  cost_matrix <- as.matrix(wide_dt[, -"id"])
  
  cost_matrix <- cost_matrix - 1000
  
  # Solve the assignment problem
  assignment <- solve_LSAP(-cost_matrix)
  # Create a final data.table based on the optimal assignment
  opt_move <- data.table(id = wide_dt$id, sp = colnames(cost_matrix)[assignment], qn_sum = cost_matrix[cbind(seq_len(nrow(cost_matrix)), assignment)])

  
  selecta <- selecta[opt_move, on = .(id, sp)]
  # print('second selecta!')
  # print(selecta)
  
  df_t <- rbind(df_t, data.table(event = 'select_fun',t = Sys.time()-t1))
  # if(depth == 0 ){
  #   print(selecta)
  #   
  #   print(wide_dt)
  #   
  #   print(cost_matrix)
  #   
  #   print(assignment)
  #   
  # }
  ### New enemy belief select ####
  
  t1 <- Sys.time()
  act_sel <- apply(sa_dt,FUN = belfun2,MARGIN = 1,plist = prob_lst,leg_list = leg_lst)
  df_t <- rbind(df_t, data.table(event = 'belief_fun',t = Sys.time()-t1))

  t1 <- Sys.time()
  s_act <- data.table(s = sa_dt$s, a = act_sel)
  s_act <- merge(s_act, leg_acts, by = c('s','a'), all.x = TRUE)
  #dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  df_t <- rbind(df_t, data.table(event = 'same_fun',t = Sys.time()-t1))
  
  # print('enemy sel')
  # print(s_act)
  
  actvec <- c(selecta$a,s_act$a)
  sp_sel <- c(selecta$sp, s_act$sp)
  ##### Move Select #####
  
  t1 <- Sys.time()
  
  move <- state
  # print('move')
  # print(move)
  setorder(move, -type, id)
  move$a <- actvec
  move$sp <- sp_sel
  # print('move')
  # print(move)

  #df_t <- rbind(df_t, data.table(event = 'move_select_2paste',t = Sys.time()-t1))
  # print(move)
  t2 <- Sys.time()
  
  trans <- transition_function2(move[type == 'f'],move[type == 'e'],k_t,lanc)
  
  df_t <- rbind(df_t, data.table(event = 'transition_func',t = Sys.time()-t2))
  df_t <- rbind(df_t,trans[[5]])
  
  grad_rew <- trans[[7]]
  #print(trans[[7]])
  rew <- reward_new(trans,grad_rew)
  # print(rew)
  df_t <- rbind(df_t, data.table(event = 'ex_new',t = Sys.time()-t1))
  typeout <- 'new'
  
  dtout <- df_t
  
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew,grad_rew,0,dtout,trans[[6]]))
  
}




q_update <- function(indv_q,ind_q_ls, gamma = 0.95,j) {
 

  indv_q_list <- split(indv_q, paste(indv_q$s, indv_q$id, sep = "."))
  
  names_ls <- names(ind_q_ls)
  
  ind_q_ls <- lapply(names(ind_q_ls), function(s) {
    sublist <- lapply(names(ind_q_ls[[s]]), function(id) {
      
      
      if (paste(s, id, sep = ".") %in% names(indv_q_list)) {
        a_use <- indv_q_list[[paste(s, id, sep = ".")]]$a
        q_use <- indv_q_list[[paste(s, id, sep = ".")]]$q_new
        
        for(i in 1:length(a_use)){
          ind_q_ls[[s]][[id]][a==a_use[i],
                              n := n + 1]
          ind_q_ls[[s]][[id]][a==a_use[i],
                              q := q + ((q_use[i] - q) / n)]
        }

      }
      ind_q_ls[[s]][[id]]
    })
    names(sublist) <- names(ind_q_ls[[s]])
    sublist
  })
  

  
  names(ind_q_ls) <- names_ls

  indv_out <- data.table(id = indv_q$id, val = indv_q$val_new)

  
  return(list(0,list(0,indv_out),0,0,ind_q_ls))
}


q_setup <- function(unit_obj, legal_a){
  # replicate the data.table object for each value in the vector
  replicated_dt <- lapply(unit_obj[type == 'f']$id, function(v) legal_a)
  
  # combine the replicated data.table objects into a new data.table object
  new_dt <- rbindlist(replicated_dt, idcol = "id")
  
  # add a column with the values of the vector
  new_dt[, id := unit_obj[type == 'f']$id[id]]
  
  new_dt$n <- 0
  new_dt$q <- 0
  
  new2 <- split(new_dt, by = 's')
  
  # Then, split each subset by 'id'
  
  ind_q_lst <- lapply(new2, function(x) split(x, x$id))
  
  return(ind_q_lst)
}


simulate_mcts <- function(unit_obj, ind_q_in, legal_a, terr_loc, q, c = 5,n_turns,
                          n_iter = 250, depth = 5, actions,k_terr, gamma = 0.95,lanc_df,turnval,
                          ind_f, ind_e, afil_val, opp_val){
  
  avg_u <- 0
  bigval <- 0
  i <- 0
  lst_log <- data.table()
  df_type <- data.frame()
  time_stamp <- Sys.time()
  actions_dt <- data.table(actions)
  prob_base <- prob_setup()
  df_log <- data.table()
  print(unit_obj)
  
  ### Set up list with individual q values ####
  
  
  ind_q_lst <- ind_q_in

  prob_ls <- split(prob_base, by = 'a')
  leg_ls <- split(data.table(legal_a), by = 's')
  
  
  all_states <- unique(legal_a$s)
  short_dist <- as.vector(sapply(all_states, FUN = find_move, target_id = k_terr$s))
  
  dist_dt <- data.table(s = all_states, a = short_dist) 
  
  
  while(i < n_iter ){
    # print(ind_f)
    # print(ind_e)
    units_new <- unit_obj
    sim_index <- ind_f
    sim_e_index <- ind_e
    
    key_terr <- k_terr
    
    j <- 0
    #print(i)
    lst_out <- data.table()
    
    if(i %% 100 == 0){
      print(paste("Simulation",i))
      print(paste('100 Iter Time:',Sys.time()-time_stamp))
      time_stamp <- Sys.time()
    }
# 
#     print(units_new)
#     print(turnval)
#     print(j)
#     print(depth)
    while(max(units_new[type == 'f']$str) > 10 & max(units_new[type == 'e']$str) > 10 & j+turnval <= n_turns){
      
      # print('yoooo')
      
      input_state <- data.table(id = units_new$id,s = units_new$s,str = units_new$str, 
                                type = units_new$type, class = units_new$class)
      
      time <- Sys.time()
      
      eny_dist <- left_join(input_state[type == 'e',.(s)],dist_dt, by = "s")$a
      
      out <- execute_action(state=input_state,actions = legal_a[s %in% input_state$s],grad = terr_loc,
                            c=c, lasta = input_state,act_dt = actions_dt,
                            probdf = single_out, depth = j,prob_b = prob_base, disc = gamma,
                            prob_lst = prob_ls, leg_lst = leg_ls, k_t = key_terr,
                            shrt_dist = eny_dist, ind_q_sel = ind_q_lst, leg_acts = legal_a,lanc = lanc_df)
      
      t1 <- Sys.time()
      
      df_log <- rbind(df_log,out[[5]])
      
      
      df_type <- bind_rows(df_type,data.frame(iter = i, depth = j, type = out[[4]]))
      
      lst_temp <- out[[2]][[2]]
      lst_temp$turn <- j
      # print(lst_out)
      # print(lst_temp)
      lst_out <- rbind(lst_out,lst_temp)

      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type), a = c(out[[1]]$a),
                              class = c(out[[1]]$class))
      
      old_len <- nrow(units_new)
      
      
      ### Remove lost units
      units_new <- units_new[str > 30]
      
      
      ### Add reinforcements
      # print('trns')
      # print(turnval)
      # print(j)
      turn <- turnval + j
      # print(turn)
      for(ik in 1:dim(rein_mat)[2]){
        
        if(rein_mat[turn,ik]>0){
          # print('here')
          # print(rein_mat[turn,ik])
          for(jk in 1:rein_mat[turn,ik]){
            
            new_u <- data.frame(class = rein_class[ik],str = 100)
            # print(key_tern)
            if(rein_type[ik] == 'f' & key_tern[s == '081210']$type == afil_val){
              
              if(length(f_rein_areas_fwd[!f_rein_areas_fwd %in% units_new$s]) > 0){
                
                new_u$s <- f_rein_areas_fwd[!f_rein_areas_fwd %in% units_new$s][1]
                new_u$id <- paste(new_u$class,'f',sim_index,sep = "_")
                new_u$type <- ifelse(afil_val == 'f','f','e')
                sim_index <- sim_index + 1
              } else {
                
                new_u$s <- territory$pos[!territory$pos %in% units_new$s][1]
                new_u$id <- paste(new_u$class,'f',sim_index,sep = "_")
                new_u$type <- ifelse(afil_val == 'f','f','e')
                sim_index <- sim_index + 1
              }
              
            } else if(rein_type[ik] == 'f' & key_tern[s == '081210']$type == opp_val){
              
              if(length(f_rein_areas_rear[!f_rein_areas_rear %in% units_new$s]) > 0 ){
                
                new_u$s <- f_rein_areas_rear[!f_rein_areas_rear %in% units_new$s][1]
                new_u$id <- paste(new_u$class,'f',sim_index,sep = "_")
                new_u$type <- ifelse(afil_val == 'f','f','e')
                sim_index <- sim_index + 1
                
              } else {
                
                new_u$s <- territory$pos[!territory$pos %in% units_new$s][1]
                new_u$id <- paste(new_u$class,'f',cur_index,sep = "_")
                new_u$type <- ifelse(afil_val == 'f','f','e')
                sim_index <- sim_index + 1
              }
              
            } else if(rein_type[ik] == 'e') {
              
              if(length(e_rein_areas[!e_rein_areas %in% units_new$s])>0){
                new_u$s <- e_rein_areas[!e_rein_areas %in% units_new$s][1]
                
                new_u$id <- paste(new_u$class,'e',sim_e_index,sep = "_")
                new_u$type <- ifelse(afil_val == 'f','e','f')
                sim_e_index <- sim_e_index + 1
              } else {
                
                new_u$s <- rev(territory$pos[!territory$pos %in% units_new$s][1])
                
                new_u$id <- paste(new_u$class,'e',sim_e_index,sep = "_")
                new_u$type <- ifelse(afil_val == 'f','e','f')
                sim_e_index <- sim_e_index + 1
              }
              
            }
             # print(new_u)
            # new_q_lst <- q_setup(as.data.table(new_u), as.data.table(legal_a))
            # # print((ind_q_lst$`131104`))
            # # print('to add')
            # # print((new_q_lst$`131104`))
            # 
            # player_data <- lapply(names(ind_q_lst), function(state) {
            #   c(ind_q_lst[[state]], new_q_lst[[state]])
            # })
            # 
            # names(player_data) <- names(ind_q_lst)
            # 
            # ind_q_lst <- player_data
            
            #ind_q_lst <- c(ind_q_lst, new_q_lst)
            #print('second')
            #print((ind_q_lst$`131104`))
            new_u$a <- 'adj0'
            units_new <- rbind(units_new,new_u)
            
          }
          
        }
      }
      
      
      ### Key terrain
      key_terr <- out[[6]]

      units_new[, a:= NULL]
      
      j <- j + 1
      
      df_log <- rbind(df_log, data.table(event = 'cleanup',t = Sys.time()-t1))
      
    }
    
    lst_out1 <- copy(lst_out)
    lst_out1$sim <- i
    lst_log <- rbind(lst_log,lst_out1)
    
    setorder(lst_out, id, turn)
    
    lst_out[, val := val * (gamma ^ turn)]
    lst_out[, q_new := rev(Reduce(function(x, y) gamma * x + y, rev(val), accumulate = TRUE)), by = id]
    
    #print(lst_out)
    t1 <- Sys.time()
    # q_temp <- q_update(lst_out,ind_q_lst, gamma = 0.95, j = k)
    
    # for(k in 1:nrow(lst_out)){
    #   #print(ind_q_lst[[lst_out[[i,2]]]][[lst_out[[i,1]]]][a == lst_out[[i,3]]])
    #   ind_q_lst[[lst_out[[k,2]]]][[lst_out[[k,1]]]][a == lst_out[[k,3]],n := n + 1]
    #   ind_q_lst[[lst_out[[k,2]]]][[lst_out[[k,1]]]][a == lst_out[[k,3]],q := q + ((lst_out[[k,6]] - q) / n)]
    # }
    
    
    
    
    for(k in 1:nrow(lst_out)){
      dt <- ind_q_lst[[lst_out[[k,2]]]][[lst_out[[k,1]]]]
      
      # if(k == 1 ){
      #   print('heeeere')
      #   print(lst_out)
      #   print(dt)
      # }
      rows_to_update <- dt$a %in% lst_out[[k,3]]
      
      # update n
      set(dt, i = which(rows_to_update), j = "n", value = dt[rows_to_update, n] + 1)
      
      # update q
      set(dt, i = which(rows_to_update), j = "q", value = dt[rows_to_update, q] + ((lst_out[[k,6]] - dt[rows_to_update, q]) / dt[rows_to_update, n]))
    }
    
    
    # ind_q_lst <- q_temp[[5]]
    
    df_log <- rbind(df_log, data.table(event = 'q_update',t = Sys.time()-t1))

    i <- i + 1
  }

  total_time <- Sys.time() - time_stamp
  return(list(lst_log,df_type,df_log,total_time,ind_q_lst))
}


vec_add <- function(vector, add_vector){
  return(vector + add_vector)
}

belfun <- function(df,plist){
  sample(plist[[df]]$nexta,1,prob = plist[[df]]$p)
}

belfun2 <- function(df,plist,leg_list){
  probtest <- plist[[df[[2]]]][nexta %in% leg_list[[df[[1]]]][[2]]]
  
  sample(probtest[[2]],1,prob = probtest[[3]])
}

samefun <- function(df,olist){
  return(olist[[df[[1]]]][a == df[[2]]]$sp)
}

selfun <- function(state,prob,legal_a,rand){
  
  if(is.null(prob[[state[[6]]]])==TRUE | sum(prob[[state[[6]]]]$q) == 0 | rand == TRUE){
    
    sample(legal_a[[state[[2]]]]$a,1)
    
  } else{
    
    sample(prob[[state[[6]]]]$a,1,replace = TRUE,prob =prob[[state[[6]]]]$n )
    
  }
}



## Selection function for naive MCTS ####

selfun2 <- function(dt,lst, c, rand_sel = FALSE) {
  
  num <- nrow(lst[[dt[[2]]]][[dt[[1]]]])
  
  out <- lst[[dt[[2]]]][[dt[[1]]]][sample(num, 1), ]
  
  
  if(out$n == 0 | rand_sel == TRUE ){
    out_f <- out
  } else {
    out <- lst[[dt[[2]]]][[dt[[1]]]][n>0]
    print(out)
    ucb <- out$q + c*sqrt(log(sum(out$n))/out$n)
    #print(ucb)
    out_f <- out[which.max(ucb)]
    print(out_f)
  }
  
  return(out_f)
}

selfun3 <- function(dt,lst) {
  
  num <- lst[[dt[[2]]]][[dt[[1]]]]
  num
}


### Find the shortest path ####


find_move <- function(current_id, target_id) {
  add_or_subtract <- function(value, change) {
    return(sprintf("%02d", as.integer(value) + change))
  }
  
  apply_move <- function(id, move) {
    c1 <- substr(id, 1, 2)
    c2 <- substr(id, 3, 4)
    c3 <- substr(id, 5, 6)
    
    if (move == 'adj2') {
      return(paste0(add_or_subtract(c1, 1), add_or_subtract(c2, 1), c3))
    } else if (move == 'adj3') {
      return(paste0(c1, add_or_subtract(c2, 1), add_or_subtract(c3, 1)))
    } else if (move == 'adj4') {
      return(paste0(add_or_subtract(c1, -1), c2, add_or_subtract(c3, 1)))
    } else if (move == 'adj5') {
      return(paste0(add_or_subtract(c1, -1), add_or_subtract(c2, -1), c3))
    } else if (move == 'adj6') {
      return(paste0(c1, add_or_subtract(c2, -1), add_or_subtract(c3, -1)))
    } else if (move == 'adj1') {
      return(paste0(add_or_subtract(c1, 1), c2, add_or_subtract(c3, -1)))
    } else {
      return(NULL)
    }
  }
  
  find_shortest_distance <- function(start_id, end_id) {
    moves <- c('adj2', 'adj3', 'adj4', 'adj5', 'adj6', 'adj1')
    min_distance <- Inf
    
    best_move <- ""
    
    for(end in end_id){
      
      for (move in moves) {
        next_id <- apply_move(start_id, move)
        distance <- abs(as.integer(substr(next_id, 1, 2)) - as.integer(substr(end, 1, 2))) +
          abs(as.integer(substr(next_id, 3, 4)) - as.integer(substr(end, 3, 4))) +
          abs(as.integer(substr(next_id, 5, 6)) - as.integer(substr(end, 5, 6)))
        
        if (distance < min_distance) {
          min_distance <- distance
          best_move <- move
        }
      }
    }
    
    
    if(start_id %in% end_id){
      best_move <- 'adj0'
    }
    return(best_move)
  }
  
  return(find_shortest_distance(current_id, target_id))
}

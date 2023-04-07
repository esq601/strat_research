library(data.table)
source('hex_setup.R')
source('hex_conflict.R')
source('hex_funcs.R')


matchfun <- function(x,new) {
  identical(new, x)
}

yes_fun <- function(vector){
  rbeta(n = 1, shape1 = vector[[1]], shape2 = vector[[2]])
}


dir_setup <- function(dataframe,act_dt,leg_a ) {
  # print(leg_a)
  # print(act_dt)
  # print('now')
  #print(dataframe)
  state <- unique(dataframe$s)
  # print(state)
  new_dt <- data.table(dataframe)[act_dt, on = c('a' ='actions')]
  new_dt[,q:=q+1]
  new_dt[is.na(q),q:= 1]
  out <- new_dt[a %in% leg_a[s == state]$a]
  return(out)
}

dir_setup

select_fun <- function(id,state,legal_moves, prob_df,act_dt){
  #print(act_dt)
  #print(state)
  idstate <- paste0(id,state)
  #dfmove <- prob_df[a == action & nexta %in% legal_moves[s == state]$a]
  #print(prob_df)
  #sample(dfmove$nexta, size =1 , prob = dfmove$p)
  val <- which(idstate==prob_df$s.s)
  #print(val)
  if(length(val) > 0){
    out <-dir_setup(prob_df[val,]$data[[1]],act_dt,legal_moves)
    
    valout <- max.col(rdirichlet(n = 1, alpha = out$q))
    #print(valout)
    aout <- out[[valout,1]]
  } else {
    #print(legal_moves)
    aout <- sample(legal_moves[s == state]$a,1)
    #print(aout)
  }

  #print(aout)
  #break
  
}


#outtest <- ptest[a == 'adj5' & nexta %in% legal_acts[s == '030407']$a]
#sample(outtest$nexta,1,prob = outtest$p)

belief_fun <- function(action,state,legal_moves, prob_df,act_dt){
  #print(prob_df)
  out <- prob_df[a == action & nexta %in% legal_moves[s == state]$a]
  
  
  #valout <- max.col(rdirichlet(n = 1, alpha = out$q))
  #print(valout)
  aout <-sample(out$nexta,1,prob = out$p)

  return(aout)
  
}



execute_action <- function(state,actions,grad,q_lst,c, lasta,act_dt, probdf, depth,prob_b, disc = 0.95) {
  
  # Only current states are subsetted
  #print(lasta)
  t1 <- Sys.time()
  sa_dt <- lasta[type == 'e',.(s,a)]
  
  
  move <- actions[state[,a := NULL], on = .(s)]
  #print(move)
  actvec <- vector()
  eny_row <- nrow(lasta[type =='e'])
  eny_a <- lasta[type == 'e']
  lasta <- lasta[type == 'f']
  #print(eny_a)
  for(i in 1:nrow(lasta)){
    act_select <- select_fun(id = lasta[[i,1]],state = lasta[[i,2]],actions,probdf,act_dt)
    
    actvec <- c(actvec,act_select)
  }
  
  for(i in 1:nrow(sa_dt)){
    #print(eny_a)
    act_select <- belief_fun(action = sa_dt[[i,2]],state = sa_dt[[i,1]],actions,prob_b,act_dt)
    #print(act_select)
    actvec <- c(actvec,act_select)
  }
  
  td1 <- Sys.time() - t1
  t2 <- Sys.time()
  
  movesel <- data.table(s = state$s, a = actvec, id = state$id)
  #print(movesel)
  
  move <- move[movesel, on = .(s,a,id)]
  s_vec <- paste0(t(move[,list(id,s,str,type)]),collapse = '')
  sa_vec <- paste0(paste0(t(move[,list(id,s,str,type)]),collapse = ''),
                   paste0(t(move[type=='f',list(a)]),collapse = ''),
                   collapse = '')
  
  
  ### If a in (s,a) select max UCB
  # matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(move[,list(id,s,str,type)]),
  #                                                    t(move[type=='f',list(a)]))))
  # print(sa_vec)
  matches <- q_lst$sa$sa %in% sa_vec
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  

  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) > 0) {
    
    matches_s <- q_lst$s$s %in% s_vec
    
    matches_s <- which(matches_s)

    ucb <- ((c*(disc^depth))*sqrt(log(sum(unlist(q_lst$n[matches_s])))/unlist(q_lst$n[matches_s])))
    
    qsa <- unlist(q_lst$q[matches_s])
    # if(depth ==0){
    # 
    #   cat(c(unlist(q_lst$a[matches_s]),sum(unlist(q_lst$n[matches_s])),
    #   unlist(q_lst$n[matches_s]),'ucb:',ucb,'qsa:',qsa),'\r')
    # }

    ucb <- ucb + qsa
    
    movenew <- data.table(s = move[type == 'f']$s,str = move[type == 'f']$str,type = 'f',
                          id = move[type=='f']$id,
                           rand = move[type =='f']$rand)
    movenew$a <- q_lst$a[matches_s[[which.max(ucb)]]]
    
    if(which.max(ucb) == which.max(qsa)){
      
      type_act <- 'exploit'
    } else {
      
      type_act <- 'explore'
    }
    
    movenew <- actions[movenew, on = .(s,a)]
    
    trans <- transition_function(movenew,move[type == 'e'])
    
    #grad_rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    
    #rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad,grad_rew)
    
    td2 <- Sys.time() - t2
    typeout <- 'update'
    
  }else{
    
    type_act <- 'new'
    #print(move)
    trans <- transition_function(move[type == 'f'],move[type == 'e'])
    # print(trans)
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad,grad_rew)
    td2 <- Sys.time() - t2
    typeout <- 'new'
  }
  
  dtout <- data.table(init = td1, ifs = td2, type = typeout)
  
  #print(list('he',trans[[1]],trans[[2]]))
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew,grad_rew,type_act,dtout))
  
}


q_update <- function(q_lst, transition, gamma = 0.95,j) {
  
  trigger <- F
  
  transition[[1]][order(id)]
  #print(transition)
  s_old_vec <- paste0(t(transition[[1]][,list(id,s,str_old,type)]),collapse = '')
  
  s_vec <- paste0(t(transition[[1]][,list(id,sp,str,type)]),collapse = '')
  
  sa_vec <- paste0(paste0(t(transition[[1]][,list(id,s,str_old,type)]),collapse = ''),
                   paste0(t(transition[[1]][type=='f',list(a)]),collapse = ''),
                   collapse = '')
  
  # matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
  #                                                        t(transition[[1]][type=='f',list(a)]))))
  matches <- q_lst$sa$sa %in% sa_vec
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  # print(outnew)
  # print(transition[[4]])
  val <- transition[[2]]
  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) == 0) {
    
    q_lst$s <- rbind(q_lst$s,data.table(s = s_old_vec))
    q_lst$a <- append(q_lst$a,list(as.vector(t(transition[[1]][type=='f',list(a)]))))
    q_lst$sa <- rbind(q_lst$sa,data.table(sa = sa_vec))
    q_lst$q <- append(q_lst$q,list(val))
    q_lst$n <- append(q_lst$n,list(1))
    q_lst$grad_rew <- append(q_lst$grad_rew,transition[[2]])
    
  } 
  ### Else, add the reward to U(s') and increment N(s,a)
  
  matches_s <- q_lst$s$s %in% s_vec
  #print(matches_s)
  next_state <- which(matches_s, arr.ind = FALSE)
  
  # If next state does not exist
  if(length(next_state) > 0){
    
    # matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
    #                                                        t(transition[[1]][type=='f',list(a)]))))
    #print(next_state)
    
    #print(length(q_lst$n))
    matches <-  q_lst$sa$sa %in% sa_vec
    outnew <- which(matches, arr.ind = FALSE)
    #print(outnew)
    u_ind <- utility_func(q_lst, s_vec)
    
    q_lst$n[outnew] <- list(q_lst$n[[outnew]]+1)
    if(u_ind > 25){
      print(u_ind)
      print(val)
    }
    val <- (val + gamma^j * u_ind)
    if(u_ind > 25){
      print(val)
    }
    
    q_lst$q[outnew] <- list(q_lst$q[[outnew]] + (val - q_lst$q[[outnew]])/q_lst$n[[outnew]])
    
  } 
  
  return(list(q_lst,val,outnew,trigger))
}

simulate_mcts <- function(unit_obj,last_a, legal_a, terr_loc, q, c = 5,
                          n_iter = 250, depth = 5, single_out,actions){
  
  avg_u <- 0
  bigval <- 0
  df_type <- data.frame()
  time_stamp <- Sys.time()
  actions_dt <- data.table(actions)
  prob_base <- prob_setup()
  df_log <- data.table()
  print(unit_obj)
  for(i in 1:n_iter){
    
    acts_new <- last_a
    
    units_new <- unit_obj
    
    j <- 0
    
    lst_out <- list()
    
    if(i %% 100 == 0){
      print(paste("Simulation",i))
      print(paste('100 Iter Time:',Sys.time()-time_stamp))
      time_stamp <- Sys.time()
    }
    
    while(max(units_new[type == 'f']$str) > 10 & max(units_new[type == 'e']$str) > 10 & j < depth){
      
      #if(j == 0 ){
        input_state <- data.table(id = units_new$id,s = units_new$s,str = units_new$str, 
                                  type = units_new$type, a = acts_new)
        
      #} else {
        
        #input_state <- data.table(s = units_new$s, a = acts_new,id = units_new$id)
        
      #}
      
      #print(input_state)
      time <- Sys.time()
      
      #print(input_state)
      #print(units_new)
      out <- execute_action(state=input_state,actions = legal_a,grad = terr_loc,
                            q_lst=q,c=c, lasta = input_state,act_dt = actions_dt,
                            probdf = single_out, depth = j,prob_b = prob_base, disc = 0.95)
      #print(out[[1]])
      df_log <- rbind(df_log,out[[5]])
      #print(c("Time Execute", Sys.time()-time))
      
      df_type <- bind_rows(df_type,data.frame(iter = i, depth = j, type = out[[4]]))
      
      lst_out <- append(lst_out,list(out))
      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type), a = c(out[[1]]$a))
      
      # if(min(units_new$str) < 10){
      #   print('One down!')
      #   print(units_new)
      #   #print('yoo')
      #   #print(acts_new[which(units_new$str > 10)])
      #   
      # }
      # 
      # if(max(units_new$str) < 100){
      #   print('all fightin!')
      #   print(units_new)
      #   #print('yoo')
      #   #print(acts_new[which(units_new$str > 10)])
      #   
      # }
      
      old_len <- nrow(units_new)
      units_new <- units_new[str > 10]
      
      
      new_len <- nrow(units_new)

      acts_new <- acts_new[which(units_new$str > 10)]


      units_new[, a:= NULL]
      
      j <- j + 1
      
      
      
    }
    
    
    
    for(k in length(lst_out):1){
      
      q_temp <- q_update(q,lst_out[[k]], gamma = 0.95, j = k)
      
      q <- q_temp[[1]]
      
    }
    
    val <- unlist(q_temp[[2]])
    val[is.null(val)] <- 0
    
    
  }
  #print(unit_obj)
  s_vec <- paste0(t(unit_obj[,list(id,s,str,type)]),collapse = '')
  
  state_in <- q$s$s %in% s_vec
  #print(q$s$s[1:100])
  #state_in <- sapply(q$s,matchfun, new = as.vector(t(unit_obj[,list(id,s,str,type)])))
  
  
  
  lst_new <- data.table(q = unlist(q$q[which(state_in)]), a = (q$a[which(state_in)]))
  lst_new[order(-q)]
  
  return(list(q,lst_new,df_type,df_log))
}


vec_add <- function(vector, add_vector){
  return(vector + add_vector)
}


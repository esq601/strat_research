library(foreach)
library(doParallel)
library(patchwork)

cores=detectCores()
cl <- makeCluster(cores[1]/2) #not to overload your computer
registerDoParallel(cl)
cores

execute_one_action <- function(state,actions,grad,q_lst,c,
                           lasta,act_dt, probdf, depth,prob_b,
                           disc = 0.95,prob_lst,leg_lst) {
  
  # Only current states are subsetted
  
  t1 <- Sys.time()
  df_t <- data.table()
  sa_dt <- lasta[type == 'e',.(s,a)]
  
  eny_row <- nrow(lasta[type =='e'])
  eny_a <- lasta[type == 'e']
  lasta <- lasta[type == 'f']
  lasta$sid <- paste0(lasta$id,lasta$s)
  
  #### New select function ####
  
  t1 <- Sys.time()
  
  
  actvec_f <- apply(lasta,FUN = selfun,MARGIN = 1,
                    prob = NULL, legal_a = leg_lst, rand = TRUE)
  
  s_act <- data.table(s = lasta$s, a = actvec_f)
  
  sp_sel <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  
  # s_act <- data.table(s = lasta$s, a = actvec)
  # dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  
  act_sel <- unlist(lapply(sa_dt$a,belfun, plist = prob_lst))
  
  s_act <- data.table(s = sa_dt$s, a = act_sel)
  dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  
  while(class(dtout) == 'list' | any(duplicated(dtout)) == TRUE){
    #print('working!')
    act_sel <- unlist(lapply(sa_dt$a,belfun, plist = prob_lst))
    
    s_act <- data.table(s = sa_dt$s, a = act_sel)
    dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  }
  
  actvec <- c(actvec_f,act_sel)
  sp_sel <- c(sp_sel, dtout)
  
  
  
  # esel <- state[type == 'e', .(s,id)]
  # esel$a <- 'adj0'
  
  move <- state
  move$a <- actvec
  move$sp <- sp_sel
  
  s_vec <- paste0(lasta$id,lasta$s,collapse = '')
  sa_vec <- paste0(lasta$id,lasta$s,actvec_f,collapse = '')
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
    # print(matches_s)
    ucb <- ((c*(disc^depth))*sqrt(log(sum(unlist(q_lst$n[matches_s])))/unlist(q_lst$n[matches_s])))
    
    qsa <- unlist(q_lst$q[matches_s])
    
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
    
    trans <- transition_function2(movenew,move[type == 'e'])
    
    # grad_rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    # 
    # rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad_rew, mode = 'ind')
    
    typeout <- 'update'
    
  }else{
    
    type_act <- 'new'
    
    trans <- transition_function2(move[type == 'f'],move[type == 'e'])
    # print(trans)
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad_rew, mode = 'ind')
    
    typeout <- 'new'
  }
  
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew,grad_rew,type_act))
  
}


q_one_update <- function(q_lst, transition, gamma = 0.95,j) {
  
  trigger <- F
  
  #transition[[1]][order(id)]
  #print(transition)
  friendlies <- transition[[1]][type == 'f']
  
  s_old_vec <- paste0(friendlies$id,friendlies$s,collapse = '')
  
  s_vec <- paste0(friendlies$id,friendlies$sp,collapse = '')
  
  sa_vec <- paste0(friendlies$id,friendlies$s,friendlies$a, collapse = '')
  
  # matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
  #                                                        t(transition[[1]][type=='f',list(a)]))))
  matches <- q_lst$sa$sa %in% sa_vec
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  # print(outnew)
  # print(transition[[4]])
  val <- transition[[2]][[1]]
  #print(val)
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
    
    val <- (val + gamma * u_ind)
    
    q_lst$q[outnew] <- list(q_lst$q[[outnew]] + (val - q_lst$q[[outnew]])/q_lst$n[[outnew]])
    
  } 
  
  return(list(q_lst,val,outnew,trigger))
}



simulate_one_mcts <- function(unit_obj,last_a, legal_a,terr_loc,actions, c = 5, n_iter = 250, depth = 5){
  
  avg_u <- 0
  bigval <- 0
  df_type <- data.frame()
  actdt <- data.table(actions)
  prob_tran <- prob_setup()
  time_stamp <- Sys.time()
  
prob_tran
  prob_ls <- split(prob_tran, by = 'a')
  leg_ls <- split(data.table(legal_a), by = 's')
  
  q <- list(s = data.table(s = unit_obj[type == 'f']$s), 
            a = list('adj0'),
            sa = data.table(sa = paste0(unit_obj[type == 'f']$s,'adj0',collapse = '')), 
            q = list(0), n =list(1), grad_rew = 0)
  
  df_log <- data.table()
  
  for(i in 1:n_iter){
    units_new <- unit_obj
    acts_new <- last_a
    
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
      
      
      time <- Sys.time()
      out <- execute_one_action(state=input_state,actions = legal_a,grad = terr_loc,
                                q_lst=q,c=c,act_dt=actdt, lasta = input_state,
                                probdf = prob_tran, depth = j, disc = 0.95,
                                prob_lst = prob_ls, leg_lst = leg_ls)
      
      #print(c("Time Execute", Sys.time()-time))
      
      df_type <- bind_rows(df_type,data.frame(iter = i, depth = j, type = out[[4]]))
      
      lst_out <- append(lst_out,list(out))
      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type), a = c(out[[1]]$a))
      
      old_len <- nrow(units_new)
      units_new <- units_new[str > 10]
      new_len <- nrow(units_new)
      
      acts_new <- acts_new[which(units_new$str > 10)]
      
      units_new[, a:= NULL]
      
      j <- j + 1
      
      
      
    }
    
    for(k in length(lst_out):1){
      #print(lst_out[[k]])
      q_temp <- q_one_update(q,lst_out[[k]], gamma = 0.95, j = k)
      
      q <- q_temp[[1]]
      
    }
    
  }
  #lst_new[order(-q)]
  
  out <- data.table(id = unit_obj[type == 'f']$id,
                    s = q$s , a = unlist(q$a), q = unlist(q$q))
  
  return(out)
}


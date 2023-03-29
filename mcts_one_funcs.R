library(foreach)
library(doParallel)
library(patchwork)

cores=detectCores()
cl <- makeCluster(cores[1]/2) #not to overload your computer
registerDoParallel(cl)
cores

execute_one_action <- function(state,actions,grad,q_lst,c, lasta, probdf, depth, disc = 0.95) {
  
  # Only current states are subsetted
  
  move <- actions[state, on = .(s)]
  
  actvec <- sample(move[type == 'f']$a,1)
  
  esel <- state[type == 'e', .(s,id)]
  esel$a <- 'adj0'
  
  movesel <- data.table(s = state[type == 'f']$s, a = actvec,
                        id = state[type == 'f']$id)
  
  movesel <- rbind(movesel,esel)
  
  move <- move[movesel, on = .(s,a,id)]
  
  #move <- rbind(move, )
  
  
  s_vec <- paste0(t(move[type == 'f',list(id,s)]),collapse = '')
  sa_vec <- paste0(paste0(t(move[type == 'f',list(id,s)]),collapse = ''),
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
    
    trans <- transition_function(movenew,move[type == 'e'])
    
    grad_rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    
    rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    
    typeout <- 'update'
    
  }else{
    
    type_act <- 'new'
    
    trans <- transition_function(move[type == 'f'],move[type == 'e'])
    # print(trans)
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad,grad_rew)
    typeout <- 'new'
  }
  
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew,grad_rew,type_act))
  
}


q_one_update <- function(q_lst, transition, gamma = 0.95,j) {
  
  trigger <- F
  
  transition[[1]][order(id)]
  #print(transition)
  s_old_vec <- paste0(t(transition[[1]][type == 'f',list(id,s)]),collapse = '')
  
  s_vec <- paste0(t(transition[[1]][type == 'f',list(id,sp)]),collapse = '')
  
  sa_vec <- paste0(paste0(t(transition[[1]][type == 'f',list(id,s)]),collapse = ''),
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
    
    val <- (val + gamma^j * u_ind)
    
    q_lst$q[outnew] <- list(val)
    
  } 
  
  return(list(q_lst,val,outnew,trigger))
}



simulate_one_mcts <- function(unit_obj,last_a, legal_a, terr_loc, c = 5, n_iter = 250, depth = 5){
  
  avg_u <- 0
  bigval <- 0
  df_type <- data.frame()
  prob_tran <- prob_setup()
  time_stamp <- Sys.time()
  
  q <- list(s = data.table(s = paste0(t(unit_obj),collapse = '')), 
            a = list(rep('adj0',nrow(unit_obj[type=='f']))),
            sa = data.table(sa = paste0(paste0(t(unit_obj),collapse=''),
                                        paste0(rep('adj0',nrow(unit_obj[type=='f'])),collapse = ''),collapse = '')), 
            q = list(0), n =list(1), grad_rew = 0)
  
  df_log <- data.table()
  for(i in 1:n_iter){
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
      input_state <- data.table(s = units_new$s, a = last_a,id = units_new$id)
      
      #} else {
      
      #input_state <- data.table(s = units_new$s, a = acts_new,id = units_new$id)
      
      #}
      
      
      time <- Sys.time()
      out <- execute_one_action(state=units_new,actions = legal_a,grad = terr_loc,
                                q_lst=q,c=c, lasta = input_state,
                                probdf = prob_tran, depth = j, disc = 0.95)
      
      #print(c("Time Execute", Sys.time()-time))
      
      df_type <- bind_rows(df_type,data.frame(iter = i, depth = j, type = out[[4]]))
      
      lst_out <- append(lst_out,list(out))
      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type), a = c(out[[1]]$a))
      
      old_len <- nrow(units_new)
      units_new <- units_new[str > 10]
      new_len <- nrow(units_new)
      
      acts_new <- units_new$a
      
      units_new[, a:= NULL]
      
      j <- j + 1
      
      
      
    }
    
    for(k in length(lst_out):1){
      
      q_temp <- q_one_update(q,lst_out[[k]], gamma = 0.95, j = k)
      
      q <- q_temp[[1]]
      
    }
    
  }
  #lst_new[order(-q)]
  
  out <- data.table(id = unit_obj[type == 'f']$id,
                    s = q$s , a = unlist(q$a), q = unlist(q$q))
  
  return(out)
}

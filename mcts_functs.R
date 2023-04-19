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

select_fun <- function(id,state,legal_moves, prob_df,act_dt,ns){
  #print(act_dt)
  #print(state)
  idstate <- paste0(id,state)
  #dfmove <- prob_df[a == action & nexta %in% legal_moves[s == state]$a]
  #print(prob_df)
  #sample(dfmove$nexta, size =1 , prob = dfmove$p)
  #print(ns)
  #print(legal_moves)
  if (length(ns) > 1) {
   legal_moves <-  legal_moves[!(sp %in% ns)]
  }
  #print(nrow(legal_moves))
  val <- which(idstate==prob_df$s.s)
  #print(val)
  if(length(val) > 0){
    out <-dir_setup(prob_df[val,]$data[[1]],act_dt,legal_moves)
    #print(out)
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
  return(aout)
}


#outtest <- ptest[a == 'adj5' & nexta %in% legal_acts[s == '030407']$a]
#sample(outtest$nexta,1,prob = outtest$p)

belief_fun <- function(action,state,legal_moves, prob_df,act_dt,ns){
  #print(prob_df)
  
  if (length(ns) > 1) {
    legal_moves <-  legal_moves[!(sp %in% ns)]
  }
  
  out <- prob_df[a == action & nexta %in% legal_moves[s == state]$a]
  
  
  #valout <- max.col(rdirichlet(n = 1, alpha = out$q))
  #print(valout)
  aout <-sample(out$nexta,1,prob = out$p)

  return(aout)
  
}



execute_action <- function(state,actions,grad,q_lst,c,
                           lasta,act_dt, probdf, depth,prob_b,
                           disc = 0.95,prob_lst,leg_lst,sing_lst) {
  
  # Only current states are subsetted
  #print(lasta)
  t1 <- Sys.time()
  df_t <- data.table()
  sa_dt <- lasta[type == 'e',.(s,a)]
  
  # print(actions)
  # print(state)
  #move <- actions[state[,a := NULL], on = .(s)]
  # print(move)
  #actvec <- vector()
  eny_row <- nrow(lasta[type =='e'])
  eny_a <- lasta[type == 'e']
  lasta <- lasta[type == 'f']
  lasta$sid <- paste0(lasta$id,lasta$s)
  #print(eny_a)
  next_state <- vector()
  
  #while(max(table(next_state)) < )
  #act_use <- actions[s %in% lasta$s]
  
  
  #### New select function ####
  #print(sing_lst)
  t1 <- Sys.time()
  
  #print(paste0(lasta$id,lasta$s))
  #print(lasta)
  
  actvec <- apply(lasta,FUN = selfun,MARGIN = 1,
                           prob = sing_lst, legal_a = leg_lst, rand = FALSE)
  
  s_act <- data.table(s = lasta$s, a = actvec)
  dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  
  while(class(dtout) == 'list' | any(duplicated(dtout)) == TRUE){
    #print('working!')
    
    actvec <- apply(lasta,FUN = selfun,MARGIN = 1,
                    prob = sing_lst, legal_a = leg_lst, rand = FALSE)
    
    s_act <- data.table(s = lasta$s, a = actvec)
    dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  }
   
  df_t <- rbind(df_t, data.table(event = 'select_fun',t = Sys.time()-t1))
  ### Old Select function ####
  
  # for(i in 1:nrow(lasta)){
  #   
  #   t1 <- Sys.time()
  #   
  #   act_select <- select_fun(id = lasta[[i,1]],state = lasta[[i,2]],actions[!(sp %in% next_state)],probdf,act_dt,next_state)
  #   
  #   next_state <- c(next_state,actions[s == lasta[[i,2]] & a == act_select]$sp)
  #   # print(lasta[[i,2]])
  #   # print(next_state)
  #   df_t <- rbind(df_t, data.table(event = 'select_fun',t = Sys.time()-t1))
  #   actvec <- c(actvec,act_select)
  # }
  
  #next_state <- vector()
  
  ### New enemy belief select ####
  
  #print(sa_dt)
  t1 <- Sys.time()
  act_sel <- unlist(lapply(sa_dt$a,belfun, plist = prob_lst))
  
  s_act <- data.table(s = sa_dt$s, a = act_sel)
  dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  
  while(class(dtout) == 'list' | any(duplicated(dtout)) == TRUE){
    #print('working!')
    act_sel <- unlist(lapply(sa_dt$a,belfun, plist = prob_lst))
    
    s_act <- data.table(s = sa_dt$s, a = act_sel)
    dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  }
  df_t <- rbind(df_t, data.table(event = 'belief_fun',t = Sys.time()-t1))
  #print(dtout)
  
  actvec <- c(actvec,act_sel)
  
  #print(actvec)
  #### Old enemy belief select####
  
  # for(i in 1:nrow(sa_dt)){
  #   #print(eny_a)
  #   t1 <- Sys.time()
  #   
  #   act_select <- belief_fun(action = sa_dt[[i,2]],state = sa_dt[[i,1]],actions[!(sp %in% next_state)],prob_b,act_dt,next_state)
  #   #print(act_select)
  #   
  #   next_state <- c(next_state,actions[s == sa_dt[[i,1]] & a == act_select]$sp)
  #   
  #   actvec <- c(actvec,act_select)
  #   
  #   df_t <- rbind(df_t, data.table(event = 'belief_fun',t = Sys.time()-t1))
  # }

  ##### Move Select #####
  t1 <- Sys.time()
  
  move <- data.table(s = state$s, a = actvec, id = state$id,type = state$type,str = state$str)
  move$sp <- apply(move,FUN = samefun,MARGIN = 1,olist = leg_lst)
  # print(movesel)
  # print(move)
  # move <- move[movesel, on = .(s,a,id)]
  s_vec <- paste0(t(move[type=='f',list(id,s)]),collapse = '')
  sa_vec <- paste0(paste0(t(move[type == 'f',list(id,s)]),collapse = ''),
                   paste0(t(move[type=='f',list(a)]),collapse = ''),
                   collapse = '')
  
  
  ### If a in (s,a) select max UCB
  # matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(move[,list(id,s,str,type)]),
  #                                                    t(move[type=='f',list(a)]))))
  # print(sa_vec)
  matches <- q_lst$sa$sa %in% sa_vec
  #print(sa_vec)
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  
  df_t <- rbind(df_t, data.table(event = 'move_select',t = Sys.time()-t1))
  
  t1 <- Sys.time()
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) > 0) {
    

    matches_s <- q_lst$s$s %in% s_vec
    
    matches_s <- which(matches_s)

    ucb <- ((c*(disc^depth))*sqrt(log(sum(unlist(q_lst$n[matches_s])))/unlist(q_lst$n[matches_s])))
    
    qsa <- unlist(q_lst$q[matches_s])
# 
    # if(depth ==0){
    # 
    #   cat(c(sum(unlist(q_lst$n[matches_s])),
    #   unlist(q_lst$n[matches_s]),'ucb:',round(ucb,digits = 2),'qsa:',round(qsa,digits = 2),'q_ind:',
    #   q_lst$ind_q[[outnew]]$val),'\r')
    # }

    ucb <- ucb + qsa
    
    movenew <- data.table(id = move[type=='f']$id,s = move[type == 'f']$s,
                          type = move[type=='f']$type, str = move[type=='f']$str)
    
    movenew$a <- q_lst$a[matches_s[[which.max(ucb)]]]
    
    if(which.max(ucb) == which.max(qsa)){
      
      type_act <- 'exploit'
      #print('exploit')
      #print(ucb)
      #print(q_lst$ind_q[outnew])
      if(max(q_lst$ind_q[[outnew]]$val >= 0.05)){
        
        #next_state <- vector()
        
        for (i in which(q_lst$ind_q[[outnew]][order(id)]$val < 0.05)){
          
          j_switch <- 0
          rand_switch <- FALSE
          cur_move <- movenew[i,]
          cur_move$sid <- paste0(cur_move$id,cur_move$s)
          #print(cur_move)
          # print('here')
          #next_state <- actions[movenew[-i,], on = .(s,a)]
          # print(actions[movenew[-i,], on = .(s,a)])
          new_move <- apply(cur_move,FUN = selfun,MARGIN = 1,
                            prob = sing_lst, legal_a = leg_lst, rand = rand_switch)
          
          # print(new_move)
          # print(cur_move$a)

          
          while(new_move == cur_move$a){
            new_move <- apply(cur_move,FUN = selfun,MARGIN = 1,
                              prob = sing_lst, legal_a = leg_lst, rand = rand_switch)
            j <- j+1
            if(j>10){
              rand_switch <- TRUE
            }
          }
          
          #print(new_move)
          #print(movenew$sp)
          # if(max(table(move[type =='f']$sp))>1) {
          #   print('whoopsie')
          #   print(actions[s == cur_move$s & !(sp %in% movenew$sp)])
          #   print(new_move)
          #   movenew
          # }
          
          movenew[i,]$a <- new_move
          # print(movenew)
          
        }
      }
    } else {
      
      type_act <- 'explore'
    }
    
    #print(movenew)
    movenew <- actions[movenew, on = .(s,a)]
    # if(max(table(movenew$sp))>1){
    #   print('update')
    #   print(movenew)
    #   print(move[type == 'e'])
    #   break
    # }

    trans <- transition_function2(movenew,move[type == 'e'])
    
    #grad_rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    
    #rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad_rew)
    
    #td2 <- Sys.time() - t2
    typeout <- 'update'
    
    df_t <- rbind(df_t, data.table(event = 'ex_update',t = Sys.time()-t1))
    
  }else{
    
    type_act <- 'new'
    # if(max(table(move[type =='f']$sp))>1){
    #   print('new')
    #   print(move)
    # }
    # print(move)
    trans <- transition_function2(move[type == 'f'],move[type == 'e'])
    # print(trans)
    grad_rew <- 0#grad_reward(trans, grad, c = .25)
    # print(trans)
    rew <- reward_new(trans,grad_rew)
    #td2 <- Sys.time() - t2
    
    df_t <- rbind(df_t, data.table(event = 'ex_new',t = Sys.time()-t1))
    typeout <- 'new'
  }
  
  dtout <- df_t
  # print('trans')
  # print(rbind(trans[[1]][order(id)],trans[[2]][order(id)]))
  #print(rew)
  #print(list('he',trans[[1]],trans[[2]]))
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew,grad_rew,type_act,dtout))
  
}



q_update <- function(q_lst, transition,last_val, gamma = 0.95,j) {
  
  trigger <- F
  
  transition[[1]][order(id)]
  #print(transition)
  s_old_vec <- paste0(t(transition[[1]][type == 'f',list(id,s)]),collapse = '')
  
  s_vec <- paste0(t(transition[[1]][type =='f',list(id,sp)]),collapse = '')
  
  sa_vec <- paste0(paste0(t(transition[[1]][type == 'f',list(id,s)]),collapse = ''),
                   paste0(t(transition[[1]][type=='f',list(a)]),collapse = ''),
                   collapse = '')
  
  # matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
  #                                                        t(transition[[1]][type=='f',list(a)]))))
  matches <- q_lst$sa$sa %in% sa_vec
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  
  u_ind <- last_val[[1]]
  # print(outnew)
  # print(transition[[4]])
  # print(last_val)
  val <- transition[[2]][[1]]
  ind_dt <- transition[[2]][[2]]
  
  val <- (val + gamma * u_ind)
  
  # print(last_val)
  # print(ind_dt)
  # print(last_val[[2]])
  # print('old q')
  # print(length(q_lst$q))
  # print(length(q_lst$ind_q))
  # print(q_lst$ind_q[outnew])
  # print(j)
  # 
  # print(ind_dt)
  # print(last_val[[2]])
  indv_q <- data.table::merge.data.table(ind_dt,last_val[[2]], by = "id", all = TRUE,  suffixes = c('_new','_old'))
  indv_q[is.na(indv_q)] <- 0
  indv_q_update <- data.table(id = indv_q$id, val = indv_q$val_new + gamma * indv_q$val_old)
  #print(indv_q_update)
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) == 0) {
    
    
    #val <- (val + gamma^j * last_val[[1]])
    
    q_lst$s <- rbind(q_lst$s,data.table(s = s_old_vec))
    q_lst$a <- append(q_lst$a,list(as.vector(t(transition[[1]][type=='f',list(a)]))))
    q_lst$sa <- rbind(q_lst$sa,data.table(sa = sa_vec))
    q_lst$q <- append(q_lst$q,list(val))
    q_lst$n <- append(q_lst$n,list(1))
    q_lst$grad_rew <- append(q_lst$grad_rew,transition[[2]][[1]])
    
    
    q_lst$ind_q <- append(q_lst$ind_q, list(indv_q_update))

    #indv_q_update <- ind_dt

    # print(q_lst)
  } else{
    
    matches <-  q_lst$sa$sa %in% sa_vec
    outnew <- which(matches, arr.ind = FALSE)
    
    
    q_lst$n[outnew] <- list(q_lst$n[[outnew]]+1)
    # if(u_ind > 25){
    #   print(u_ind)
    #   print(val)
    # }


    inv_q_mean <- data.table::merge.data.table(indv_q_update,q_lst$ind_q[[outnew]],
                                               by = "id", all = TRUE,
                                               suffixes = c('_new','_old'))
    inv_q_mean[is.na(inv_q_mean)] <- 0
    
    indv_q_update <- data.table(id = inv_q_mean$id, 
                                val = inv_q_mean$val_old + (inv_q_mean$val_new - inv_q_mean$val_old)/q_lst$n[[outnew]])
    # print('update')
    #print(val)
    q_lst$ind_q[outnew] <- list(indv_q_update)
    q_lst$q[outnew] <- list(q_lst$q[[outnew]] + (val - q_lst$q[[outnew]])/q_lst$n[[outnew]])
    
  }
  # print(indv_q_update)
  #print(val)
  return(list(q_lst,list(val,indv_q_update),outnew,trigger))
}

simulate_mcts <- function(unit_obj,last_a, legal_a, terr_loc, q, c = 5,
                          n_iter = 250, depth = 5, single_out,actions){
  
  avg_u <- 0
  bigval <- 0
  i <- 0
  df_type <- data.frame()
  time_stamp <- Sys.time()
  time_init <- Sys.time()
  actions_dt <- data.table(actions)
  prob_base <- prob_setup()
  df_log <- data.table()
  print(unit_obj)
  
  ### New stuff for belief update ####
  prob_ls <- split(prob_base, by = 'a')
  leg_ls <- split(data.table(legal_a), by = 's')
  sing_ls <- split(single_out, by = 's.s')
  
  #### Rest of stuff
  matches_s <- 1
  
  s_vec <- paste0(t(unit_obj[type == 'f',list(id,s)]),collapse = '')
  
  while(i < n_iter & max(unlist(q$n[matches_s])) < (.1 * n_iter)){
    
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
      time <- Sys.time()
      
      #print(input_state)
      #print(units_new)
      out <- execute_action(state=input_state,actions = legal_a[s %in% input_state$s],grad = terr_loc,
                            q_lst=q,c=c, lasta = input_state,act_dt = actions_dt,
                            probdf = single_out, depth = j,prob_b = prob_base, disc = 0.95,
                            prob_lst = prob_ls, leg_lst = leg_ls,sing_lst = sing_ls)
      
      t1 <- Sys.time()
      #print(out[[1]])
      df_log <- rbind(df_log,out[[5]])
      #print(c("Time Execute", Sys.time()-time))
      
      df_type <- bind_rows(df_type,data.frame(iter = i, depth = j, type = out[[4]]))
      
      lst_out <- append(lst_out,list(out))
      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type), a = c(out[[1]]$a))
      
      old_len <- nrow(units_new)
      # if(min(units_new$str) < 10){
      #   print(units_new)
      # }
      
      units_new <- units_new[str > 10]
      
      
      new_len <- nrow(units_new)

      acts_new <- acts_new[which(units_new$str > 10)]


      units_new[, a:= NULL]
      
      j <- j + 1
      
      df_log <- rbind(df_log, data.table(event = 'cleanup',t = Sys.time()-t1))
      
    }
    
    
    
    for(k in length(lst_out):1){
      t1 <- Sys.time()
      if(k == length(lst_out)){
        last_val_in <- 0
        last_val_dt <- data.table(id = units_new[type == 'f']$id, val = 0)
        last_val_in <- list(last_val_in,last_val_dt)
      }
      
      # print('update')
      # print(k)
      # #print(lst_out[[k]])
      # print(last_val_in)
      q_temp <- q_update(q,lst_out[[k]],last_val_in, gamma = 0.95, j = k)
      
      q <- q_temp[[1]]
      
      last_val_in <- q_temp[[2]]
      #print(df_log)
      df_log <- rbind(df_log, data.table(event = 'q_update',t = Sys.time()-t1))
      
    }
    
    val <- unlist(q_temp[[2]])
    val[is.null(val)] <- 0
    i <- i + 1

    matches_s <- q$s$s %in% s_vec
    
    matches_s <- which(matches_s)
    
    
  }
  #print(unit_obj)
  s_vec <- paste0(t(unit_obj[type == 'f',list(id,s)]),collapse = '')
  
  state_in <- q$s$s %in% s_vec
  #print(q$s$s[1:100])
  #state_in <- sapply(q$s,matchfun, new = as.vector(t(unit_obj[,list(id,s,str,type)])))
  
  
  
  lst_new <- data.table(q = unlist(q$q[which(state_in)]), a = (q$a[which(state_in)]))
  lst_new[order(-q)]
  total_time <- Sys.time() - time_init
  return(list(q,lst_new,df_type,df_log,total_time))
}


vec_add <- function(vector, add_vector){
  return(vector + add_vector)
}

belfun <- function(df,plist){
  sample(plist[[df]]$nexta,1,prob = plist[[df]]$p)
}

samefun <- function(df,olist){
  return(olist[[df[[1]]]][a == df[[2]]]$sp)
}

selfun <- function(state,prob,legal_a,rand){
  #print(prob[[stateid]])
  if(is.null(prob[[state[[6]]]])==TRUE | sum(prob[[state[[6]]]]$q) == 0 | rand == TRUE){
    
    sample(legal_a[[state[[2]]]]$a,1)
    #legal_a[[state]][sample(nrow(legal_a),1), ]
  } else{
    #print('here')
    sample(prob[[state[[6]]]]$a,1,replace = TRUE,prob =prob[[state[[6]]]]$q )
    #prob[[stateid]][sample(nrow(prob),1,replace = TRUE,prob =prob[[stateid]]$q ), ]
  }
}

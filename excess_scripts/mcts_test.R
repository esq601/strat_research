source('hex_setup.R')
source('hex_conflict.R')
source('hex_funcs.R')
library(data.table)

matchfun <- function(x,new) {
  identical(new, x)
}

yes_fun <- function(vector){
  rbeta(n = 1, shape1 = vector[[1]], shape2 = vector[[2]])
}




execute_action <- function(state,actions,grad,q_lst,c, lasta, probdf) {
  
  #q_list <- 
  ### Sample a
  
  # Only current states are subsetted
  move <- actions[state, on = .(s)]
  #  print(move)
  # # print(actions)
  #print(state)
  #print(lasta)
  
  lasta <- lasta[id %in% state$id]
  #print('new lasta')
  #print(lasta)
  actvec <- vector()
  for(i in 1:nrow(lasta)){
    #print(nrow(lasta))
    #print(lasta[[i,2]])
    
    probs <- probdf[a == lasta[[i,2]]]
    availa <- actions[s == lasta[[i,1]]]
    #print('here')
    #print(probs)
    #print(availa)
    outtest <- probs[availa, on = c('nexta' = 'a')]
    #print('next here')
    outtest[, pnew := (p/sum(p))]
    #print(outtest)
    act_select <- outtest[sample(length(outtest$pnew), size = 1, prob = outtest$pnew)][[2]]
    #print(act_select)
    # if(length(act_select) > 1){
    #   print("bozooooooooo")
    # }
    
    actvec <- c(actvec,act_select)
    #print(probs)
    #print(act_select)
    
    # print(actions[s == lasta[[i,1]] & a == lasta[[i,2]]])
  }
  
  #### taking out the update stuff to test directional sampling
  # Sample from beta distribution
  #move$rand <- sapply(move$param, yes_fun)
  # print(actvec)
  #Select a with maximum sample
  #move <- move[, .SD[which.max(rand)], by = id]
  
  #### uncommen these if this shit doesn't work
  movesel <- data.table(s = state$s, a = actvec, id = state$id)
  # print(movesel)
  #print(movesel)
  
  move <- move[movesel, on = .(s,a,id)]
  #print(move)
  #move$a <- actvec
  
  move[type == 'e', a := 'adj0']
  move[type == 'e', sp := s]
  
  # print(move)
  ### If a in (s,a) select max UCB
  matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(move[,list(id,s,str,type)]),
                                                     t(move[type=='f',list(a)]))))
  
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) > 0) {
    #print(outnew)
    
    matches_s <- sapply(q_lst$s,matchfun, new = as.vector(t(state[,list(id,s,str,type)])))
    
    matches_s <- which(matches_s)
    
    ucb <- (c*sqrt(log(sum(unlist(q_lst$n[matches_s])))/unlist(q_lst$n[matches_s])))
    
    qsa <- unlist(q_lst$q[matches_s])
    #print(qsa)
    ucb <- ucb + qsa
    
    
    
    movenew <- data.table(s = move[type == 'f']$s,str = move[type == 'f']$str,type = 'f',
                          id = move[type=='f']$id,
                           rand = move[type =='f']$rand)
    movenew$a <- q_lst$a[matches_s[[which.max(ucb)]]]
    # print(ucb)
    # print(matches_s[[which.max(ucb)]])
    # print(q_lst$sa[matches_s[[which.max(ucb)]]])
    # print(matches_s)
    # print('here')
    # print(movenew)
    if(which.max(ucb) == which.max(qsa)){
      #print("Exploit!!!")
      type_act <- 'exploit'
    } else {
      #print('explore')
      type_act <- 'explore'
    }
    
    # print(movenew)
    movenew <- actions[movenew, on = .(s,a)]#move[type == 'f', a := q_lst$a[which.max(ucb)]]
    #print(movenew)
    #print(move)
    trans <- transition_function(movenew,move[type == 'e'])
    #print(trans)
    grad_rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
    #print('yooo')
    #print(grad_rew)
    
    rew <- q_lst$grad_rew[matches_s[[which.max(ucb)]]]
  }else{
    #print('else')
    type_act <- 'new'
    trans <- transition_function(move[type == 'f'],move[type == 'e'])
    
    grad_rew <- grad_reward(trans, grad, c = .25)
    
    rew <- reward_new(trans,grad,grad_rew)
  }

  # if(identical(as.vector(t(units[type=='f',list(id,s,str,type)])) , as.vector(t(trans[[1]][,list(id,s,str,type)])) )==FALSE) {
  #   rew <- reward_new(trans,grad, c = 5)
  # }
  # 
  # rew <- reward_new(trans,grad, c = 5)
  
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew,grad_rew,type_act))
  
}


q_update <- function(q_lst, transition, gamma = 0.95,j) {
  
  trigger <- F
  
  transition[[1]][order(id)]
  matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
                                                         t(transition[[1]][type=='f',list(a)]))))
  
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  #print(outnew)
  
  val <- transition[[2]]
  # if(val > 30){
  #   print(c('outnew',outnew,val))
  # }
  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) == 0) {
    #print('here')
    #print(outnew)
    q_lst$s <- append(q_lst$s,list(as.vector(t(transition[[1]][,list(id,s,str_old,type)]))))
    q_lst$a <- append(q_lst$a,list(as.vector(t(transition[[1]][type=='f',list(a)]))))
    q_lst$sa <- append(q_lst$sa,list(as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
                                                 t(transition[[1]][type=='f',list(a)])))))
    q_lst$q <- append(q_lst$q,list(val))
    q_lst$n <- append(q_lst$n,list(1))
    q_lst$grad_rew <- append(q_lst$grad_rew,transition[[2]])
    #print(transition[[2]])
  } 
  ### Else, add the reward to U(s') and increment N(s,a)
  #else {
  #print('down here now')
  # Check if next state exists
  #print(transition[[3]])
  #print(outnew)
  matches_s <- sapply(q_lst$s,matchfun, new = as.vector(t(transition[[1]][,list(id,sp,str,type)])))
  
  #print(as.vector(t(transition[[1]][,list(id,s,str_old,type)])))
  #print(as.vector(t(transition[[1]][,list(id,sp,str,type)])))
  
  next_state <- which(matches_s, arr.ind = FALSE)
  #print(c('nextstate',next_state))
  
  #print(as.vector(t(transition[[1]][,list(id,sp,str,type)])))
  # If next state does not exist
  if(length(next_state) > 0){
    #print(length(q_lst$n))
    #print('match!').
    matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
                                                           t(transition[[1]][type=='f',list(a)]))))
    outnew <- which(matches, arr.ind = FALSE)
    
    #print(c('outnew',outnew))
    # val <- q_lst$grad_rew[outnew]
    # print(val)
    u_ind <- utility_func(q_lst, as.vector(t(transition[[1]][,list(id,sp,str,type)])))
    
    #print(c('utility',u_ind,'reward',val,'n',q_lst$n[[outnew]]))
    
    q_lst$n[outnew] <- list(q_lst$n[[outnew]]+1)
    
    #print(c('oldq',q_lst$q[[outnew]],'newq',(val + gamma * u_ind)))
    
    
    #### Testing out using the max q value for the update
    #val <- q_lst$q[[outnew]] + ((((val + gamma * u_ind)-q_lst$q[[outnew]])) / q_lst$n[[outnew]])
    #print(val)
    val <- (val + gamma^j * u_ind)
    
    q_lst$q[outnew] <- list(val)
    # if(val > 30){
    #   
    #   trigger <- T
    #   print(c('old val',val))
    #   
    #   print(c('new val',val_new))
    #   print(c('index',outnew, 'q_value',q_lst$q[outnew]))
    #   
    # }
    #print(c('updated q',val))
    

    
    
  } 
  return(list(q_lst,val,outnew,trigger))
}

simulate_mcts <- function(unit_obj, legal_a, terr_loc, q, c = 5, n_iter = 250, depth = 5){
  
  avg_u <- 0
  bigval <- 0
  df_type <- data.frame()
  prob_tran <- prob_setup()
  time_stamp <- Sys.time()
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
      #searchtime <- Sys.time()
      
      if(j == 0 ){
        input_state <- data.table(s = units_new$s, a = "adj0",id = units_new$id)
        
      } else {
        
        input_state <- data.table(s = units_new$s, a = acts_new,id = units_new$id)
      }
      
      out <- execute_action(state=units_new,actions = legal_a,grad = terr_loc,q_lst=q,c=c, lasta = input_state, probdf = prob_tran)
      #searchtime <- Sys.time() - searchtime
      #print(paste("Search:",searchtime))
      #print(out)
      #print('now q')
      df_type <- bind_rows(df_type,data.frame(iter = i, depth = j, type = out[[4]]))
      
      #print(out)
      
      lst_out <- append(lst_out,list(out))
      
      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type), a = c(out[[1]]$a))
      
      #print(units_new)
      old_len <- nrow(units_new)
      units_new <- units_new[str > 10]
      new_len <- nrow(units_new)

      acts_new <- units_new$a
      units_new[, a:= NULL]
      # if(old_len > new_len){
      #   print(units_new)
      # }
      #print(units_new)
      j <- j + 1
      
      
      
    }

    
    #searchtime <- Sys.time()
    
    
    for(k in length(lst_out):1){
      #print(k)
      q_temp <- q_update(q,lst_out[[k]], gamma = 0.95, j = k)
      
      q <- q_temp[[1]]
      
      # if(q_temp[[4]] == TRUE){
      #   
      #   bigval <- q_temp[[3]]
      #   print(c('out qtemp',bigval,"out q",q$q[[bigval]]))
      # }
      
    }
    
    # if(bigval > 0){
    #   print(c('outside qtemp',bigval,"outside q",q$q[[bigval]]))
    # }
    
    #searchtime <- Sys.time() - searchtime
    #print(paste("Update:",searchtime))
    
    val <- unlist(q_temp[[2]])
    val[is.null(val)] <- 0
    
    avg_u <- avg_u + (val-avg_u)/i
    
    if(val > avg_u) {
      legal_a[paste(s,a) %in% paste(lst_out[[1]][[1]]$s,lst_out[[1]][[1]]$a),
              param := list(lapply(param,vec_add,add_vector = c(.01,0) ))]
    } else {
      legal_a[paste(s,a) %in% paste(lst_out[[1]][[1]]$s,lst_out[[1]][[1]]$a),
              param := list(lapply(param,vec_add,add_vector =  c(0,.01) ))]
    }
    
    
  }
  
  # if(bigval > 0){
  #   print(c('outside final qtemp',bigval,"outside final q",q$q[[bigval]]))
  # }

  
  state_in <- sapply(q$s,matchfun, new = as.vector(t(unit_obj[,list(id,s,str,type)])))
  
  lst_new <- data.table(q = unlist(q$q[which(state_in)]), a = (q$a[which(state_in)]),sp = (q$sp[which(state_in)]))
  lst_new[order(-q)]
  #print(lst_new)
  return(list(q,lst_new,df_type))
}

vec_add <- function(vector, add_vector){
  return(vector + add_vector)
}

#### start the thing



numu <- 3
nume <- 4

posf <- df2 %>%
  ungroup() %>%
  filter(x_pos < 16) %>%
  select(pos) %>%
  sample_n(numu)

pose <- df2 %>%
  ungroup() %>%
  filter(x_pos > 16) %>%
  select(pos) %>%
  sample_n(nume)



f_players <- data.table(
  id = paste0("inf_",1:numu),
  s = c('020711','030609','050506'),
  #s = posf$pos,
  str = 100,
  
  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('050809','071211','070706','061010'),
  #s = pose$pos,
  str = 100,
  
  #sp = c('071009','081008'),
  type = 'e'
)



legal_acts <- data.table(adj_df)
legal_acts[,param := list(c(1,1))]

legal_acts
units <- rbind(f_players,e_target)

units
unit_trans <- list(units[type=='f'],units[type == 'e'])

units_log <- data.table()
turn <- 0

actions <- c(paste0("adj",0:6))


selected_a <- sample(actions, nrow(units[type=='f']))

territory <- data.table(df2[,c('pos','x_pos','y_pos')])
territory[,lst := Map(list,x_pos,y_pos)]
unit_trans[[2]]
rew_start <- grad_reward(trans = unit_trans,territory,c = .25)

q_work <- list(s = list(as.vector(t(units))), a = list(selected_a),
          sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1), grad_rew = rew_start)



#out <- simulate_mcts(units,legal_acts,territory, q_work,c = 5, n_iter = 2000, depth = 6)


#q_work <- out[[1]]
while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10){
  
  legal_acts <- data.table(adj_df)
  legal_acts[,param := list(c(1,1))]
  print(turn)
  print(length(q_work$sa))

  # selected_a <- sample(actions, nrow(units[type=='f']))
  # 
  # q <- list(s = list(as.vector(t(units))), a = list(selected_a),
  #           sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1))
  # 
  #grad_out <- gradient_function(units[type == 'f']$s,units[type == 'e']$s)
  if(turn > 0){
    keep_val <- which(unlist(q_work$q) > quantile(unlist(q_work$q),.75))
    
    q_work$s <- q_work$s[keep_val]
    q_work$a <- q_work$a[keep_val]
    q_work$sa <- q_work$sa[keep_val]
    q_work$q <- q_work$q[keep_val]
    q_work$n <- q_work$n[keep_val]
    q_work$grad_rew <- q_work$grad_rew[keep_val]
  }
  
  # 
  # 
  print(length(q_work$sa))
  
  out <- simulate_mcts(units,legal_acts,territory, q_work,c = 50, n_iter = 1000, depth = 8)
  q_work <- out[[1]]
  
  #out[[2]][order(-q)][[1,2]]
  #rep('adj0',nrow(units[type == 'e']))
  
  
  units[,a :=c(out[[2]][order(-q)][[1,2]],rep('adj0',nrow(units[type == 'e']))) ]
  
  move <- legal_acts[units, on = .(s,a)]
  print(move)
  move[,param := NULL]
  trans <- transition_function(move[type == 'f'],move[type == 'e'])
  print(trans)
  print(out[[2]])
  
  
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,s=sp,str,type)]
  units <- units[str>10]
  #print(units)
  turn <- turn + 1
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  write_csv(units_log, 'mcts_test_28feb.csv')
  
}



test <- sapply(q_work$sa,paste,collapse=" ")

df <- data.frame(x = test, q = unlist(q_work$q), n = unlist(q_work$n), r = unlist(q_work$grad_rew), s = sapply(q_work$s,paste,collapse=" "))

all(c(T,T,T))
str(df)
identical(df$s,sample(df$s,1))



valtest <- sample(df$s,1)

df$match <- sapply(df$s,matchfun, new = valtest)

df %>%
  filter(match == TRUE) %>%
  select(x,q,n,r)

ggplot(df, aes(x = r, y = q)) +
  geom_point()

length(test)
ength(unique(test))

### Find starting state

out_test <- out

keep_val <- which(unlist(out_test[[1]]$q) > quantile(unlist(out_test[[1]]$q),.75))

testlst <- out_test[[1]]$q[keep_val]



### explore exploit examine

expexp <- out[[3]]


expexp2 <- expexp %>%
  mutate(turn = row_number()) %>%
  group_by(depth,type) %>%
  mutate(number_seen = row_number())

ggplot(expexp2) +
  geom_point(aes(x = turn, y = number_seen, color = type)) +
  facet_wrap(~depth)

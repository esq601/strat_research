source('hex_setup.R')
source('hex_conflict.R')
source('hex_funcs.R')
library(data.table)

numu <- 2
# posf <- df2 %>%
#   ungroup() %>%
#   filter(x_pos < 16) %>%
#   select(pos) %>%
#   sample_n(numu)
# 
# pose <- df2 %>%
#   ungroup() %>%
#   filter(x_pos > 16) %>%
#   select(pos) %>%
#   sample_n(numu)



f_players <- data.table(
  id = paste0("inf_",1:numu),
  s = c('020711','040810'),
  #s = posf$pos,
  str = 100,
  
  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:numu),
  s = c('050809','071211'),
  #s = pose$pos,
  str = 50,
  
  #sp = c('071009','081008'),
  type = 'e'
)

units <- rbind(f_players,e_target)
#units2 <- units[,str := str*.75]

actions <- c(paste0("adj",0:6))


selected_a <- sample(actions, nrow(units[type=='f']))

q <- list(s = list(as.vector(t(units))), a = list(selected_a),
          sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1))
q
q1 <- append(q,q)

#blanklist <- list(s = vector(), a = vector(), q = numeric(), n = integer())

#test <- append(blanklist,q)
q$s <- append(q$s,list(as.vector(t(units2))))

vectest <- as.vector(t(units))
vectest2 <- as.vector(t(units2))

matchfun <- function(x,new) {
  identical(new, x)
}

all(vectest %in% q$s[[1]])
matchfun(vectest,q$sa[[1]])

out <- sapply(q$s,matchfun, new = vectest)
which(out, arr.ind = TRUE)



grad_out <- gradient_function(move[type == 'f']$s,move[type == 'e']$s)


input_df <-legal_a[units, on ='s']
input_df[,param := list(c(1,1))]


yes_fun <- function(vector){
  rbeta(n = 1, shape1 = vector[[1]], shape2 = vector[[2]])
}

input_df$rand <- sapply(input_df$param, yes_fun)
selected_a <- input_df[, .SD[which.max(rand)], by = id]



execute_action <- function(state,actions,grad,q_lst,c) {
  
  #q_list <- 
  ### Sample a
  
  # Only current states are subsetted
  move <- actions[state, on = .(s)]
  
  #print(move)
  # Sample from beta distribution
  move$rand <- sapply(move$param, yes_fun)
  
  #Select a with maximum sample
  move <- move[, .SD[which.max(rand)], by = id]
  
  move[type == 'e', a := 'adj0']
  move[type == 'e', sp := s]
  ### If a in (s,a) select max UCB
  matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(move[,list(id,s,str,type)]),
                                                     t(move[type=='f',list(a)]))))
  
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) > 0) {
    
    matches_s <- sapply(q_lst$s,matchfun, new = as.vector(t(state[,list(id,s,str,type)])))
    
    matches_s <- which(matches_s)
    
    #print('change')
    #print(matches_s)
    #print(as.vector(t(state[,list(id,s,str,type)])))
    ucb <- (c*sqrt(log(sum(unlist(q_lst$n[matches_s])))/unlist(q_lst$n[matches_s])))
    #print(c(matches_s,q_lst$n[matches_s]))
    #print(unlist(q_lst$n[matches_s]))
    #print(ucb)
    qsa <- unlist(q_lst$q[matches_s])
    #print(qsa)
    ucb <- ucb + qsa
    # 
    # if(which.max(qsa) == which.max(ucb)){
    #   print('exploit')
    # } else {
    #   print('explore')
    # }
    # 
    #print(ucb)
    #print(matches_s[[which.max(ucb)]])
    # print(q_lst$a[which.max(ucb)])
    # print(move)
    movenew <- data.table(s = move[type == 'f']$s,str = move[type == 'f']$str,type = 'f',
                          id = move[type=='f']$id,
                           rand = move[type =='f']$rand)
    movenew$a <- q_lst$a[matches_s[[which.max(ucb)]]]
    # print(movenew)
    movenew <- actions[movenew, on = .(s,a)]#move[type == 'f', a := q_lst$a[which.max(ucb)]]
    #print(movenew)
    trans <- transition_function(movenew,move[type == 'e'])
  }else{
    #print('else')
    trans <- transition_function(move[type == 'f'],move[type == 'e'])
  }

  if(identical(as.vector(t(units[type=='f',list(id,s,str,type)])) , as.vector(t(trans[[1]][,list(id,s,str,type)])) )==FALSE) {
    rew <- reward_new(trans,grad)
    #print(rew)
    #print(as.vector(t(transition[[1]][,list(id,s,str,type)])))
    #print(trans)
  }
 
  rew <- reward_new(trans,grad)
  
  return(list(rbind(trans[[1]][order(id)],trans[[2]][order(id)]),rew))
  
}

#legal_a[c(s,a) == c('060101','adj2')]
#identical(c(1,2,3),c(1,3,2))
out <- execute_action(state=units,actions = legal_a,grad = grad_out,q_lst = q,c=1)
out
#t(out[[1]][,list(id,s,str,type)])

#as.vector(c(t(out[[1]][,list(id,s,str,type)]),t(out[[1]][type=='f',list(a)])))
#q

test <- data.table(s = c('020711','040810'),a = c('adj0','adj0'))

legal_a[test, on = .(s,a)]

c(1,1) + c(0,1)

d <- data.table()


q_update <- function(q_lst, transition, gamma = 0.95) {
  
  ### Determine what (s,a) pairs have a match in the Q list
  #print(transition)
  matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
                                                    t(transition[[1]][type=='f',list(a)]))))

  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  #print(outnew)
  
  
  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) == 0) {
    #print('here')
    #print(outnew)
    q_lst$s <- append(q_lst$s,list(as.vector(t(transition[[1]][,list(id,s,str_old,type)]))))
    q_lst$a <- append(q_lst$a,list(as.vector(t(transition[[1]][type=='f',list(a)]))))
    q_lst$sa <- append(q_lst$sa,list(as.vector(c(t(transition[[1]][,list(id,s,str_old,type)]),
                            t(transition[[1]][type=='f',list(a)])))))
    q_lst$q <- append(q_lst$q,list(transition[[2]]))
    q_lst$n <- append(q_lst$n,list(1))
  } 
  ### Else, add the reward to U(s') and increment N(s,a)
  else {
    #print('down here now')
    # Check if next state exists
    matches_s <- sapply(q_lst$s,matchfun, new = as.vector(t(transition[[1]][,list(id,sp,str_old,type)])))
    #print(as.vector(t(transition[[1]][,list(id,sp,str,type)])))
    # If next state does not exist
    if(sum(matches_s) == 0){
      #print('more here')
      #print(matches_s)
      # Set q as reward and increment N
      q_lst$q[outnew] <- list(transition[[2]])
      q_lst$n[outnew] <- list(q_lst$n[[outnew]]+1)
    } else {
      
      #print(as.vector(t(transition[[1]][,list(id,sp,str,type)])))
      #print(as.vector(t(transition[[1]][,list(id,sp,str_old,type)])))
      u_ind <- utility_func(q_lst, as.vector(t(transition[[1]][,list(id,sp,str_old,type)])))
      #print(u_ind)
      # matches_s <- which(matches_s)
      # if(u_ind > 0){
      #   print(u_ind)
      #   print(q_lst$q[[outnew]])
      # }
      
      # match_mat <- cbind(ind = matches_s,q=unlist(q_lst$q[matches_s]))
      # 
      # if(sum(match_mat[,2] == max(match_mat[,2])) == 1 ) {
      #   u_ind <- match_mat[match_mat[,2] == max(match_mat[,2]),][1]
      # } else {
      #   temp <- match_mat[match_mat[,2] == max(match_mat[,2]),]
      #   u_ind <- temp[sample(sum(match_mat[,2] == max(match_mat[,2])),size=1,replace=FALSE),][1]
      # }
      #print(transition)
      #print(outnew)
      
      q_lst$n[outnew] <- list(q_lst$n[[outnew]]+1)
      q_lst$q[outnew] <- list(q_lst$q[[outnew]] + (((transition[[2]] + gamma * u_ind)-q_lst$q[[outnew]]) / q_lst$n[[outnew]]))
      
      # if(u_ind > 0){
      #   print(q_lst$n[[outnew]])
      #   print(u_ind)
      #   print(q_lst$q[[outnew]])
      # }
      # if(identical(as.vector(t(units[,list(id,s,str,type)])) , as.vector(t(transition[[1]][,list(id,s,str,type)])) )==TRUE) {
      #   
      #   print(as.vector(t(transition[[1]][,list(id,s,str,type)])))
      #   print(c(transition[[1]],transition[[2]],u_ind,q_lst$q[[outnew]]))
      # }
    }

    #print(c(u_ind,q_lst$q[u_ind],q_lst$n[u_ind]))
  }
  
  return(list(q_lst,q_lst$q[outnew]))
}

test <- q_update(q,out,gamma = 0.95)

unlist(test,recursive = FALSE)
q <- list(s = list(as.vector(t(units))), a = list(selected_a),
          sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1))

simulate_mcts <- function(unit_obj, legal_a, grad_out, q, c = 5, n_iter = 250, depth = 5){
  
  avg_u <- 0
  
  #print(legal_a)
  
  for(i in 1:n_iter){
    units_new <- units
    #print(max(units_new[type == 'f']$str))
    j <- 0
    
    lst_out <- list()
    
    
    while(max(units_new[type == 'f']$str) > 10 & max(units_new[type == 'e']$str) > 10 & j < depth){
      
      out <- execute_action(state=units_new,actions = legal_a,grad = grad_out,q_lst=q,c=c)
      #print(out)
      #print('now q')
      lst_out <- append(lst_out,list(out))
      
      
      units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                              str = c(out[[1]]$str), type = c(out[[1]]$type))
      units_new <- units_new[str > 10]
      #print(units_new)
      j <- j + 1
      
      
      
    }
    
    for(k in length(lst_out):1){
      #a <- Sys.time()
      #print(lst_out[[i]])
      
      q_temp <- q_update(q,lst_out[[k]], gamma = 0.99)
      q <- q_temp[[1]]
      #print(Sys.time() - a)
    }
    
    val <- unlist(q_temp[[2]])
    val[is.null(val)] <- 0
    #print(val)
    
    avg_u <- avg_u + (val-avg_u)/i
    #print(avg_u)
    #print(legal_a[paste(s,a) %in% paste(lst_out[[1]][[1]]$s,lst_out[[1]][[1]]$a)])
    #print(lst_out[[1]][[1]])
    if(val > avg_u) {
      legal_a[paste(s,a) %in% paste(lst_out[[1]][[1]]$s,lst_out[[1]][[1]]$a), param := list(lapply(param,vec_add,add_vector = c(1,0) ))]
    } else {
      legal_a[paste(s,a) %in% paste(lst_out[[1]][[1]]$s,lst_out[[1]][[1]]$a), param := list(lapply(param,vec_add,add_vector =  c(0,1) ))]
    }
    
    #print(legal_a[paste(s,a) %in% paste(lst_out[[1]][[1]]$s,lst_out[[1]][[1]]$a)])
    
    
  }
  
  state_in <- sapply(q$s,matchfun, new = as.vector(t(unit_obj[,list(id,s,str,type)])))
  
  lst_new <- data.table(q = unlist(q$q[which(state_in)]), a = (q$a[which(state_in)]),sp = (q$sp[which(state_in)]))
  lst_new[order(-q)]
  #print(lst_new)
  return(list(q,lst_new))
}

test <- data.frame(lapply(legal_acts$param,vec_add,add_vector = c(1,1)))

str(legal_a)

out
legal_a

legal_acts[paste(s,a) %in% paste(move$s,move$a), param := list(lapply(param,vec_add,add_vector = c(1,1) ))]
legal_acts$param
legal_acts[paste(s,a) %in% paste(move$s,move$a)]

vec_add <- function(vector, add_vector){
  return(vector + add_vector)
}



### Start the thing

# f_players <- data.table(
#   id = paste0("inf_",1:numu),
#   s = c('020711','040810'),
#   #s = posf$pos,
#   str = 100,
#   
#   type = 'f'
# )
# 
# 
# e_target <- data.table(
#   #id = c('inf_a','inf_b'),
#   id = paste0("eny_",1:numu),
#   s = c('050809','071211'),
#   #s = pose$pos,
#   str = 80,
#   
#   #sp = c('071009','081008'),
#   type = 'e'
# )



numu <- 2
nume <- 2

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
  s = c('040911','030609'),
  #s = posf$pos,
  str = 100,
  
  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('050809','071211'),
  #s = pose$pos,
  str = 60,
  
  #sp = c('071009','081008'),
  type = 'e'
)



legal_acts <- data.table(adj_df)
legal_acts[,param := list(c(1,1))]

legal_acts
units <- rbind(f_players,e_target)

units

units_log <- data.table()
turn <- 0

actions <- c(paste0("adj",0:6))


selected_a <- sample(actions, nrow(units[type=='f']))

q <- list(s = list(as.vector(t(units))), a = list(selected_a),
          sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1))


while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10){
  
  legal_acts <- data.table(adj_df)
  legal_acts[,param := list(c(1,1))]
  print(turn)
  selected_a <- sample(actions, nrow(units[type=='f']))
  
  q <- list(s = list(as.vector(t(units))), a = list(selected_a),
            sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1))
  
  grad_out <- gradient_function(units[type == 'f']$s,units[type == 'e']$s)
  
  out <- simulate_mcts(units,legal_acts,grad_out, q,c = 25, n_iter = 150, depth = 5)
  #q <- out[[1]]
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
  
  
}
summary(unlist(q$n))
### Find starting state




selected_a

  
  
out <- execute_action(state=units_new,actions = legal_a,grad = grad_out,q_lst=q,c=5)
  #print(out)
  #print('now q')
  lst_out <- append(lst_out,list(out))
  
  
  units_new <- data.table(id = c(out[[1]]$id), s = c(out[[1]]$sp),
                          str = c(out[[1]]$str), type = c(out[[1]]$type))
  units_new <- units_new[str > 10]
  #print(units_new)
  j <- j + 1
  
  
  



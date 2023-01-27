source('hex_setup.R')
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
  str = 1,
  
  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:numu),
  s = c('050809','071211'),
  #s = pose$pos,
  str = .15,
  
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



legal_a <- data.table(adj_df)

samp[samp[, .I[sample(.N, 1)], by = id][[2]]]

units
moves <- legal_a[units, on = .(s)]


dt <- CJ(moves, sorted = FALSE)
dt[,Idx:=.I]
dt[,by=Idx, Obs:=all_empty(outcome, groupvar, gradevar, regionvar)]
dt[,Idx:=NULL]




move <- legal_a[units, on = .(s)][legal_a[units, on = .(s)][, .I[sample(.N, 1)], by = id][[2]]]
move

grad_out <- gradient_function(move[type == 'f']$s,move[type == 'e']$s)

sqrt(log(10)/1)

vec_global <- 1

c(vec_global,1)

execute_action <- function(state,actions,grad,q_lst,c) {
  
  #q_list <- 
  ### Sample a
  move <- actions[state, on = .(s)][actions[state, on = .(s)][, .I[sample(.N, 1)], by = id][[2]]]
  
  ### If a in (s,a) select max UCB
  matches <- sapply(q_lst$sa,matchfun, new = as.vector(c(t(move[,list(id,s,str,type)]),
                                                     t(move[type=='f',list(a)]))))
  
  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = FALSE)
  
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) > 0) {
    matches_s <- sapply(q_lst$s,matchfun, new = as.vector(t(state[,list(id,s,str,type)])))
    
    matches_s <- which(matches_s)
    
    print('change')
    
    #print(as.vector(t(state[,list(id,s,str,type)])))
    ucb <- (c*sqrt(log(sum(unlist(q_lst$n[matches_s])))/unlist(q_lst$n[matches_s])))
    qsa <- unlist(q_lst$q[matches_s])
    ucb <- ucb + qsa
    # print(which.max(ucb))
    # print(q_lst$a[which.max(ucb)])
    # print(move)
    movenew <- data.table(s = move[type == 'f']$s,str = move[type == 'f']$str,type = 'f',
                          id = move[type=='f']$id)
    movenew$a <- q_lst$a[which.max(ucb)]
    # print(movenew)
    movenew <- actions[movenew, on = .(s,a)]#move[type == 'f', a := q_lst$a[which.max(ucb)]]
    print(movenew)
    trans <- transition_function(movenew,move[type == 'e'])
  }else{
    print('else')
    trans <- transition_function(move[type == 'f'],move[type == 'e'])
  }
  
  
  rew <- reward_new(trans,grad)
  
  return(list(rbind(trans[[1]],trans[[2]]),rew))
  
}

#legal_a[c(s,a) == c('060101','adj2')]
#identical(c(1,2,3),c(1,3,2))
out <- execute_action(state=units,actions = legal_a,grad = grad_out,q_lst = q,c=2)
out
#t(out[[1]][,list(id,s,str,type)])

#as.vector(c(t(out[[1]][,list(id,s,str,type)]),t(out[[1]][type=='f',list(a)])))
#q

test <- data.table(s = c('020711','040810'),a = c('adj0','adj0'))

legal_a[test, on = .(s,a)]



q_update <- function(q_lst, transition) {
  
  ### Determine what (s,a) pairs have a match in the Q list
  matches <- sapply(q$sa,matchfun, new = as.vector(c(t(transition[[1]][,list(id,s,str,type)]),
                                                    t(transition[[1]][type=='f',list(a)]))))

  ### Count and index the matches of (s,a)
  outnew <- which(matches, arr.ind = TRUE)
  print(outnew)
  ### If there is no match, initiate counter and set Q=r
  if(length(outnew) == 0) {
    print('here')
    q_lst$s <- append(q_lst$s,list(as.vector(t(transition[[1]][,list(id,s,str,type)]))))
    q_lst$a <- append(q_lst$a,list(as.vector(t(transition[[1]][type=='f',list(a)]))))
    q_lst$sa <- append(q_lst$sa,list(as.vector(c(t(transition[[1]][,list(id,s,str,type)]),
                            t(transition[[1]][type=='f',list(a)])))))
    q_lst$q <- append(q_lst$q,list(transition[[2]]))
    q_lst$n <- append(q_lst$n,list(1))
  } 
  ### Else, add the reward to U(s') and increment N(s,a)
  else {
    
  }
  
  return(q_lst)
}

for(i in 1:20){
 # print(i)
  out <- execute_action(state=units,actions = legal_a,grad = grad_out,q_lst=q,c=1)
  #out
  q <- q_update(q,out)
}


out <- execute_action(state=units,actions = legal_a,grad = grad_out)
#out
q <- q_update(q,out)
length(outnew)
out
reward_new(out,grad_out)

transition_function(move[type == 'f'],move[type == 'e'])




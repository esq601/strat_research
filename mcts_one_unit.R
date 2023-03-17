library(foreach)
library(doParallel)
library(patchwork)
source('mcts_functs.R')


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



#### Testing




numu <- 4
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
  s = c('020711','111106','041012','030407'),#c('020711','030609','050506'),
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
f_players[2]
units
unit_trans <- list(units[type=='f'],units[type == 'e'])

units_log <- data.table()
turn <- 0

actions <- c(paste0("adj",0:6))


selected_a <- rep('adj2', nrow(units[type=='f']))

territory <- data.table(df2[,c('pos','x_pos','y_pos')])
territory[,lst := Map(list,x_pos,y_pos)]
unit_trans[[2]]
rew_start <- grad_reward(trans = unit_trans,territory,c = .25)

q_work <- list(s = data.table(s = paste0(t(units),collapse = '')), a = list(rep('adj0',nrow(units[type=='f']))),
               sa = data.table(sa = paste0(paste0(t(units),collapse=''),paste0(rep('adj0',nrow(units[type=='f'])),collapse = ''),collapste = '')), 
               q = list(0), n =list(1), grad_rew = 0)#rew_start)


q_work
sim_change <- 1



selected_a <- c(rep('adj0',nrow(units[type == 'f'])), rep('adj0',nrow(units[type == 'e'])))





out <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr'),
                    .inorder = FALSE, .verbose = TRUE, .errorhandling = 'remove',
                    .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
                      
                      
                      
                      out <- simulate_one_mcts(rbind(f_players[i],e_target),selected_a,
                                               legal_a = legal_acts,terr_loc=territory,
                                               c = 30,
                                               n_iter = 500, depth = 8)  
                      out <- out[-1,]
                      out
                    }

lstout <- list()

for(i in 1:4){
  
  val <- paste0('inf_',i)
  
  dt2 <- out[id == val, .(max_value = max(q)), by = s.s]
  
  dt2[,s := str_sub(s.s,6)]
  
  
  hex3 <- hexdf2 %>%
    left_join(dt2, by = c('pos' = 's'))
  
  
  p <-ggplot(hex3, aes (x=x_h, y = y_h)) +
    geom_polygon(color = 'black',aes(group = pos,fill = (max_value))) +
    geom_polygon(data = hex3 %>% filter(pos == f_players[id == val]$s), fill = 'green') +
    geom_polygon(data = hex3 %>% filter(pos %in% e_target$s),aes(group = pos), fill = 'red') +
    theme(legend.position = 'none',
          axis.title = element_blank()) +
    coord_equal()
  
  lstout[[i]] <- p
}

(lstout[[1]] + lstout[[2]]) / (lstout[[3]] + lstout[[4]])




dt3 <- out[s.s == sample(out[id == val]$s.s,1)]

dt3[,s := str_sub(s.s,6)]


hex4 <- hexdf2 %>%
  left_join(dt3, by = c('pos' = 's'))


ggplot(hex4, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = (q))) +
  geom_polygon(data = hex4 %>% filter(pos == f_players[id == val]$s), fill = 'green') +
  geom_polygon(data = hex4 %>% filter(pos %in% e_target$s),aes(group = pos), fill = 'red') +
  coord_equal()

ggplot(dt3) +
  geom_bar(aes(x = a, y = q), stat = 'identity')


actions_dt <- data.table(actions)

new_dt <- dt3[actions_dt, on = c('a' ='actions')]
new_dt[,q:=q+1]
new_dt[is.na(q),q:= 1]

legal_acts <- data.table(legal_acts)

new_dt[a %in% legal_acts[s == unique(dt3$s)]$a]


samples <- rdirichlet(n = 1, alpha = new_dt$q)
max_indices <- max.col(samples)

ggplot(data.frame(x = max_indices)) +
  geom_bar(aes(x=x))


out1 <- out %>%
  mutate(s = str_sub(s.s,6)) %>%
  group_by(id,s.s) %>%
  nest()



dir_setup <- function(dataframe,leg_a, act_dt) {
  state <- unique(dataframe$s)
  new_dt <- data.table(dataframe)[act_dt, on = c('a' ='actions')]
  new_dt[,q:=q+1]
  new_dt[is.na(q),q:= 1]
  out <- new_dt[a %in% leg_a[s == state]$a]
  out
}

dir_setup(out1$data[[1]], legal_acts, actions_dt)

which('inf_100'==out1$s.s)

out1$probs <- lapply(out1$data,dir_setup, leg_a = legal_acts)

str(out1)

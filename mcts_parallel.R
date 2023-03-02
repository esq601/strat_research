library(foreach)
library(doParallel)
source('mcts_functs.R')


cores=detectCores()
cl <- makeCluster(cores[1]/2) #not to overload your computer
registerDoParallel(cl)
cores

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


selected_a <- rep('adj2', nrow(units[type=='f']))

territory <- data.table(df2[,c('pos','x_pos','y_pos')])
territory[,lst := Map(list,x_pos,y_pos)]
unit_trans[[2]]
rew_start <- grad_reward(trans = unit_trans,territory,c = .25)

q_work <- list(s = list(as.vector(t(units))), a = rep('adj0',nrow(units[type=='f'])),
               sa = list(as.vector(c(t(units),rep('adj0',nrow(units[type=='f']))))), 
               q = list(0), n =list(1), grad_rew = 0)#rew_start)


q_work
sim_change <- 1



selected_a <- c(rep('adj2',nrow(units[type == 'f'])), rep('adj0',nrow(units[type == 'e'])))





while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10){
  
  legal_acts <- data.table(adj_df)
  legal_acts[,param := list(c(1,1))]
  print(turn)
  print(length(q_work$sa))
  
  
  
  out_test <- foreach(i=1:(cores[1]/2), .combine = c,.packages = c('data.table','dplyr'),
          .inorder = FALSE, .verbose = TRUE, .errorhandling = 'remove',
          .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
  
  out <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                       q=q_work,c = 50*nrow(units[type == 'f']),
                       n_iter = sim_change*10, depth = 6)
  
  out[[2]]$iter <- i
  out[[3]]$sim <- i
  
  out <- append(out,list(i))
  
  out
          }
  
  policy_test <- rbindlist(out_test[(4*1:11)-2])
  
  selected_a <- policy_test[order(-q)][[1,2]]
  units[,a :=c(selected_a,rep('adj0',nrow(units[type == 'e']))) ]
  
  best_val <- policy_test[order(-q)][[1,3]]
  
  #q_work <- out_test[[(4*best_val)-3]]
  print(selected_a)
  
  
  expexp <- rbindlist(out_test[(4*1:11)-1])
  decide <- expexp[sim ==best_val & depth %in% c(0,1), .(count = .N), by = .(type,sim)]
  
  decide[, pct := count/sum(count)]
  
  sim_change <- decide[type == 'exploit'][[4]]
  #length(sim_change)
  sim_change <- ifelse(length(sim_change) == 0, 1, 1-sim_change)
  
  print(length(q_work$sa))
  print(sim_change)
  
  move <- legal_acts[units, on = .(s,a)]
  print(move)
  move[,param := NULL]
  trans <- transition_function(move[type == 'f'],move[type == 'e'])
  print(trans)
  #print(out[[2]])
  
  
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,s=sp,str,type)]
  units <- units[str>10]
  
  q_work <- list(s = list(as.vector(t(units))), a = rep('adj0',nrow(units[type=='f'])),
                 sa = list(as.vector(c(t(units),rep('adj0',nrow(units[type=='f']))))), 
                 q = list(0), n =list(1), grad_rew = 0)#rew_start)
  
  selected_a <- c(policy_test[order(-q)][[1,2]], rep('adj0',nrow(units[type == 'e'])))
                  
  #print(units)
  turn <- turn + 1
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  write_csv(units_log, 'mcts_test_01mar.csv')
  
}







expexp$sim <- sort(rep(1:11,3000))

sim_change <- 1-decide[[1,4]]

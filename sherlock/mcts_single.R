source('MCTS/mcts_functs.R')
source('MCTS/mcts_one_funcs.R')

install.packages('DirichletReg', repos = 'https://cran.wustl.edu/')
library(DirichletReg)
#### start the thing



numu <- 5
nume <- 6

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
  s = c('020711','030609','050506','040507','060404'),
  #s = posf$pos,
  str = 100,
  
  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('050809','071211','070706','061010','051011','060909'),
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

q_work <- list(s = data.table(s = paste0(t(units),collapse = '')), a = list(rep('adj0',nrow(units[type=='f']))),
               sa = data.table(sa = paste0(paste0(t(units),collapse=''),paste0(rep('adj0',nrow(units[type=='f'])),collapse = ''),collapste = '')), 
               q = list(0), n =list(1), grad_rew = 0)#rew_start)




sim_change <- 1



selected_a <- c(rep('adj2',nrow(units[type == 'f'])), rep('adj0',nrow(units[type == 'e'])))



# q_work <- list(s = list(as.vector(t(units))), a = list(selected_a),
#                sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1), grad_rew = 0)#rew_start)


#out <- simulate_mcts(units,legal_acts,territory, q_work,c = 5, n_iter = 2000, depth = 6)




###### While Loop


while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10){
  
  legal_acts <- data.table(adj_df)
  legal_acts[,param := list(c(1,1))]
  print(turn)
  print(nrow(q_work$sa))
  
  #### Subfind ####
  
  f_players <- units[type == 'f']
  e_target <- units[type == 'e']
  
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
  
  out1 <- out %>%
    mutate(s = str_sub(s.s,6)) %>%
    group_by(id,s.s) %>%
    nest()
  
  
  #### Main MCTS ####
  
  #units[,a := NULL]
  out <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                       q=q_work,c = 20*nrow(units[type == 'f']),
                       n_iter = 1000, depth = 8, single_out = out1, actions=actions)
  
  q_work <- out[[1]]
  out[[2]]
  #out[[2]][order(-q)][[1,2]]
  #rep('adj0',nrow(units[type == 'e']))
  
  
  units[,a :=c(out[[2]][order(-q)][[1,2]],rep('adj0',nrow(units[type == 'e']))) ]
  
  move <- legal_acts[units, on = .(s,a)]
  print(move)
  move[,param := NULL]
  trans <- transition_function(move[type == 'f'],move[type == 'e'])
  print(trans)
  print(out[[2]])
  
  selected_a <- c(trans[[1]][str>10 & order(id)]$a,trans[[2]][str>10 & order(id)]$a)
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,s=sp,str,type)]
  units <- units[str>10]
  
  
  
  
  #print(units)
  
  
  turn <- turn + 1
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  write_csv(units_log, 'mcts_test_27mar_a.csv')
  
}

saveRDS(out,'mcts_test_27mar.rds')



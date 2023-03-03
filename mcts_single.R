source('mcts_functs.R')
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



selected_a <- c(rep('adj2',nrow(units[type == 'f'])), rep('adj0',nrow(units[type == 'e'])))



# q_work <- list(s = list(as.vector(t(units))), a = list(selected_a),
#                sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1), grad_rew = 0)#rew_start)


q_work
#out <- simulate_mcts(units,legal_acts,territory, q_work,c = 5, n_iter = 2000, depth = 6)


#q_work <- out[[1]]
while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10){
  
  legal_acts <- data.table(adj_df)
  legal_acts[,param := list(c(1,1))]
  print(turn)
  print(length(q_work$sa))
  
  print(nrow(q_work$sa))
  
  #units[,a := NULL]
  out <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                       q=q_work,c = 40*nrow(units[type == 'f']),
                       n_iter = sim_change*2000, depth = 6)
  
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
  write_csv(units_log, 'mcts_test_02mar.csv')
  
}


View(out[[2]])

saveRDS(out,'mcts_test_o2mar.rds')


View(data.frame(x = unlist(out[[1]]$q)))

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

source('mcts_functs.R')
source('mcts_one_funcs.R')

#### start the thing



numu <- 6
nume <- 6


f_players <- data.table(
  id = paste0("inf_",1:numu),
  s = c('040608','030609','040810','060606','060404','070706'),
  #s = posf$pos,
  str = c(100),
  
  type = 'f',
  bg = c('1','1','1','2','2','2')
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('091310','101208','091209','111106','091108','091007'),
  #s = c('091108','091007','091209','111106','091310','101208'),
  #s = pose$pos,
  str = c(100,100),
  
  #sp = c('071009','081008'),
  type = 'e',
  bg = c('a','a','a','b','b','b')
)

legal_acts <- data.table(adj_df)

units <- rbind(f_players,e_target)


units_log <- data.table()
turn <- 0

actions <- c(paste0("adj",0:6))

territory <- data.table(df2[,c('pos','x_pos','y_pos')])
territory[,lst := Map(list,x_pos,y_pos)]

q_work <- list(s = data.table(s = paste0(t(units),collapse = '')), a = list(rep('adj0',nrow(units[type=='f']))),
               sa = data.table(sa = paste0(paste0(t(units),collapse=''),paste0(rep('adj0',nrow(units[type=='f'])),collapse = ''),collapste = '')), 
               q = list(0),ind_q = list(data.table(id = f_players$id,val = 0)), n =list(1), grad_rew = 0)#rew_start)


selected_a <- c(rep('adj0',nrow(units[type == 'f'])), rep('adj5',nrow(units[type == 'e'])))



# q_work <- list(s = list(as.vector(t(units))), a = list(selected_a),
#                sa = list(as.vector(c(t(units),t(selected_a)))), q = list(0), n =list(1), grad_rew = 0)#rew_start)

q_eny <- q_work
q_work
#out <- simulate_mcts(units,legal_acts,territory, q_work,c = 5, n_iter = 2000, depth = 6)




#### Individal Explore ####



# 
# out <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr'),
#                .inorder = FALSE, .verbose = TRUE, .errorhandling = 'remove',
#                .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
#                  
#                  
#                  
#                  out <- simulate_one_mcts(rbind(f_players[i],e_target),selected_a,
#                                           legal_a = legal_acts,terr_loc=territory,
#                                           c = 30,
#                                           n_iter = sim_change*1000, depth = 8)  
#                  out <- out[-1,]
#                  out
#                }
# 
# out1 <- out %>%
#   mutate(s = str_sub(s.s,6)) %>%
#   group_by(id,s.s) %>%
#   nest()


###### While Loop

unit_bg1 <- units[bg %in% c('1','a','b')]
unit_bg2 <- units[bg %in% c('2','a','b')]

# 
# uniteny_bga <- units_eny[bg %in% c('a','1','2')]
# uniteny_bgb <- units_eny[bg %in% c('b','1','2')]

key_tern <- data.table(s = c('040911','070605'), value = c(1,1), type = 'f')
key_tern1 <- data.table(s = c('040911','070605'), value = c(1,0), type = 'f')
key_tern2 <- data.table(s = c('040911','070605'), value = c(0,1), type = 'f')

q_work <- list(s = data.table(s = paste0(units[type=='f']$id,units[type=='f']$s,collapse = '')), a = list(rep('adj0',nrow(units[type=='f']))),
               sa = data.table(sa = paste0(units[type=='f']$id,units[type=='f']$s,rep('adj0',nrow(units[type == 'f'])),collapse = '')), 
               q = list(0),ind_q = list(data.table(id = f_players$id,val = 0)), n =list(1), grad_rew = 0)#rew_start)


#selected_a <- c(rep('adj0',nrow(units[type == 'f'])), rep('adj5',nrow(units[type == 'e'])))

q_eny <- list(s = data.table(s = paste0(units[type=='e']$id,units[type=='e']$s,collapse = '')), a = list(rep('adj0',nrow(units[type=='e']))),
              sa = data.table(sa = paste0(units[type=='e']$id,units[type=='e']$s,rep('adj0',nrow(units[type == 'e'])),collapse = '')), 
              q = list(0),ind_q = list(data.table(id = e_target$id,val = 0)), n =list(1), grad_rew = 0)#rew_start)

q_work1 <- q_work
q_work2 <- q_work
q_enya <- q_eny
q_enyb <- q_eny
#q_work <- out[[1]]
while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10 & turn <= 25){
  
  q_work1 <- copy(q_work)
  q_work2 <- copy(q_work)
  q_enya <- copy(q_eny)
  q_enyb <- copy(q_eny)
  

  
  #q_eny <- q_work
  
  legal_acts <- data.table(adj_df)
  
  #legal_acts <- data.table(adj_df)
  print(turn)
  print(nrow(q_work$sa))
  
  #### Subfind ####
  
  f_players <- units[type == 'f']
  e_target <- units[type == 'e']
  time_single <- Sys.time()
  i<- 1
  unit_bg1 <- units[bg %in% c('1','a','b')]
  unit_bg2 <- units[bg %in% c('2','a','b')]
  
  cl <- makeCluster(cores[1]/2)
  registerDoParallel(cl)
  
  out_single <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr'),
                 .inorder = FALSE, .verbose = TRUE, .errorhandling = 'remove',
                 .export = c('actions_samp','trunc_func','grad_reward')) %dopar% {
                   
                   
                   single_a <- c(selected_a[i],selected_a[(length(selected_a)-nrow(e_target)+1):length(selected_a)])
                   
                   
                   out <- simulate_one_mcts(rbind(f_players[i],e_target),single_a,
                                             legal_a = legal_acts,terr_loc=territory,actions = actions,
                                             c = 1.5,
                                             n_iter = 500, depth = 8, k_terr = key_tern)  
                   out <- out[-1,]
                   out
                 }
  #hold <- out_single
  #write.csv(out_single,"bg_test_start.csv")
  print(paste("Friendly Single Time:",Sys.time()-time_single))
  
  stopCluster(cl)
  registerDoSEQ()
  #out_single[,q:=ifelse(q < 0, 0, q)]
  
  # out1 <- out_single %>%
  #   mutate(s = str_sub(s.s,6)) %>%
  #   group_by(id,s.s) %>%
  #   nest()
  
  # out_single
  #### Main MCTS ####
  
  #units[,a := NULL]
  out <- simulate_mcts(unit_bg1,rep('adj0',nrow(unit_bg1)),legal_a = legal_acts,terr_loc=territory, 
                       q=q_work1,c =0.2,
                       n_iter = 1000, depth =8, single_out = out_single, actions=actions,
                       k_terr = key_tern1)
  
  #q_work1 <- out[[1]]
  (out[[2]][order(-q)])
  
  out2 <- simulate_mcts(unit_bg2,rep('adj0',nrow(unit_bg2)),legal_a = legal_acts,terr_loc=territory, 
                       q=q_work2,c =0.2,
                       n_iter = 1000, depth =8, single_out = out_single, actions=actions,
                       k_terr = key_tern2)
  
  #q_work2 <- out2[[1]]
  (out2[[2]][order(-q)])
  #out[[2]][order(-q)][[1,2]]
  #rep('adj0',nrow(units[type == 'e']))
  
  #### Added this if statement to setup fixed policy for E
  #### To return, leave the adj0 one without if
  
  
  
  #### Determine opponent action!
  
  units_eny <- copy(units)
  eny_tern <- copy(key_tern)
  
  units_eny[,type := ifelse(type == 'e','f','e')]
  eny_tern[,type := ifelse(type == 'e','f','e')]
  
  uniteny_bga <- units_eny[bg %in% c('a','1','2')]
  uniteny_bgb <- units_eny[bg %in% c('b','1','2')]
  
  eny_tern_a <- copy(eny_tern)
  eny_tern_a[,value := c(1,0)]
  eny_tern_b <- copy(eny_tern)
  eny_tern_b[,value := c(0,1)]
  units_eny <- units_eny[order(-type,id)]
  
  f_players <- units_eny[type == 'f']
  e_target <- units_eny[type == 'e']
  
  selected_a_eny <- c(selected_a[(length(selected_a)-nrow(f_players)+1):length(selected_a)],
                      selected_a[1:nrow(e_target)])
  cl <- makeCluster(cores[1]/2)
  registerDoParallel(cl)
  #i <- 1
  out_single <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr'),
                        .inorder = FALSE, .verbose = TRUE, .errorhandling = 'remove',
                        .export = c('actions_samp','trunc_func','grad_reward')) %dopar% {
                          
                          
                          single_a <- c(selected_a_eny[i],selected_a_eny[(length(selected_a)-nrow(e_target)+1):length(selected_a_eny)])
                          
                          
                          out <- simulate_one_mcts(rbind(f_players[i],e_target),single_a,
                                                   legal_a = legal_acts,terr_loc=territory,actions = actions,
                                                   c = 1.5,
                                                   n_iter = 500, depth = 8, k_terr = eny_tern)  
                          out <- out[-1,]
                          out
                        }
  #hold <- out_single
  
  stopCluster(cl)
  registerDoSEQ()
  #out_single[,q:=ifelse(q < 0, 0, q)]
  
  out_single
  #### Main MCTS ####

  #units[,a := NULL]
  out_eny_a <- simulate_mcts(uniteny_bga,selected_a_eny[c(1:3,7:12)],legal_a = legal_acts,terr_loc=territory, 
                       q=q_enya,c =0.2,
                       n_iter = 1000, depth =8, single_out = out_single,
                       actions=actions, k_terr = eny_tern_a)
  
  #q_enya <- out_eny_a[[1]]
  
  out_eny_a[[2]][order(-q)]
  
  out_eny_b <- simulate_mcts(uniteny_bgb,selected_a_eny[c(4:12)],legal_a = legal_acts,terr_loc=territory, 
                             q=q_enyb,c =0.2,
                             n_iter = 1000, depth =8, single_out = out_single,
                             actions=actions, k_terr = eny_tern_b)
  
  #q_enyb <- out_eny_b[[1]]
  
  out_eny_b[[2]][order(-q)]
  units[,a :=c(out[[2]][order(-q)][[1,2]],out2[[2]][order(-q)][[1,2]],
               out_eny_a[[2]][order(-q)][[1,2]],out_eny_b[[2]][order(-q)][[1,2]]) ]
  #units[,a :=c(out[[2]][order(-q)][[1,2]],c('adj6','adj0','adj5')) ]
  
  
  
  move <- legal_acts[units, on = .(s,a)]
  print(move)
  #move[,param := NULL]
  trans <- transition_function2(move[type == 'f'],move[type == 'e'],key_terrain = key_tern)
  print(trans)
  #print(out[[2]])
  
  key_tern <- trans[[6]]
  
  key_tern1 <- copy(key_tern)
  key_tern1[,value := c(1,0)]
  key_tern2 <- copy(key_tern)
  key_tern2[,value := c(0,1)]
  
  
  selected_a <- c(trans[[1]][str>10 & order(id)]$a,trans[[2]][str>10 & order(id)]$a)
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,s=sp,str,type,bg)]
  units <- units[str>10]
  #print(units)
  
  
  turn <- turn + 1
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  write_csv(units_log, 'results/mcts_test_two_player_03may_c.csv')
  
}


View(out[[2]])

saveRDS(out,'mcts_test_12apr.rds')


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
  geom_point(aes(x = iter, y = number_seen, color = type)) +
  facet_wrap(~depth) +
  ggsci::scale_color_lancet() +
  theme_minimal() +
  labs(
    y = "Count",
    x = "Simulation",
    color = "Action",
    title = "Sample MCTS Search for Tactical Action",
    subtitle = "Subgraph at each depth of search."
  )
#ggsave('images/mcts_expexp_07apr.jpeg',height = 6, width = 8, dpi = 320)




#### look at logs

df <- out[[4]]

summary(out[[4]])

sum(sum(df$init),sum(df$ifs))

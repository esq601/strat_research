source('mcts_functs.R')
source('mcts_one_funcs.R')
library(DirichletReg)
#### start the thing



numu <- 3
nume <- 3


f_players <- data.table(
  id = paste0("inf_",1:numu),
  s = c('040507','060808','070706'),
  #s = posf$pos,
  str = c(100,100,80),

  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('081008','090906', '101107'),
  #s = pose$pos,
  str = c(100,100,80),

  #sp = c('071009','081008'),
  type = 'e'
)
# f_players <- data.table(
#   id = paste0("inf_",1:numu),
#   s = c('060707','050708','080604'),
#   #s = posf$pos,
#   str = c(53,41,17),
#   type = 'f'
# )
# 
# 
# e_target <- data.table(
#   #id = c('inf_a','inf_b'),
#   id = paste0("eny_",1:nume),
#   s = c('070908','071009'),
#   #s = pose$pos,
#   str = c(38,14),
#   
#   #sp = c('071009','081008'),
#   type = 'e'
# )

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







#q_work <- out[[1]]
while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10 & turn <= 25){
  
  timeout <- Sys.time()
  
  q_work <- list(s = data.table(s = paste0(t(units),collapse = '')), a = list(rep('adj0',nrow(units[type=='f']))),
                 sa = data.table(sa = paste0(paste0(t(units),collapse=''),paste0(rep('adj0',nrow(units[type=='f'])),collapse = ''),collapste = '')), 
                 q = list(0),ind_q = list(data.table(id = f_players$id,val = 0)), n =list(1), grad_rew = 0)#rew_start)
  
  q_eny <- q_work
  
  legal_acts <- data.table(adj_df)
  #legal_acts[,param := list(c(1,1))]
  print(turn)
  print(nrow(q_work$sa))
  
  #### Subfind ####

  
  #### Friendly setup
  
  f_players <- units[type == 'f']
  e_target <- units[type == 'e']
  
  # 
  out_single <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr','foreach','doParallel'),
                        .inorder = FALSE, .verbose = FALSE, .errorhandling = 'remove',
                        .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
                          
                          
                          single_a <- c(selected_a[i],selected_a[(length(selected_a)-nrow(e_target)+1):length(selected_a)])
                          
                          
                          out <- simulate_one_mcts(rbind(f_players[i],e_target),single_a,
                                                   legal_a = legal_acts,terr_loc=territory,actions = actions,
                                                   c = 2,
                                                   n_iter = 200, depth = 5)
                          out <- out[-1,]
                          out
                        }
  #hold <- out_single
  
  out_single[,q:=ifelse(q < 0, 0, q)]
  
  out1 <- out_single %>%
    mutate(s = str_sub(s.s,6)) %>%
    group_by(id,s.s) %>%
    nest()
  
  
  ### Eny setup
  
  units_eny <- copy(units)
  
  units_eny[,type := ifelse(type == 'e','f','e')]
  units_eny <- units_eny[order(-type,id)]
  
  f_players <- units_eny[type == 'f']
  e_target <- units_eny[type == 'e']
  
  selected_a_eny <- c(selected_a[(length(selected_a)-nrow(f_players)+1):length(selected_a)],
                      selected_a[1:nrow(e_target)])
  
  
  out_single <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr','foreach','doParallel'),
                        .inorder = FALSE, .verbose = FALSE, .errorhandling = 'remove',
                        .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
                          
                          
                          single_a <- c(selected_a_eny[i],selected_a_eny[(length(selected_a)-nrow(e_target)+1):length(selected_a_eny)])
                          
                          
                          out <- simulate_one_mcts(rbind(f_players[i],e_target),single_a,
                                                   legal_a = legal_acts,terr_loc=territory,actions = actions,
                                                   c = 1,
                                                   n_iter = 200, depth = 5)  
                          out <- out[-1,]
                          out
                        }
  #hold <- out_single
  
  out_single[,q:=ifelse(q < 0, 0, q)]
  
  out2 <- out_single %>%
    mutate(s = str_sub(s.s,6)) %>%
    group_by(id,s.s) %>%
    nest()
  
  
  #type_chr <- 'e'
  outvar <- foreach(type_chr = c('f','e'),.combine = rbind,.packages = c('data.table','dplyr','DirichletReg'),
                    .inorder = FALSE, .verbose = FALSE,
                    .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
                      
                      if(type_chr == 'f'){
                        
                        
                        out_f <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                                             q=q_work,c =2,
                                             n_iter = 5*7^nrow(units[type == 'f']), depth =4, single_out = out1, actions=actions)
                        out_return <- out_f[[2]]
                        
                        out_return$result <- 'f'
                        
                      } else {
                        

                        
                         #out_single
                        #### Main MCTS ####
                        
                        #units[,a := NULL]
                        out_e <- simulate_mcts(units_eny,selected_a_eny,legal_a = legal_acts,terr_loc=territory, 
                                             q=q_work,c =2,
                                             n_iter = 5*7^nrow(units_eny[type == 'f']), depth =4, single_out = out2, actions=actions)
                        
                        out_return <- out_e[[2]]
                        
                        out_return$result <- 'e'
                      }
                      
                      out_return
                    }                  
  
  
  
  #q_work <- out[[1]]
  (outvar[order(-q)])
  #out[[2]][order(-q)][[1,2]]
  #rep('adj0',nrow(units[type == 'e']))
  
  #### Added this if statement to setup fixed policy for E
  #### To return, leave the adj0 one without if
  
  
  
  #### Determine opponent action!
  
  outf <- outvar[result == 'f']
  oute <- outvar[result == 'e']
  #out_eny[[2]][order(-q)]
  units[,a :=c(outf[order(-q)][[1,2]],oute[order(-q)][[1,2]]) ]
  #units[,a :=c(out[[2]][order(-q)][[1,2]],c('adj6','adj0','adj5')) ]
  
  
  
  move <- legal_acts[units, on = .(s,a)]
  print(move)
  #move[,param := NULL]
  trans <- transition_function2(move[type == 'f'],move[type == 'e'])
  print(trans)
  #print(out[[2]])
  
  selected_a <- c(trans[[1]][str>10 & order(id)]$a,trans[[2]][str>10 & order(id)]$a)
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,s=sp,str,type)]
  units <- units[str>10]
  #print(units)
  
  
  turn <- turn + 1
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  write_csv(units_log, 'mcts_test_two_player_05aprh2.csv')
  timenew <- Sys.time()
  print(timenew-timeout)
  
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



foreach(i = 1:10) %:% 
  
  # Inner loop
  foreach(j = 1:5) %dopar% {
    
    # Code to be executed
    print(paste("i =", i, "j =", j))
  }


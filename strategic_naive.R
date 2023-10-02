source('mcts_functs.R')
#source('mcts_one_funcs.R')
source('initation_script.R')
library(clue)
library(foreach)
library(doParallel)
#### start the thing


# 
# numu <- 3
# nume <- 1
# # 
# f_players <- data.table(
#   id = paste0("inf_",1:numu),
#   s = c('050708','060606','040709'),
#   #s = posf$pos,
#   str = c(40),
# 
#   type = 'f',
#   class = c('inf','inf','inf')
# )
# 
# 
# e_target <- data.table(
#   #id = c('inf_a','inf_b'),
#   id = paste0("eny_",1:nume),
#   s = c('091108'),
#   #s = c('091108','091007','091209','111106','091310','101208'),
#   #s = pose$pos,
#   str = c(100),
# 
#   #sp = c('071009','081008'),
#   type = 'e',
#   class = c('inf')
# )
# f_players <- data.table(
#   id = paste0("inf_",1:numu),
#   s = c('040608','030609','040810','060606','060404','070706'),
#   #s = posf$pos,
#   str = c(100),
# 
#   type = 'f'
# )
# 
# 
# e_target <- data.table(
#   #id = c('inf_a','inf_b'),
#   id = paste0("eny_",1:nume),
#   s = c('091310','101208','091209','111106','091108','091007'),
#   #s = c('091108','091007','091209','111106','091310','101208'),
#   #s = pose$pos,
#   str = c(100,100),
# 
#   #sp = c('071009','081008'),
#   type = 'e'
# )

legal_acts <- data.table(adj_df)

# units <- rbind(f_players,e_target)


units_log <- data.table()
ter_log <- data.table()
turn <- 1

actions <- c(paste0("adj",0:6))

territory <- data.table(df2[,c('pos','x_pos','y_pos')])
territory[,lst := Map(list,x_pos,y_pos)]
q_work_f <- 0
q_work_e <- 0
# 
# key_tern <- data.table(s = c('040507'), value = c(1), type = 'f')

units_eny <- copy(units)

units_eny[,type := ifelse(type == 'e','f','e')]

# ind_q <- q_setup(units, data.table(legal_acts))
# ind_q_eny <- q_setup(units_eny, data.table(legal_acts))

while(max(units[type == 'f']$str) > 30 & max(units[type == 'e']$str) > 30 & turn <= n_t){
  
  time_turn <- Sys.time()
  print(turn)
  

  
  
  units_eny <- copy(units)
  units_eny_full <- copy(units_full)
  eny_tern <- copy(key_tern)
  
  units_eny[,type := ifelse(type == 'e','f','e')]
  units_eny_full[,type := ifelse(type == 'e','f','e')]
  eny_tern[,type := ifelse(type == 'e','f','e')]
  units_eny <- units_eny[order(-type,id)]
  
  ind_q <- q_setup(units_full, data.table(legal_acts))
  ind_q_eny <- q_setup(units_eny_full, data.table(legal_acts))
  
  cl <- makeCluster(2)
  registerDoParallel(cl)
  type_chr = 'f'
  out <- foreach(type_chr = c('f','e'),.combine = c,.packages = c('data.table','dplyr','clue'),
                    .inorder = FALSE, .verbose = FALSE,
                    .export = c('actions_samp','trunc_func','grad_reward')) %dopar% {
                      
                      if(type_chr == 'f'){
                        out <- simulate_mcts(units,ind_q_in = ind_q,legal_a = legal_acts,terr_loc=territory, 
                                             q=q_work_f,c =n_c,n_turns = n_t,
                                             n_iter = n_iter, depth =8,  actions=actions,
                                             k_terr = key_tern, gamma =n_gamma,lanc_df = bel_lanc,turnval = turn,
                                             ind_f = cur_index, ind_e = e_index, afil_val = 'f', opp_val = 'e')
                        
                        out
                        
                      } else {
                        
                        out_eny <- simulate_mcts(units_eny,ind_q_in = ind_q_eny,legal_a = legal_acts,terr_loc=territory, 
                                                 q=q_work_e,c =n_c,n_turns = n_t,
                                                 n_iter = n_iter, depth =8,  actions=actions,
                                                 k_terr = eny_tern, gamma = n_gamma,lanc_df = bel_lanc,turnval = turn,
                                                 ind_f = cur_index, ind_e = e_index, afil_val = 'e', opp_val = 'f')
                        out_eny
                      }
                      
                    }

  stopCluster(cl)
  registerDoSEQ()
  
  out_lst <- out[[5]]
  # q_work_f <- out[[5]]
  # q_work_e <- out[[10]]
  ind_q <- out[[5]]
  
  out_lst[['101107']]
  act_vec_f <- vector()
  units_select <- data.table()
#units
  
  
  for(i in 1:nrow(units[type == 'f']) ) {
    #print(units[type == 'f'][[i,2]])
    unitsel <- units[type == 'f'][i]
    
    act <- out_lst[[unitsel$s]][[unitsel$id]]
    units_select <- rbind(units_select,act)
  }
  #print(units_select)
  wide_dt <- dcast(units_select, id ~ sp, value.var = "q")
  #print(wide_dt)
  units_f <- units_select
  # Replace NA values with -Inf
  wide_dt[is.na(wide_dt)] <- -100
  # print(wide_dt)
  # Remove the 'id' column and convert the data.table to a matrix
  cost_matrix <- as.matrix(wide_dt[, -"id"])
  
  cost_matrix <- cost_matrix - 100
  
  # print(-cost_matrix)
  # Solve the assignment problem
  assignment <- solve_LSAP(-cost_matrix)
  
  # Create a final data.table based on the optimal assignment
  opt_move <- data.table(id = wide_dt$id, sp = colnames(cost_matrix)[assignment], qn_sum = cost_matrix[cbind(seq_len(nrow(cost_matrix)), assignment)])
  
  print(opt_move)
  units_select <- units_select[opt_move, on = .(id, sp)]
  
  
  act_vec_f <- units_select[order(id)]$a

  
  
  out_lst_e <- out[[10]]
  ind_q_eny <- out[[10]]
  # out_lst_e <- out_eny[[5]]
  # ind_q_eny <- out_eny[[5]]
  
  out_lst_e[['141810']]#$eny_4
  act_vec_e <- vector()
  units_select_e <- data.table()
  
  for(i in 1:nrow(units_eny[type == 'f']) ) {
    
    unitsel <- units_eny[type=='f'][i]
    
    act <- out_lst_e[[unitsel$s]][[unitsel$id]]
    
    units_select_e <- rbind(units_select_e,act)
  }
  
  
  wide_dt <- dcast(units_select_e, id ~ sp, value.var = "q")
  
  # Replace NA values with -Inf
  wide_dt[is.na(wide_dt)] <- -100
  # print(wide_dt)
  # Remove the 'id' column and convert the data.table to a matrix
  cost_matrix <- as.matrix(wide_dt[, -"id"])
  
  cost_matrix <- cost_matrix - 100
  
  # print(-cost_matrix)
  # Solve the assignment problem
  assignment <- solve_LSAP(-cost_matrix)
  
  # Create a final data.table based on the optimal assignment
  opt_move <- data.table(id = wide_dt$id, sp = colnames(cost_matrix)[assignment], qn_sum = cost_matrix[cbind(seq_len(nrow(cost_matrix)), assignment)])
  
  #print(opt_move)
  units_select_e <- units_select_e[opt_move, on = .(id, sp)]
  
  
  
  act_vec_e <- units_select_e[order(id)]$a
  units[order(-type,id)]
  units[order(-id),a:= 'adj0']
  units[order(-type,id),a :=c(act_vec_f,act_vec_e)]
  #units[,a :=c(out[[2]][order(-q)][[1,2]],c('adj6','adj0','adj5')) ]
  
  
  
  move <- legal_acts[units, on = .(s,a)]
  #print(move)
  #move[,param := NULL]
  trans <- transition_function2(move[type == 'f'],move[type == 'e'],key_terrain = key_tern,lanc = u_lanchester)
  #print(trans)
  #print(out[[2]])
  
  key_tern <- trans[[6]]
  
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,class,s=sp,str,type)]
  
  ### Remove lost units
  
  units <- units[str>30]
  
  ### Add reinforcements
  
  
  for(i in 1:dim(rein_mat)[2]){
    print(i)
    if(rein_mat[turn,i]>0){
      
      for(j in 1:rein_mat[turn,i]){
        
        new_u <- data.frame(class = rein_class[i], type = rein_type[i],str = 100)
        
        if(rein_type[i] == 'f' & key_tern[s == '081210']$type == 'f'){
          
          if(length(f_rein_areas_fwd[!f_rein_areas_fwd %in% units$s]) > 0){
            
            new_u$s <- f_rein_areas_fwd[!f_rein_areas_fwd %in% units$s][1]
            new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
            cur_index <- cur_index + 1
          } else {
            
            new_u$s <- territory$pos[!territory$pos %in% units$s][1]
            new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
            cur_index <- cur_index + 1
          }
          
        } else if(rein_type[i] == 'f' & key_tern[s == '081210']$type == 'f'){
          
          if(length(f_rein_areas_rear[!f_rein_areas_rear %in% units$s]) > 0 ){
            
            new_u$s <- f_rein_areas_rear[!f_rein_areas_rear %in% units$s][1]
            new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
            cur_index <- cur_index + 1
            
          } else {
            
            new_u$s <- territory$pos[!territory$pos %in% units$s][1]
            new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
            cur_index <- cur_index + 1
          }
          
        } else if(rein_type[i] == 'e') {
          
          if(length(e_rein_areas[!e_rein_areas %in% units$s])>0){
            new_u$s <- e_rein_areas[!e_rein_areas %in% units$s][1]
            
            new_u$id <- paste(new_u$class,new_u$type,e_index,sep = "_")
            e_index <- e_index + 1
          } else {
            
            new_u$s <- rev(territory$pos[!territory$pos %in% units$s][1])
            
            new_u$id <- paste(new_u$class,new_u$type,e_index,sep = "_")
            e_index <- e_index + 1
          }
          
        }
        units <- rbind(units,new_u)
        print(new_u)
      }
      
    }
  }
  
  
  
  

  #units_log[,bg := NULL]
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  ter_log <- cbind(rbind(ter_log,cbind(trans[[6]],turn),cbind(trans[[6]],turn)))
  
  
  turn <- turn + 1
  write_csv(units_log, paste0('results/outcome/strat_unit_',file_name))
  write_csv(ter_log, paste0('results/outcome/strat_terr_',file_name))
  
  
  print(paste("Turn time:",lubridate::as.duration(Sys.time()-time_turn)))
  
}


source('MCTS_naive/mcts_functs.R')




#### start the thing

# Specify the package name
package_name <- "clue"

# Check if the package is already installed
if (!requireNamespace(package_name, quietly = TRUE)) {
  # Install the package
  install.packages(package_name, repos = 'https://cran.wustl.edu/')
}

# Load the package
library(package_name, character.only = TRUE)
library(foreach)
library(doParallel)


numu <- 3
nume <- 1
# 
f_players <- data.table(
  id = paste0("inf_",1:numu),
  s = c('050708','060606','040709'),
  #s = posf$pos,
  str = c(40),

  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('091108'),
  #s = c('091108','091007','091209','111106','091310','101208'),
  #s = pose$pos,
  str = c(100),

  #sp = c('071009','081008'),
  type = 'e'
)

# Generate a unique identifier with a random component
unique_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), sample(1:100000, 1))

# Use the unique identifier in your file name
file_name <- paste0("process_updated_", unique_id, ".csv")


legal_acts <- data.table(adj_df)

units <- rbind(f_players,e_target)


units_log <- data.table()
turn <- 0

actions <- c(paste0("adj",0:6))

territory <- data.table(df2[,c('pos','x_pos','y_pos')])
territory[,lst := Map(list,x_pos,y_pos)]
q_work1 <- 0


key_tern <- data.table(s = c('040507'), value = c(1), type = 'f')

units_eny <- copy(units)
units_eny[,type := ifelse(type == 'e','f','e')]

# ind_q <- q_setup(units, data.table(legal_acts))
# ind_q_eny <- q_setup(units_eny, data.table(legal_acts))


while(max(units[type == 'f']$str) > 10 & max(units[type == 'e']$str) > 10 & turn <= 25){
  
  time_turn <- Sys.time()
  print(turn)
  
  
  
  
  units_eny <- copy(units)
  eny_tern <- copy(key_tern)
  
  units_eny[,type := ifelse(type == 'e','f','e')]
  eny_tern[,type := ifelse(type == 'e','f','e')]
  units_eny <- units_eny[order(-type,id)]
  
  ind_q <- q_setup(units, data.table(legal_acts))
  ind_q_eny <- q_setup(units_eny, data.table(legal_acts))
  
  cl <- makeCluster(2)
  registerDoParallel(cl)
  #type_chr = 'f'
  out <- foreach(type_chr = c('f','e'),.combine = c,.packages = c('data.table','dplyr','clue'),
                 .inorder = FALSE, .verbose = FALSE,
                 .export = c('actions_samp','trunc_func','grad_reward')) %dopar% {
                   
                   if(type_chr == 'f'){
                     out <- simulate_mcts(units,ind_q_in = ind_q,legal_a = legal_acts,terr_loc=territory, 
                                          q=q_work_f,c =1.5,
                                          n_iter = 1000, depth =8,  actions=actions,
                                          k_terr = key_tern, gamma =0.95)
                     
                     out
                     
                   } else {
                     
                     out_eny <- simulate_mcts(units_eny,ind_q_in = ind_q_eny,legal_a = legal_acts,terr_loc=territory, 
                                              q=q_work_e,c =1.5,
                                              n_iter = 1000, depth =8,  actions=actions,
                                              k_terr = eny_tern, gamma = 0.95)
                     out_eny
                   }
                   
                 }
  
  stopCluster(cl)
  registerDoSEQ()
  
  out_lst <- out[[5]]
  # q_work_f <- out[[5]]
  # q_work_e <- out[[10]]
  ind_q <- out[[5]]
  
  #out_lst[['050607']]
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
  
  #print(opt_move)
  units_select <- units_select[opt_move, on = .(id, sp)]
  
  
  act_vec_f <- units_select$a
  
  
  
  out_lst_e <- out[[10]]
  ind_q_eny <- out[[10]]
  
  out_lst_e[['070807']]#$eny_4
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
  
  
  
  act_vec_e <- units_select_e$a
  
  units[,a:= 'adj0']
  units[,a :=c(act_vec_f,act_vec_e)]
  #units[,a :=c(out[[2]][order(-q)][[1,2]],c('adj6','adj0','adj5')) ]
  
  
  
  move <- legal_acts[units, on = .(s,a)]
  #print(move)
  #move[,param := NULL]
  trans <- transition_function2(move[type == 'f'],move[type == 'e'],key_terrain = key_tern)
  #print(trans)
  #print(out[[2]])
  
  key_tern <- trans[[6]]
  
  units <- rbind(trans[[1]][order(id)],trans[[2]][order(id)])[,list(id,s=sp,str,type)]
  units <- units[str>10]
  #print(units)
  
  
  turn <- turn + 1
  #units_log[,bg := NULL]
  units_log <- cbind(rbind(units_log,cbind(trans[[1]],turn),cbind(trans[[2]],turn)))
  write_csv(units_log, paste0('results/trial_3on1/',file_name))
  print(paste("Turn time:",lubridate::as.duration(Sys.time()-time_turn)))
  
}


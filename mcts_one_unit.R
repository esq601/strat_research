
source('mcts_functs.R')
source('mcts_one_funcs.R')


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

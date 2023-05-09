source('mcts_functs.R')
source('mcts_one_funcs.R')

#### start the thing

single_a <- c(selected_a[2],selected_a[(length(selected_a)-nrow(e_target)+1):length(selected_a)])

test <- simulate_one_mcts(rbind(f_players[2],e_target),single_a,
                  legal_a = legal_acts,terr_loc=territory,actions = actions,
                  c = 20,
                  n_iter = 100, depth = 6)  


test <- data.table(x = c(10,20,30,40,50))

which(test$x > 20)





players <- data.table(id = c('inf1','inf2','inf3'),s = c('060808','050809','050809'), 
                      sp = c('060808','060909','060909'))
target <- data.table(id = c('eny1','eny2'),s = c('070908','070908'), sp = c('060808','060909'))


as.numeric(players$s) - as.numeric(players$sp)
target
conf_check(players,target)

target$sp %in% players$sp

players[target , on = .(sp == s , s == sp),nomatch = 0]
players[target, on = 'sp', nomatch = 0, allow.cartesian = TRUE]
players[target , on = .(sp == s ),nomatch = 0]
target[players , on = .(sp == s ),nomatch = 0]



#### Debug transition


test <- data.table(read_csv("mcts_test_two_player_05aprh.csv"))

testturn <- test[turn == 7]

players <- testturn[type =='f',c('id','s','a','str')]
players <- legal_acts[players, on = .(s,a)]
players <- players[,c('id','s','sp','str')]

target <- testturn[type =='e',c('id','s','a','str')]
target <- legal_acts[target, on = .(s,a)]
target <- target[,c('id','s','sp','str')]



players <- data.table(id = c('inf1','inf2'),s = c('060707','040507'),
                      sp = c('060707','050607'),str = c(32,100))
target <- data.table(id = c('eny1','eny2'),s = c('070807','060808'), sp = c('060808','060707'),
                     str = c(100,100))

terr <- data.table(s = c('060707'),value = c(1), type = c('f'))

nrow(conf_check(players,target))

test <- conf_check2(players,target)
test

trans2 <- transition_function2(players,target, key_terrain = terr)
trans2
gradrew <- sum(trans2[[6]][type == 'f']$value) -sum(trans2[[6]][type == 'e']$value)
reward_new(trans2,gradrew)

test <- paste0(players$s,players$sp)
teste <- paste0(target$sp,target$s)
players[paste0(s,sp)%in%paste0(target$sp,target$s),s:=sp]
test %in% teste
test <- data.table(id = c('inf1','eny2'),sp = c('110904'),str = c(90,100))
test[, .SD[sample(.N, 1, prob = str)], by = sp]

test2 <- reward_conf(trans)
test2
test1[[2]]$val + .95*test2[[2]]$val

as.numeric(players$s) - as.numeric(players$sp)
target
conf_check(players,target)

target$sp %in% players$sp

players[target , on = .(sp == s , s == sp),nomatch = 0]
players[target, on = 'sp', nomatch = 0, allow.cartesian = TRUE]
players[target , on = .(sp == s ),nomatch = 0]
target[players , on = .(sp == s ),nomatch = 0]

data.table(id = trans[[1]]$id, val = ifelse(trans[[1]]$id %in% trans[[4]]$player,1,0))

library(data.table)

# create the first data.table object
dt1 <- data.table(id = 1:5, x = c(NA, 2, 3, NA, 5))

# create the second data.table object
dt2 <- data.table(id = c(2, 3, 4, 6), y = c(2, 3, 4, 6))

# join the two data.table objects and replace missing values with 0
dt_join <- merge(dt1, dt2, by = "id", all = TRUE, fill = 0)

# print the joined data.table object
print(dt_join)

str(test2[[2]])
test <- data.table::merge.data.table(test1[[2]],test2[[2]], by = "id", all = TRUE,  suffixes = c('_new','_old'))
test[is.na(test)] <- 0

test
data.table::merge.data.table()


actions[actions != 'adj0']



##### Testing the simulation part ####

numu <- 3
nume <- 1


f_players <- data.table(
  id = paste0("inf_",1:numu),
  s = c('080806','080907','070807'),
  #s = posf$pos,
  str = c(100,100,80),
  
  type = 'f'
)


e_target <- data.table(
  #id = c('inf_a','inf_b'),
  id = paste0("eny_",1:nume),
  s = c('131306'),
  #s = pose$pos,
  str = c(100),
  
  #sp = c('071009','081008'),
  type = 'e'
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


legal_acts <- data.table(adj_df)
legal_acts[,param := list(c(1,1))]
print(turn)
print(nrow(q_work$sa))

#### Subfind ####

f_players <- units[type == 'f']
e_target <- units[type == 'e']


out_single <- foreach(i=1:nrow(f_players), .combine = rbind,.packages = c('data.table','dplyr'),
                      .inorder = FALSE, .verbose = TRUE, .errorhandling = 'remove',
                      .export = c('actions_samp','yes_fun','trunc_func','grad_reward')) %dopar% {
                        
                        
                        single_a <- c(selected_a[i],selected_a[(length(selected_a)-nrow(e_target)+1):length(selected_a)])
                        
                        
                        out <- simulate_one_mcts(rbind(f_players[i],e_target),single_a,
                                                 legal_a = legal_acts,terr_loc=territory,actions = actions,
                                                 c = .75,
                                                 n_iter = 50, depth = 8)  
                        out <- out[-1,]
                        out
                      }
#hold <- out_single

out_single[,q:=ifelse(q < 0, 0, q)]


out1 <- out_single %>%
  mutate(s = str_sub(s.s,6)) %>%
  group_by(id,s.s) %>%
  nest()

#### Main MCTS ####

#units[,a := NULL]
out <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                     q=q_work,c =1,
                     n_iter = 15*7^nrow(units[type == 'f']), depth =5, single_out = out1, actions=actions)


test <- data.table(i = c(1,2,3,4), j = c('a','b','c','d'))

for(i in 1:4){
  print(test[i])
}





prob_dt <- prob_setup()
leg_dt <- data.table(adj_df)

prob_ls <- split(prob_dt, by = 'a')
leg_ls <- split(data.table(leg_dt), by = 's')

state <- '141608'
start_a <- c('adj0','adj6','adj4','adj5')
dt_out <- data.table()
for(a in start_a){
  for(i in 1:1000){
    new_state <- state
    a
    for (depth in 1:10){
      
      lst_legal <- lapply(prob_ls, function(dt) {
        dt[dt$nexta %in% leg_ls[[new_state]]$a, ]
      })
      
      new_a <- belfun(a,lst_legal)
      
      new_state <- leg_ls[[new_state]][a == new_a]$sp
      dt_out <- rbind(dt_out,data.table(starta  = a, newstate = new_state, newa = new_a))
    }
  }
}


count_dt <- dt_out[,.N,by = c('starta','newstate')]

count_dt[is.na(N),N :=0]

hexdf2a <- hexdf2 %>%
  left_join(count_dt, by = c('pos'='newstate')) %>%
  as.data.table()
#hexdf2a[is.na(N),N:=0]
hexdf3 <- data.table()
for(a in start_a){
  newhex <- hexdf2
  newhex$starta <- a
  hexdf3 <- rbind(hexdf3,newhex)
}

direcst <- c('Stationary','Northwest','West','Southwest')
names(direcst) <- c('adj0','adj4','adj5','adj6')

ggplot(hexdf2a[!is.na(starta)], aes (x=x_h, y = y_h)) +
  geom_polygon(data = hexdf3, color = 'black',aes(group = pos),fill = 'grey90') +
  
  #annotate('polygon', color = 'black', group = pos) +
  geom_polygon(color = 'black',aes(group = pos, fill = log(N))) +
geom_point(data = hexdf3[pos == state], aes(x = x_pos, y = y_pos), size = 3) +
  #geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  coord_equal() +
  scale_fill_distiller(palette = 14, direction = 1) +
  theme_void() +
  labs(
    title = "Distribution of Movement Belief",
    subtitle = 'Determined by Previous Movement Direction'
    ) +
  facet_wrap(~starta,
             labeller = labeller(starta = direcst ))


ggsave('images/belief_dist.jpeg',width = 10, height = 6, dpi = 320)
unique(hexdf2a$starta)



##### New Belief function #####

dttest <- data.table(s = c('051011','060909'),lasta = c('adj4','adj6'))

a
dttest[2,]$lasta <- 'boobs'
probs <- prob_setup()
legal_acts


probsplit <- split(probs,by = 'a')
legsplit <- split(legal_acts, by = 's')

probtest <- probsplit[[dttest[[1,2]]]][nexta %in% legsplit[[dttest[[1,1]]]]$a]

sample(probtest[[2]],1,prob = probtest[[3]])


belfun2 <- function(df,plist,leg_list){
  probtest <- probsplit[[df[[2]]]][nexta %in% legsplit[[df[[1]]]][[2]]]
  
  sample(probtest[[2]],1,prob = probtest[[3]])
}

selfun2 <- function(state,prob,legal_a,rand){
  
  if(is.null(prob[[state[[6]]]])==TRUE | sum(prob[[state[[6]]]]$q) == 0 | rand == TRUE){
    
    sample(legal_a[[state[[2]]]]$a,1)
    
  } else{
    
    sample(prob[[state[[6]]]]$a,1,replace = TRUE,prob =prob[[state[[6]]]]$n )
    
  }
}
belfun2(dttest[2],probsplit,legsplit)       

apply(dttest,FUN = belfun2,MARGIN = 1,plist = probsplit,leg_list = legsplit)

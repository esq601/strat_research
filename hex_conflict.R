# players <- data.table(
#   id = c('inf_1','inf_2','inf_3'),
#   s = c('060505','060606','090401'),
#   str = c(1,1,1)
# )
# 
# 
# target <- data.table(
#   id = c('inf_a','inf_b'),
#   s = c('111005','081210'),
#   str = c(1,1)
# )
# 
# target$s
# players$s
# 
# play_test <- players[,sp := c('111005','111005','081211')]
# tgt_test <- target[,sp := c('111005','081210')]

conflict <- function(conflicts,pl, tg) {
  #players <- players[s == sp, type := "d"]
  #players <- players[s != sp, type := "a"]
  #print(conflicts)
  #print(targets)
  #if(any(players$s %in% targets$sp) & any(targets$s %in% players$sp)){
  #   players <- players[s %in% targets$sp | sp %in% targets$s,c('str_old','str') := .(str, str*.25)]
  #   targets <- targets[s %in% players$sp | sp %in% players$s,c('str_old','str') := .(str, str*.25)]
  #   
  # #} else{
  #   players1 <- players[,by = sp, .(dirs = uniqueN(s),n = sum(str),type = 'f')]
  #   #targets <- targets[s == sp, type := "d"]
  #   #targets <- targets[s != sp, type := "a"]
  #   targets1 <- targets[,by = sp, .(dirs = uniqueN(s),n = sum(str),type = 'e')]
  #   #print(players1)
  #   #print(targets1)
  #   
  #   tot <- merge(players1,targets1,all = TRUE, by = 'sp')
  #   
  #   tot <- tot[,c('mod_x','mod_y') := .(n.x^dirs.x /(n.x^dirs.x + n.y^dirs.y),
  #                                       n.y^dirs.y /(n.x^dirs.x + n.y^dirs.y))]
  #   #print(tot)
  #   players <- players[tot,list(id,s,str,sp,mod_x), on = "sp"]
  #   #print(players)
  #   players <- players[,c('str_old','str') := .(str, str*mod_x)]
  #   players <- players[, mod_x := NULL]
  #   targets <- targets[tot,list(id,s,str,sp,mod_y), on = "sp"]
  #   targets <- targets[,c('str_old','str') := .(str, str*mod_y)]
  #   targets <- targets[, mod_y := NULL]
    #print(as.character(conflicts$player))
    
    ppower <- conflicts[,by= player, .(engs_plr =uniqueN(target))]
    # print(ppower)
    # print(pl)
    ppower[pl, on = 'player == id', atk_plr := str/engs_plr]
    #ppower
    tpower <- conflicts[,by= target, .(engs_tgt =uniqueN(player))]
    tpower[tg, on = 'target == id', atk_tgt := str/engs_tgt]
    #tpower
    
    fight_tbl <- conflicts[tpower, on = "target"]
    fight_tbl <- fight_tbl[ppower, on = 'player']
    #print(fight_tbl)
    
    players <- fight_tbl[, by = player, .(mod = .75^(sum(atk_tgt)/mean(engs_tgt)))]
    targets <- fight_tbl[, by = target, .(mod = .75^(sum(atk_plr)/mean(engs_plr)))]
  
  
  #players <- players[str < .1, str := 0]

  
  #targets <- targets[str < .1, str := 0]

  

    #print('through conflict')
  list(players,targets)
}


# .75^(.5/2)
# .75^()

# 
#  test <- conflict(play_test[sp %in% tgt_test$s], tgt_test)
#  test

reward_conf <- function(conf_out){
  
  slf <- ifelse(nrow(conf_out[[1]][str>0]) >0,
                sum((conf_out[[1]][str>0]$str_old - conf_out[[1]][str>0]$str)),
                0)
  
  eny <- ifelse(nrow(conf_out[[2]][str>0]) >0,
                sum((conf_out[[2]][str>0]$str_old - conf_out[[2]][str>0]$str)),
                0)
  
  # print(c(slf,eny))
  # print(conf_out[[2]])
  
  cbts <- nrow(conf_out[[1]]) + nrow(conf_out[[2]])
  
  (200*eny - 150*slf)/cbts +50*nrow(conf_out[[2]][str<0.1]) - 15*nrow(conf_out[[1]][str<0.1])
}

# 
# test <- list(trans[[1]],trans[[2]])
# 
# trans[[1]][,str := c(.45,1)] 
# trans[[2]][,str := c(.04)]
# test <- list(trans[[1]],trans[[2]])
# 
# #(test[[1]][abs(str-str_old)>0])
# test
# #mean((test[[1]][str>0]$str_old - test[[1]][str>0]$str)) / nrow(test[[1]][abs(str-str_old)>0])
# 
# reward_conf(test)
# # reward_conf(test)

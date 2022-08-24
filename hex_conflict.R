players <- data.table(
  id = c('inf_1','inf_2','inf_3'),
  s = c('060505','060606','090401'),
  str = c(.2,.2,1)
)


target <- data.table(
  id = c('inf_a','inf_b'),
  s = c('111005','081210'),
  str = c(1,1)
)

target$s
players$s

play_test <- players[,sp := c('111005','111005','081210')]
tgt_test <- target[,sp := c('111005','081210')]

conflict <- function(players, targets) {
  #players <- players[s == sp, type := "d"]
  #players <- players[s != sp, type := "a"]
  players1 <- players[,by = sp, .(dirs = uniqueN(s),n = sum(str))]
  #targets <- targets[s == sp, type := "d"]
  #targets <- targets[s != sp, type := "a"]
  targets1 <- targets[,by = sp, .(dirs = uniqueN(s),n = sum(str))]
  
  
  tot <- merge(players1,targets1,all = TRUE, by = 'sp')
  
  tot <- tot[,c('mod_x','mod_y') := .(n.x^dirs.x /(n.x^dirs.x + n.y^dirs.y),
                                   n.y^dirs.y /(n.x^dirs.x + n.y^dirs.y))]
  #print(tot)
  players <- players[tot,list(id,s,str,sp,mod_x), on = "sp"]
  #print(players)
  players <- players[,c('str_old','str') := .(str, str*mod_x)]
  players <- players[str < .1, str := 0]
  players <- players[, mod_x := NULL]
  
  targets <- targets[tot,list(id,s,str,sp,mod_y), on = "sp"]
  targets <- targets[,c('str_old','str') := .(str, str*mod_y)]
  targets <- targets[str < .1, str := 0]
  targets <- targets[, mod_y := NULL]
  list(players,targets,tot)
}


test <- conflict(play_test, tgt_test)
test

reward_conf <- function(conf_out){
  slf <- sum((conf_out[[1]][str>0]$str_old - conf_out[[1]][str>0]$str)) / nrow(conf_out[[1]][str>0])
  eny <- sum((conf_out[[2]][str>0]$str_old - conf_out[[2]][str>0]$str))/ nrow(conf_out[[2]][str>0])
  #print(c(slf,eny))
  10*eny - 5*slf +25*nrow(conf_out[[2]][str==0])
}

reward_conf(test)

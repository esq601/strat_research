
conflict <- function(conflicts,pl, tg,fmod,tmod,fexp,texp) {
  
    # These need to be variables in the function
    fmod <- .15
    emod <- .24
    fexp <- 0.9
    texp <- 0.8
    
    ppower <- conflicts[,by= player, .(engs_plr =uniqueN(target))]
    
    ppower[pl, on = 'player == id', atk_plr := str/engs_plr]
    
    tpower <- conflicts[,by= target, .(engs_tgt =uniqueN(player))]
    tpower[tg, on = 'target == id', atk_tgt := str/engs_tgt]
    
    fight_tbl <- conflicts[tpower, on = "target"]
    fight_tbl <- fight_tbl[ppower, on = 'player']
    # if(nrow(fight_tbl)>1){
    #   print(fight_tbl)
    #   
    # }
    players <- fight_tbl[, by = player, .(mod = floor(fmod*(sum(atk_tgt)^fexp * sum(engs_tgt)^texp)))]
    targets <- fight_tbl[, by = target, .(mod = floor(emod*(sum(atk_plr)^texp * sum(engs_plr)^fexp)))]
    
    #print(players)
    #print(targets)
  list(players,targets)
}


reward_conf <- function(conf_out){
  
  slf <- ifelse(nrow(conf_out[[1]][str>0]) >0,
                sum((conf_out[[1]][str>0]$str_old - conf_out[[1]][str>0]$str)),
                0)
  
  eny <- ifelse(nrow(conf_out[[2]][str>0]) >0,
                sum((conf_out[[2]][str>0]$str_old - conf_out[[2]][str>0]$str)),
                0)
  
  cbts <- nrow(conf_out[[1]]) + nrow(conf_out[[2]])
  
  rew <- (3*eny - 1.6*slf)/cbts +10*nrow(conf_out[[2]][str<10]) - 5*nrow(conf_out[[1]][str<10])
  #print(rew)
  return(rew)
}

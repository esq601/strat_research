
conflict <- function(conflicts,pl, tg,fmod,tmod,fexp,texp) {
  
    # These need to be variables in the function
    fmod <- .15
    emod <- .14
    fexp <- 0.9
    texp <- 0.8
    #print(conflicts)
    ppower <- conflicts[,by= player, .(engs_plr =uniqueN(target))]
    
    ppower[pl, on = 'player == id', atk_plr := str/engs_plr]
    #print(ppower)
    tpower <- conflicts[,by= target, .(engs_tgt =uniqueN(player))]
    tpower[tg, on = 'target == id', atk_tgt := str/engs_tgt]
    
    fight_tbl <- conflicts[tpower, on = "target"]
    #print(fight_tbl)
    fight_tbl <- fight_tbl[ppower, on = 'player']
    #print(fight_tbl)
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
  
  targets <- conf_out[[4]][,.N, by = target]
  
  target_new <- merge(conf_out[[2]], targets, by.x = 'id',by.y = 'target',)
  
  
  target_new[which(is.na(target_new)),N := 1]
  
  target_new[, rewval := N * (str_old - str)]
  
  slf <- ifelse(nrow(conf_out[[1]]) >0,
                sum((conf_out[[1]]$str_old - conf_out[[1]]$str)),
                0)
  
  eny <- ifelse(nrow(conf_out[[2]]) >0,
                sum(target_new$rewval),
                0)
  
  cbts <- nrow(conf_out[[4]])
   #print(conf_out)
  #print(slf)
  # print(eny)
   # print(cbts)
  # print(target_new)
  rew <- (eny - slf) + (10*cbts)*nrow(conf_out[[2]][str<10]) - 5*nrow(conf_out[[1]][str<10]) + 5*cbts
  #print(rew)
  return(rew)

  }

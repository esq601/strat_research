
conflict <- function(conflicts,pl, tg,fmod,tmod,fexp,texp) {
  
    # These need to be variables in the function
    fmod <- .25
    emod <- .25
    fexp <- 1
    texp <- 1
      
      # print(conflicts)
    ppower <- conflicts[,by= player, .(engs_plr =uniqueN(target))]
    
    ppower[pl, on = 'player == id', atk_plr := str/engs_plr]
    # print(ppower)
    tpower <- conflicts[,by= target, .(engs_tgt =uniqueN(player))]
    tpower[tg, on = 'target == id', atk_tgt := str/engs_tgt]
    
    
    
    fight_tbl <- conflicts[tpower, on = "target"]
    #print(fight_tbl)
    fight_tbl <- fight_tbl[ppower, on = 'player']
    
    fight_tbl$plr_mod <- fight_tbl$atk_tgt*emod
    fight_tbl$tgt_mod <- fight_tbl$atk_plr*fmod
    # if(nrow(fight_tbl)>1){
    #   print(fight_tbl)
    #   
    # }
    
    # print(fight_tbl)
    #floor(fmod*(sum(atk_tgt)^fexp * sum(engs_tgt)^texp)),mod2 =
    players <- fight_tbl[, by = player, .(mod =  floor(sum(plr_mod)))]
    targets <- fight_tbl[, by = target, .(mod =floor(sum(tgt_mod)))]
    
      #print(players)
      #print(targets)
  list(players,targets)
}


reward_conf <- function(conf_out, fight_wt = 0.25, terr_wt = 0.75){
  
  rew <- fight_wt*((sum(conf_out[[2]]$str_old) - sum(conf_out[[2]]$str))/sum(conf_out[[2]]$str_old))  #+
  #nrow(conf_out[[2]][str<10])/nrow(conf_out[[2]]) #+
  
  #print(conf_out[[7]])
  rew_ind <- data.table(id = conf_out[[1]][order(id)]$id,s = conf_out[[1]][order(id)]$s,
                        a = conf_out[[1]][order(id)]$a,
                        val = terr_wt*conf_out[[7]])
  
  rew_ind[id %in% conf_out[[4]]$player, val := val + rew]
  
  return(list(rew,rew_ind))
  
}



conflict <- function(conflicts,pl, tg,fmod,tmod,fexp,texp) {
  
    # These need to be variables in the function
    fmod <- .25
    emod <- .25
    fexp <- 1
    texp <- 1
      
    #print(conflicts)
    ppower <- conflicts[,by= player, .(engs_plr =uniqueN(target))]
    
    ppower[pl, on = 'player == id', atk_plr := str/engs_plr]
    # print(ppower)
    tpower <- conflicts[,by= target, .(engs_tgt =uniqueN(player))]
    tpower[tg, on = 'target == id', atk_tgt := str/engs_tgt]
    
    
    
    fight_tbl <- conflicts[tpower, on = "target"]
    print(fight_tbl)
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

normalize_ratio <- function(f, e, cap = 10) {
  r <- ifelse(e == 0, ifelse(f > 0, cap, 0), f/e)
  return(pmin(r, cap) / cap)
}

reward_conf <- function(conf_out, fight_wt = 0.25, terr_wt = 0.75){
  

  #print(conf_out)
  #rew <- fight_wt*((sum(conf_out[[2]]$str_old) - sum(conf_out[[2]]$str))/sum(conf_out[[2]]$str_old))  #+
  
  
  rew_ind <- data.table(id = conf_out[[1]][order(id)]$id,s = conf_out[[1]][order(id)]$s,
                        a = conf_out[[1]][order(id)]$a,
                        val = terr_wt*conf_out[[7]])
  
  

  grouped_dt <- conf_out[[4]][[1]][, .(f_str_sum = sum(f_str), e_str_sum = sum(e_str)), by = "player"
  ][, ratio_norm := normalize_ratio(f_str_sum, e_str_sum)
  ][, .(player, ratio_norm)]
  
  # Rename 'player' column to 'id' in grouped_dt
  setnames(grouped_dt, old = "player", new = "id")
  # Join the data.tables on 'player'
  rew_ind <- merge(rew_ind, grouped_dt, by = "id", all.x = TRUE)
  
  # Replace NA values with 0 in the ratio_norm column
  set(rew_ind, which(is.na(rew_ind$ratio_norm)), j = "ratio_norm", value = 0)
  
  #print(joined_dt)
  #nrow(conf_out[[2]][str<10])/nrow(conf_out[[2]]) #+
  # print(rew)
  # print(conf_out[[4]])
  #print(conf_out[[7]])
  # if(conf_out[[2]]$str < 30){
  #   print(conf_out[[2]])
  #   print(rew)
  # }
  

  

  
  rew_ind[id %in% conf_out[[4]][[3]]$player, val := val + ratio_norm]
  
  rew_ind[,ratio_norm := NULL]
  
  print(rew_ind)
  # if(conf_out[[2]]$str < 30){
  #   #(conf_out[[2]])
  #   print(rew_ind)
  # }
  # print(conf_out[[4]][[3]])
  # print(conf_out[[2]])
   #print(rew_ind)
  return(rew_ind)
  
}


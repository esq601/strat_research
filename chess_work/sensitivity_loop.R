


b_win <- chessout %>%
  mutate(score = case_when(
    game_res == "TRUE" ~ 0,
    game_res == "FALSE" ~ 1,
    game_res == "tie" ~ 0
  )) %>%
  group_by(b_fct) %>%
  summarise(pct =sum(score)/n()) %>%
  ungroup() %>%
  mutate(scaled = pct / sum(pct))


b4kprob <- c(.9,.1)



strat_dec <- data.frame(piece_sel = c('B4K','B4K','Standard','MK','MK','MK','MK','MK'),
                        dec = c('d1','d2','d3','d4','d4','d5','d6','d6'),
                        prob = c(b4kprob,1,.5,.5,.3,.2,.2),
                        piece_place = c('none','none','none','center','flank','none','center','flank'),
                        outcome = c('PPPPPPPP/RBBQKBBR','PPPPPPPP/RNBQKBNR','PPPPPPPP/RNBQKBNR',
                                    'P1PNPP1P/RNBQKBNR','N1PPPP1P/RNBQKBNR','PPPPPPPP/RNBQKBNR',
                                    'P2NN2P/RNBQKBNR','N2PP2N/RNBQKBNR'))

probs <- chessout %>%
  group_by(w_fct,b_fct,type,move,game_res) %>%
  summarise(num = n()) %>%
  group_by(w_fct,b_fct,type,move) %>%
  mutate(total = sum(num)) %>%
  mutate(pct = num/total) %>%
  mutate(points = case_when(
    game_res == "TRUE" ~ 950,
    game_res == "FALSE" ~ -1000,
    game_res == "tie" ~ -50
  ))

# opp_prob <- b_win %>%
#   select(b_fct,piece_prob = scaled)

# opp_prob <- b_win %>%
#   select(b_fct) %>%
#   mutate(piece_prob = rep((1/6),6))
# val <- (1-i)/5
# 
opp_prob <- b_win %>%
  select(b_fct) %>%
  mutate(piece_prob = c(0,0,0,0,0,1))



dfout <- data.frame()

for(i in seq(from = 0, to= 1, by = .025)){
  print(i)
  
  for(j in seq(from = 0, to= 1, by = .025)){
  
  
  side_prob <- data.frame(move = c('b','w'), move_prob = c(i,1-i))
  

  
  
  
  game_prob <- data.frame(type = c('normal','threecheck'), game_prob = c(j,1-j))
  
  
  #### Full probability ####
  

  
  
  # ggplot(probs,aes(x = pct,color = game_res,linetype = type)) +
  #   geom_density() 
  
  risk <- 0
  
  # 
  # test <- -1000:900
  # 
  # testdf <- data.frame(test) %>%
  #   mutate(norm = (test-min(test))/(max(test)-min(test))) %>%
  #   mutate(util = (1-exp(-risk*norm))/(1-exp(-risk)))
  # 
  # ggplot(testdf, aes(x = test, y = norm)) +
  #   geom_path() +
  #   geom_path(aes(y = util),color = 'red')
  
  
  full_prob <- probs %>%
    left_join(side_prob, by = 'move') %>%
    left_join(opp_prob, by = 'b_fct') %>%
    left_join(game_prob, by = 'type') %>%
    ungroup() %>%
    mutate(marg = move_prob * game_prob *piece_prob * pct,
           norm = (points-min(points))/(max(points)-min(points)),
           util = case_when(
             risk == 0 ~ norm,
             risk != 0 ~ (1-exp(-risk*norm))/(1-exp(-risk))
           ),
           margutil = marg*util)
  
  # 
  # full_prob %>%
  #   group_by(w_fct) %>%
  #   summarise(eutil = sum(margutil))
  # 
  # 
  # 
  # df1a <- chessout %>%
  #   #filter(type == 'normal' & move == 'b') %>%
  #   group_by(w_fct,b_fct,move,type,game_res) %>%
  #   summarise(num = n()) %>%
  #   group_by(w_fct,b_fct,move,type) %>%
  #   mutate(total = sum(num), pct = num/total) %>%
  #   filter(game_res == FALSE)
  # 
  # 
  # df1a %>%
  #   group_by(w_fct) %>%
  #   summarise(mean(pct))
  
  p_f <- full_prob %>%
    filter(game_res == FALSE) %>%
    group_by(w_fct) %>%
    summarise(pf=sum(marg))
  
  
  # 
  # ggplot(full_prob %>% filter(game_res == FALSE)) +
  #   geom_density(aes(x = marg),fill = 'grey50') +
  #   #scale_x_continuous(limits = c(0,1)) +
  #   facet_wrap(~w_fct)
  

  
  
  strat_pf <- strat_dec %>%
    left_join(p_f, by = c('outcome' = 'w_fct')) %>%
    mutate(p_f_dec = pf * prob)
  
  strat_pffinal <- strat_pf %>%
    group_by(dec) %>%
    filter(pf == min(pf))
  
  pfout <- strat_pffinal %>%
    group_by(piece_sel) %>%
    summarise(p_f = sum(p_f_dec)) %>%
    mutate(b_prob = i,g_prob = j)
  
  dfout <- bind_rows(dfout,pfout)
  }
}




dfout1 <- dfout %>%
  group_by(b_prob,g_prob) %>%
  filter(p_f == min(p_f))

ggplot(dfout1) +
  geom_tile(aes(x = b_prob, y=g_prob , fill = p_f)) +
  #geom_point(aes(x = b_prob, y=g_prob , color = piece_sel)) +
  ggsci::scale_color_aaas(labels = c(TeX('$d_b$'),TeX('$d_k$'),TeX('$d_s$'))) +
  scale_fill_distiller(type = 'seq', palette = 18, direction = 1) +
  #scale_fill_continuous(limits = c(0,1)) +
  labs(
    y = TeX("$P(E_v = e_{3ck})$"),
    x = TeX("$P(E_m) = e_b$"),
    fill = TeX("$P(F|p_{o1} = 0.1, p_{o2} = 0.9)$")
  )

ggsave('sens_game1.png',dpi=320, width = 6, height = 4)


ggplot(dfout1) +
  geom_tile(aes(x = b_prob, y=g_prob , fill = p_f)) +
  geom_point(aes(x = b_prob, y=g_prob , color = piece_sel)) +
  ggsci::scale_color_aaas(labels = c(TeX('$d_b$'),TeX('$d_k$'),TeX('$d_s$'))) +
  scale_fill_distiller(type = 'seq', palette = 18, direction = 1) +
  #scale_fill_continuous(limits = c(0,1)) +
  labs(
    y = TeX("$P(E_v = e_{3ck})$"),
    x = TeX("$P(E_m) = e_b$"),
    fill = TeX("$P(F|p_{o1} = 0.1, p_{o2} = 0.9)$"),
    color = TeX("$D_p^*$")
  )

ggsave('sens_game2.png',dpi=320, width = 6, height = 4)



ggplot(dfout) +
  geom_path(aes(x = b_prob, y = p_f,color = piece_sel),size = 1) +
  ggsci::scale_color_aaas(labels = c(TeX('$d_b$'),TeX('$d_k$'),TeX('$d_s$'))) +
  labs(
    x = TeX("$P(E_v = e_{3ck})$"),
    y = TeX("P(F)"),
    color = TeX("$D_p$")
  )

ggsave('sens_game.png',dpi=320, width = 6, height = 4)


write_csv(head(df1a),'conditional.csv')




#### single bishop plot ####
opp_prob <- b_win %>%
  select(b_fct) %>%
  mutate(piece_prob = c(0,0,0,0,0,1))


dfout1 <- data.frame()

for(i in seq(from = 0, to= 1, by = .025)){
  print(i)
  
  for(j in seq(from = 0, to= 1, by = .025)){
    
    
    side_prob <- data.frame(move = c('b','w'), move_prob = c(i,1-i))
    
    
    
    
    
    game_prob <- data.frame(type = c('normal','threecheck'), game_prob = c(j,1-j))
    
    
    #### Full probability ####
    
    
    
    
    # ggplot(probs,aes(x = pct,color = game_res,linetype = type)) +
    #   geom_density() 
    
    risk <- 0
    
    # 
    # test <- -1000:900
    # 
    # testdf <- data.frame(test) %>%
    #   mutate(norm = (test-min(test))/(max(test)-min(test))) %>%
    #   mutate(util = (1-exp(-risk*norm))/(1-exp(-risk)))
    # 
    # ggplot(testdf, aes(x = test, y = norm)) +
    #   geom_path() +
    #   geom_path(aes(y = util),color = 'red')
    
    
    full_prob <- probs %>%
      left_join(side_prob, by = 'move') %>%
      left_join(opp_prob, by = 'b_fct') %>%
      left_join(game_prob, by = 'type') %>%
      ungroup() %>%
      mutate(marg = move_prob * game_prob *piece_prob * pct,
             norm = (points-min(points))/(max(points)-min(points)),
             util = case_when(
               risk == 0 ~ norm,
               risk != 0 ~ (1-exp(-risk*norm))/(1-exp(-risk))
             ),
             margutil = marg*util)
    
    # 
    # full_prob %>%
    #   group_by(w_fct) %>%
    #   summarise(eutil = sum(margutil))
    # 
    # 
    # 
    # df1a <- chessout %>%
    #   #filter(type == 'normal' & move == 'b') %>%
    #   group_by(w_fct,b_fct,move,type,game_res) %>%
    #   summarise(num = n()) %>%
    #   group_by(w_fct,b_fct,move,type) %>%
    #   mutate(total = sum(num), pct = num/total) %>%
    #   filter(game_res == FALSE)
    # 
    # 
    # df1a %>%
    #   group_by(w_fct) %>%
    #   summarise(mean(pct))
    
    p_f <- full_prob %>%
      filter(game_res == FALSE) %>%
      group_by(w_fct) %>%
      summarise(pf=sum(marg))
    
    
    # 
    # ggplot(full_prob %>% filter(game_res == FALSE)) +
    #   geom_density(aes(x = marg),fill = 'grey50') +
    #   #scale_x_continuous(limits = c(0,1)) +
    #   facet_wrap(~w_fct)
    
    
    
    pfout <- p_f %>%
      mutate(b_prob = i, g_prob = j)

    
    dfout1 <- bind_rows(dfout1,pfout)
  }
}



dfout1a <- dfout1 %>%
  group_by(b_prob,g_prob) %>%
  mutate(opt = case_when(
    pf == min(pf) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(opt == TRUE)

ggplot(dfout1a) +
  geom_tile(aes(x = b_prob, y=g_prob , fill = w_fct)) +
  ggsci::scale_fill_jama(labels = c('+1 Knight, Flank','+2 Knights, Center',
                                        '+2 Knights, Flank')) +
  labs(
    y = TeX("$P(E_v = e_{3ck})$"),
    x = TeX("$P(E_m) = e_b$"),
    # = TeX("$P(F|p_{o1} = 0.1, p_{o2} = 0.9)$"),
    fill = TeX("$ \\arg \\min_{E_s} P(F|E_s)$")
  )


ggsave('sens_game3b.png',dpi=320, width = 6, height = 4)


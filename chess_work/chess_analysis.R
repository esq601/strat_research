library(tidyverse)
library(latex2exp)

chessout <- read_csv("chess_test/chessout1.csv",lazy = FALSE) %>%
  mutate(type = "normal")


chessoutvar <- read_csv("chess_test/chessout1_var.csv",lazy = FALSE) %>%
  mutate(type = 'threecheck')

chessout <- bind_rows(chessout,chessoutvar)

chessout <- chessout %>%
  filter(str_detect(w_init, "RNNQ") == FALSE) %>%
  filter(str_detect(b_init, 'rnnq') == FALSE)

chessout$w_fct <- factor(chessout$w_init,
                          levels = c('PPPPPPPP/RNBQKBNR',
                                     'PPPPPPPP/RNNQKNNR',
                                     'PPPPPPPP/RBBQKBBR',
                                     'P1PNPP1P/RNBQKBNR',
                                     'N1PPPP1P/RNBQKBNR',
                                     'P2NN2P/RNBQKBNR',
                                     'N2PP2N/RNBQKBNR'
                          ))

chessout$b_fct <- factor(chessout$b_init,
                         levels = c('rnbqkbnr/pppppppp',
                                    'rnnqknnr/pppppppp',
                                    'rbbqkbbr/pppppppp',
                                    'rnbqkbnr/p1pnpp1p',
                                    'rnbqkbnr/n1pppp1p',
                                    'rnbqkbnr/p2nn2p',
                                    'rnbqkbnr/n2pp2n'
                         ))

#str(chessout)
  # filter(str_detect(b_init,'rnbqkbnr/1') == FALSE) %>%
  # filter(str_detect(w_init,'1/RNBQKBNR')== FALSE)

# 
# b0a = 'rnbqkbnr'
# b0b = 'pppppppp'
# b0 = paste(b0a , b0b,sep = '/')
# 
# b1a = 'rbbqkbbr'
# b1b = 'pppppppp'
# b1 = paste(b1a , b1b,sep = '/')
# 
# b2a = 'rnnqknnr'
# b2b = 'pppppppp'
# b2 = paste(b2a , b2b,sep = '/')
# 
# b3a = 'nrbqkbrn'
# b3b = '1nppppp1'
# b3 = paste(b3a , b3b,sep = '/')
# 
# b4a = 'nrbqkbrn'
# b4b = '1bppppp1'
# b4 = paste(b4a , b4b,sep = '/')
# 
# 
# b5a = 'rnbqkbnr'
# b5b = '3nr3'
# b5 = paste(b5a , b5b,sep = '/')



chessout$game_res[is.na(chessout$game_res)] <- 'tie'


df1 <- chessout %>%
  #filter(b_init == b1) %>%
  group_by(w_fct,b_fct,move,type,game_res) %>%
  summarise(num = n())

ggplot(df1) +
  geom_bar(stat= 'identity', aes(x = type, y = num,fill = game_res,color = move),position = 'dodge') +
  facet_wrap(b_fct ~ w_fct,ncol = 6)


df1a <- chessout %>%
  #filter(type == 'normal' & move == 'b') %>%
  group_by(w_fct,b_fct,move,type,game_res) %>%
  summarise(num = n()) %>%
  group_by(w_fct,b_fct,move,type) %>%
  mutate(total = sum(num), pct = num/total) %>%
  filter(game_res == FALSE)



ggplot(df1a) +
  geom_density(aes(x = pct),fill = 'grey50') +
  facet_wrap(~w_fct)

df2 <- chessout %>%
  mutate(score = case_when(
    game_res == "TRUE" ~ 5,
    game_res == "FALSE" ~ -5,
    TRUE ~ 0
  )) %>%
  group_by(b_fct, w_fct,move) %>%
  summarise(mean_val = mean(score), sd_val = sd(score),n = n() )

#str(df2)

ggplot(df2) +
  geom_tile(aes(x=b_fct, y=w_fct, fill = mean_val),color = 'black') +
  scale_fill_distiller(type = 'div', palette = 5, direction = 1) +
  facet_wrap(~move)




df3 <- chessout %>%
  mutate(score = case_when(
    game_res == "TRUE" ~ "Succeed",
    game_res == "FALSE" ~ "Fail",
    game_res == "tie" ~ "Fail"
  )) %>%
  group_by(b_fct, w_fct,score) %>%
  summarise(n = n() ) %>%
  group_by(b_fct, w_fct) %>%
  mutate(pct=n/sum(n))

#str(df2)

ggplot(df3 %>% filter(score == "Fail")) +
  geom_tile(aes(x=b_fct, y=w_fct, fill = pct),color = 'black') +
  scale_fill_distiller(type = 'seq', palette = 7, direction = 1)


df_fail <- chessout %>%
  mutate(score = case_when(
    game_res == "TRUE" ~ 1,
    game_res == "FALSE" ~ 0,
    game_res == "tie" ~ 1
  )) %>%
  group_by(w_init) %>%
  summarise(pct =1- sum(score)/n())




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






dfout <- data.frame()

for(i in seq(from = 0, to= 1, by = .025)){
  



side_prob <- data.frame(move = c('b','w'), move_prob = c(.85,.15)) #works .85, .15

opp_prob <- b_win %>%
  select(b_fct,piece_prob = scaled)

opp_prob <- b_win %>%
  select(b_fct) %>%
  mutate(piece_prob = rep((1/6),6))



game_prob <- data.frame(type = c('normal','threecheck'), game_prob = c(.85,.15)) #works .85, .15


#### Full probability ####

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

df_risk <- data.frame()
for(i in -5:5){
  risk <- i
  
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
  
  
  full_temp <- full_prob %>%
    group_by(w_fct) %>%
    summarise(eutil = sum(margutil)) %>%
    mutate(gamma = i)
  df_risk <- bind_rows(df_risk,full_temp)
  
  
  
}


b4kprob <- c(.8,.2)



strat_dec <- data.frame(piece_sel = c('B4K','B4K','Standard','MK','MK','MK','MK','MK'),
                        dec = c('d1','d2','d3','d4','d4','d5','d6','d6'),
                        prob = c(b4kprob,1,.5,.5,.3,.2,.2),
                        piece_place = c('none','none','none','center','flank','none','center','flank'),
                        outcome = c('PPPPPPPP/RBBQKBBR','PPPPPPPP/RNBQKBNR','PPPPPPPP/RNBQKBNR',
                                    'P1PNPP1P/RNBQKBNR','N1PPPP1P/RNBQKBNR','PPPPPPPP/RNBQKBNR',
                                    'P2NN2P/RNBQKBNR','N2PP2N/RNBQKBNR'))

df_risk <- left_join(df_risk, strat_dec, by = c("w_fct"="outcome"))

df_risk1 <- df_risk %>%
  group_by(gamma,dec) %>%
  filter(eutil == max(eutil)) %>%
  group_by(gamma,piece_sel) %>%
  #summarise(n())
  mutate(eutilnew = eutil*prob) %>%
  summarise(util = sum(eutilnew))
ggplot(df_risk1) +
  geom_path(aes(x = gamma, y = util, color = piece_sel, group = piece_sel),size = 1.5) + 
  ggsci::scale_color_jama(labels = c(TeX('$d_b$'),TeX('$d_k$'),
                                     TeX('$d_s$'))) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    y = TeX("$U(\\Phi)$"),
    x = TeX("$\\gamma"),
    # = TeX("$P(F|p_{o1} = 0.1, p_{o2} = 0.9)$"),
    color = TeX("$D_p$")
  )

ggsave('risk_attitude.png',dpi=320, width = 6, height = 4)

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


full_prob %>%
  group_by(w_fct) %>%
  summarise(eutil = sum(margutil))



df1a <- chessout %>%
  #filter(type == 'normal' & move == 'b') %>%
  group_by(w_fct,b_fct,move,type,game_res) %>%
  summarise(num = n()) %>%
  group_by(w_fct,b_fct,move,type) %>%
  mutate(total = sum(num), pct = num/total) %>%
  filter(game_res == FALSE)


df1a %>%
  group_by(w_fct) %>%
  summarise(mean(pct))

p_f <- full_prob %>%
  filter(game_res == FALSE) %>%
  group_by(w_fct) %>%
  summarise(pf=sum(marg))


# 
# ggplot(full_prob %>% filter(game_res == FALSE)) +
#   geom_density(aes(x = marg),fill = 'grey50') +
#   #scale_x_continuous(limits = c(0,1)) +
#   facet_wrap(~w_fct)


b4kprob <- c(.8,.2)



strat_dec <- data.frame(piece_sel = c('B4K','B4K','Standard','MK','MK','MK','MK','MK'),
                        dec = c('d1','d2','d3','d4','d4','d5','d6','d6'),
           prob = c(b4kprob,1,.5,.5,.3,.2,.2),
           piece_place = c('none','none','none','center','flank','none','center','flank'),
           outcome = c('PPPPPPPP/RBBQKBBR','PPPPPPPP/RNBQKBNR','PPPPPPPP/RNBQKBNR',
                       'P1PNPP1P/RNBQKBNR','N1PPPP1P/RNBQKBNR','PPPPPPPP/RNBQKBNR',
                       'P2NN2P/RNBQKBNR','N2PP2N/RNBQKBNR'))



strat_pf <- strat_dec %>%
  left_join(p_f, by = c('outcome' = 'w_fct')) %>%
  mutate(p_f_dec = pf * prob)

strat_pffinal <- strat_pf %>%
  group_by(dec) %>%
  filter(pf == min(pf))

pfout <- strat_pffinal %>%
  group_by(piece_sel) %>%
  summarise(p_f = sum(p_f_dec)) %>%
  mutate(b_prob = i)

dfout <- bind_rows(dfout,pfout)

}
pfout
ggplot(dfout) +
  geom_path(aes(x = b_prob, y = p_f,color = piece_sel),size = 1) +
  ggsci::scale_color_aaas(labels = c(TeX('$d_b$'),TeX('$d_k$'),TeX('$d_s$'))) +
  labs(
    x = TeX("$P(E_v = e_{3ck})$"),
    y = TeX("P(F)"),
    color = TeX("$D_p$")
  )

ggsave('sens_game.png',dpi=320, width = 6, height = 4)

### Bishop heavy



samp <- sample_n(chessout[,1:6],5)

write.csv(samp,'outsamp.csv')

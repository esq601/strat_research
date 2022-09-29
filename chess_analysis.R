library(tidyverse)


chessout <- read_csv("chess_test/chessout.csv")



chessout$game_res[is.na(chessout$game_res)] <- 'tie'


df1 <- chessout %>%
  group_by(b_init, w_init,game_res) %>%
  summarise(num = n())

ggplot(df1) +
  geom_bar(stat= 'identity', aes(x = game_res, y = num)) +
  facet_wrap(b_init ~ w_init)

df2 <- chessout %>%
  mutate(score = case_when(
    game_res == "TRUE" ~ 1,
    game_res == "FALSE" ~ -1,
    game_res == "tie" ~ 0
  )) %>%
  group_by(b_init, w_init) %>%
  summarise(mean_val = mean(score), sd_val = sd(score),n = n())

ggplot(df2) +
  geom_tile(aes(x=b_init, y=w_init, fill = mean_val),color = 'black') +
  scale_fill_distiller(type = 'div', palette = 5, direction = 1)

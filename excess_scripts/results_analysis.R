filenames <- list.files(pattern = 'mcts_test_two_player_05aprh')



dftotal <- data.table()
iter <- 1

for(file in filenames){
  
  df <- data.table(read_csv(file))
  
  max_turn <- max(df$turn)
  f_str <- sum(df[turn == max_turn & type=='f']$str)
  e_str <- sum(df[turn == max_turn & type=='e']$str)
  
  dftotal <- rbind(dftotal,data.table(trial = iter,filename = file,game_length = max_turn,f_str = f_str, e_str = e_str))
  iter <- iter + 1
}


dftotal1 <- dftotal[] %>%
  pivot_longer(cols = -trial)

ggplot(dftotal1 %>% filter(name %in% c('f_str','e_str'))) +
  geom_bar(aes(x = trial, y = value, fill = name),stat='identity',position = 'dodge')

dffull <- data.table()
iter <- 1
for(file in filenames[2:length(filenames)]){
  
  df <- data.table(read_csv(file))

  df$sim <- iter
  dffull <- rbind(dffull,df)
  iter <- iter + 1
}


colordf <- data.frame(id = c('inf_1','inf_2','inf_3','eny_1','eny_2','eny_3'),
           color = c( '#00BFFF', '#00BFDD', '#00BFCC', '#B22200', '#B22222', '#B22244'))

dffull1 <- dffull %>%
  left_join(colordf, by = 'id')

ggplot(dffull1) +
  geom_path(aes(x = turn, y = str, color = color, group = id),size = 1.5, alpha = 0.8) +
  #geom_segment(aes(x = turn,xend = turn, y=str,yend=str_old,color = type, group = id)) +
  #geom_segment(aes(x = turn,xend = turn, y=str,yend=str_old,color = type, group = id)) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = "Sample Simulation Game Flow",
    subtitle = 'Unit Strength per Turn for Each Instantiation',
    x = "Turn",
    y = 'Strength'
  ) +
  facet_wrap(~sim)

ggsave('images/game_flow.jpeg',height = 6, width = 9, dpi = 320)


library(gt)
dftable <- dftotal[-1,] %>% select(trial,f_str,e_str)
dftable %>%
  mutate(trial = trial-1) %>%
  gt() %>%
  tab_header(
    title = "Combat Outcomes",
    subtitle = "Simple Scenario Final Sum of Unit Strength"
  ) %>%
  cols_label(
    trial = "Trial",
    f_str = "F Strength",
    e_str = "E Strength"
  ) %>%
  fmt_number(
    columns = vars(f_str, e_str),
    decimals = 0
  ) %>%
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "grey"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "darkgrey",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(
      columns = vars(trial),
      rows = seq(2, nrow(dftable), by = 2)
    )
  )

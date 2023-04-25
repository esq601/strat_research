greeks <- 100
persians <- 300

greek_eff <- 4
persian_eff <- 1

mult <- .0005


t <- 0

df <- data.frame()


while(greeks > 0 & persians > 0) {
  
  greeks <- greeks - mult*persian_eff*persians
  persians <- persians - mult*greek_eff*greeks
  
  
  df <- rbind(df, data.frame(t = t, greeks = greeks, persians = persians))
  t <- t +1
}

View(df)

df <- df %>%
  pivot_longer(-t)

ggplot(df) +
  geom_line(aes(x = t, y = value, color = name),size = 1.5) +
  ggsci::scale_color_jama(labels = c('Greeks','Persians')) +
  theme(plot.background = element_rect(fill = 'white')) +
  labs(title = "Battle in the Open Sea",
       y = "Force Strength",
       x = "Time",
       color = 'Side',
       subtitle = "A simplified Demonstration of Lanchester's Model")

ggsave('images/salamis_open.png',width = 8, height = 6, dpi = 320)


greeks <- 100
persians <- 100

greek_eff <- 4
persian_eff <- 1

mult <- .0005


t <- 0

df1 <- data.frame()

split <- 0

while(greeks > 0 & persians > 0 & split < 3) {
  
    greeks <- greeks - mult*persian_eff*persians
    persians <- persians - mult*greek_eff*greeks
    df1 <- rbind(df1, data.frame(t = t, greeks = greeks, persians = persians,group = split))
    if(persians < 0 ){
      persians <- 100
      split <- split +1
    }
    
    

  
  t <- t + 1
}

df1a <- df1 %>%
  pivot_longer(cols = c(greeks,persians))

ggplot(df1a) +
  geom_line(data = df1a %>% filter(name == 'persians'),aes(x = t, y = value, color = name,group = group),size = 1.5) +
  geom_line(data = df1a %>% filter(name == 'greeks'),aes(x = t, y = value, color = name),size = 1.5) +
  ggsci::scale_color_jama(labels = c('Greeks','Persians')) +
  theme(plot.background = element_rect(fill = 'white')) +
  labs(title = "Battle in the Straights of Salamis",
       y = "Force Strength",
       x = "Time",
       color = 'Side',
       subtitle = "A simplified Demonstration of Lanchester's Model")

ggsave('images/salamis_straight.png',width = 8, height = 6, dpi = 320)


View(df1)

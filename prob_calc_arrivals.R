library(tidyverse)


dfout <- data.frame()

p <- 0.15
num <- 2
# 
# df <- data.frame()
# 
# for(j in 1:10000){
#   val <- num
#   i <- 1
#   
#   while(val > 0){
#     
#     
#     sucesses <- rbinom(val,1,p)
#     
#     val <- val - sum(sucesses)
#     i <- i + 1
#     
#   }
#   dttemp <- data.frame(test = j, trials = i-1)
#   
#   df <- rbind(df,dttemp)
# }
# 
# 
# df1 <- df %>%
#   group_by(trials) %>%
#   summarise(nval = n()) %>%
#   mutate(reg = nval/max(df$test))
# 
# sum(df1$reg)


for(i in 1:24){
  
    ind <- 0
    
    for(j in 1:num){
      
      ind <- ind +  choose(num,j)*(p * dbinom(0,i-1,p))^j * (1-pbinom(0,i-1,p))^(num-j)
    }
    
    dfout <- rbind(dfout, data.frame(turn = i, p = ind, units = num, bernp = p))
    
}


dfout1 <- dfout %>%
  mutate(conc = paste0(units,', ',bernp)) %>%
  group_by(conc) %>%
  mutate(pcum = cumsum(p))


ggplot(dfout1) + 
  geom_path(aes(x = turn, y = p, color = conc), position = 'dodge',stat = 'identity',size = 1) +
  scale_x_continuous(limits = c(0,24)) +
  ggsci::scale_color_aaas() +
  theme_minimal() +
  labs(
    color = latex2exp::TeX("$u_r, p_i$"),
    x = "Turn",
    y = latex2exp::TeX("$f_{u_r}(t)$")
  )

ggsave('reinforce_pmf.jpeg',width = 6, height = 3, dpi = 320)

ggplot(dfout1) + 
  geom_path(aes(x = turn, y = pcum, color = conc), position = 'dodge',stat = 'identity',size = 1) +
  scale_x_continuous(limits = c(0,24)) +
  ggsci::scale_color_aaas() +
  theme_minimal() +
  labs(
    color = latex2exp::TeX("$u_r, p_i$"),
    x = "Turn",
    y = latex2exp::TeX("$F_{u_r}(t)$")
  )


ggsave('reinforce_cdf.jpeg',width = 6, height = 3, dpi = 320)

ggplot() + 
  geom_line(data = df1, aes(x = trials, y =reg), color = 'blue',stat ='identity') +
  geom_line(data = dfout, aes(x = turn, y =p), color = 'red',stat = 'identity')

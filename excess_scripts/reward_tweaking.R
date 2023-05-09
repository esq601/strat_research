out_single1 <- out_single %>%
  pivot_wider(names_from = a, values_from = q)

out_single2 <- out_single %>%
  pivot_wider(names_from = `s.s`, values_from = n)

out_combine <- out_single1 %>%
  left_join(out_single2, by = 's.s')

testout <- split(out_single, by = 's.s')


# Sample data
friendly_units_health_before <- c(100, 100)
friendly_units_health_after <- c(90, 90)
enemy_units_health_before <- c(100, 100, 50)
enemy_units_health_after <- c(100, 100, 9)

# Tactical value of eliminating a single unit and health weight factor
unit_tactical_value <- 0.2
health_weight <- 0.8

# Calculate eEliminated and fEliminated based on the new rule
e_eliminated <- sum(enemy_units_health_after < 10) / length(enemy_units_health_before)
f_eliminated <- sum(friendly_units_health_after < 10) / length(friendly_units_health_before)

# Calculate eHealthReduction and fHealthReduction
e_health_reduction <- sum(enemy_units_health_before - enemy_units_health_after) / sum(enemy_units_health_before)
f_health_reduction <- sum(friendly_units_health_before - friendly_units_health_after) / sum(friendly_units_health_before)

# Compute the reward R
R <- (e_eliminated * unit_tactical_value + e_health_reduction * health_weight) - (f_eliminated * unit_tactical_value + f_health_reduction * health_weight)
R
# Normalize R
minR <- -unit_tactical_value - health_weight
maxR <- unit_tactical_value + health_weight
normalizedR <- 2 * (R - minR) / (maxR - minR) - 1

normalizedR




mpdays <- as.numeric(as.Date('2022-12-16')-as.Date('2022-09-22')) +
as.numeric(as.Date('2023-03-24')-as.Date('2023-01-07')) +
as.numeric(as.Date('2023-06-14')-as.Date('2023-04-01'))

mpcost <- 6937

mpweeks <- mpdays/7
mpcostwk <- mpcost / mpweeks
mpcostwk/15


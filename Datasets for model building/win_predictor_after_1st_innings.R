library('tidyverse')

matches = read_csv('./Cleaned Datasets/matches.csv')
deliveries = read_csv('./Cleaned Datasets/deliveries.csv')

#Merging both datasets
df <- merge(deliveries, matches, by.x='match_id', by.y='id')
df <- arrange(df,
              match_id,
              inning,
              over,
              ball)

#Features Required for Win predictor after 1st innings
#1. Batting Team in 2nd innings
#2. Bowling Team in 2nd innings
#3. Head to Head results
#4. Home Ground advantage
#5. Target
#6. Batting team points
#7. Bowling team points
#8. DL Applied or not
#9. Chase win percentage at the ground
#10. Chase win percentage of batting team
#11. Defense win percentage of bowling team
#12. First half average

#Preparing dataset
df1 <- df %>% 
  filter(inning == 1) %>% 
  group_by(match_id)

library('tidyverse')

df <- read_csv('./dataset/teamwise_home_and_away.csv')

#Null Values
for (c in colnames(df)) {
  n = df %>% 
    mutate(c = is.na(.[c])) %>% 
    filter(c == TRUE) %>% 
    summarise(n = n())
  print(paste(c, n$n, sep=":"))
}

#No null values

#Combining duplicate teams
df %>% 
  select('team') %>% 
  distinct()
#PuneWarriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(team = case_when(
    team == 'Pune Warriors' | team == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    team == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    team == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(team)
  )) %>% 
  group_by(team) %>% 
  summarise(
    home_wins = sum(home_wins),
    away_wins = sum(away_wins),
    home_matches = sum(home_matches),
    away_matches = sum(away_matches),
    home_win_percentage = (home_wins/home_matches)*100,
    away_win_percentage = (away_wins/away_matches)*100
  ) %>% 
  ungroup()

#Writing the cleaned dataset
df %>% 
  write.csv(., file='./Cleaned Datasets/teamwise_home_and_away.csv', row.names = F)

library('tidyverse')

df <- read_csv('./dataset/deliveries.csv')

#Null Values
for (c in colnames(df)) {
  n = df %>% 
    mutate(c = is.na(.[c])) %>% 
    filter(c == TRUE) %>% 
    summarise(n = n())
  print(paste(c, n$n, sep=":"))
}

#No Null values or Unnecessary Missing values

#Removing Duplicate Values(Typo errors)
#Batting team
df %>% 
  select('batting_team') %>% 
  distinct()
#Duplicate entries
#Pune Warriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(batting_team = case_when(
    batting_team == 'Pune Warriors' | batting_team == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    batting_team == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    batting_team == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(batting_team)
  ))

#Bowling team
df %>% 
  select('bowling_team') %>% 
  distinct()
#Duplicate entries
#Pune Warriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(bowling_team = case_when(
    bowling_team == 'Pune Warriors' | bowling_team == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    bowling_team == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    bowling_team == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(bowling_team)
  ))

#Writing the cleaned dataset
df %>% 
  write.csv(., file='./Cleaned Datasets/deliveries.csv', row.names = F)

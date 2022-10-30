library('tidyverse')

df <- read_csv('./dataset/matches.csv')

#Missing Values - city - 7, winner - 4, umpire1 - 2, 
#            umpire2 - 2, umpire3 - 637 missing values 

#Dropping Umpire3
df <- df %>% 
  select(-c('umpire3'))

#Dropping all null values since other columns have very few null values
df <- na.omit(df)

#Dimensions
dim(df)
#743,17

#Removing Duplicate Values(Typo errors)
#City
df %>% 
  select('city') %>% 
  distinct()
#No duplicate values

#Team1
df %>% 
  select('team1') %>% 
  distinct()
#Duplicate entries
#Pune Warriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(team1 = case_when(
    team1 == 'Pune Warriors' | team1 == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    team1 == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    team1 == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(team1)
  ))

#Team2
df %>% 
  select('team2') %>% 
  distinct()
#Duplicate entries
#Pune Warriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(team2 = case_when(
    team2 == 'Pune Warriors' | team2 == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    team2 == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    team2 == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(team2)
  ))

#Toss_winner
df %>% 
  select('toss_winner') %>% 
  distinct()
#Duplicate entries
#Pune Warriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(toss_winner = case_when(
    toss_winner == 'Pune Warriors' | toss_winner == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    toss_winner == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    toss_winner == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(toss_winner)
  ))

#Toss_decision
df %>% 
  select('toss_decision') %>% 
  distinct()
#No duplicate entries

#Result
df %>% 
  select('result') %>% 
  distinct()
#No duplicate entries

#Winner
df %>% 
  select('winner') %>% 
  distinct()
#Duplicate entries
#Pune Warriors, Rising Pune Super Giant, Rising Pune SuperGiants
#Deccan chargers, Sunrisers Hyderabad
#Delhi daredevils, Delhi Capitals
df <- df %>% 
  mutate(winner = case_when(
    winner == 'Pune Warriors' | winner == 'Rising Pune Supergiant' ~ 'Rising Pune Supergiants',
    winner == 'Deccan Chargers' ~ 'Sunrisers Hyderabad',
    winner == 'Delhi Daredevils' ~ 'Delhi Capitals',
    TRUE ~ as.character(winner)
  ))

#Writing the cleaned dataset
df %>% 
  write.csv(., file='./Cleaned Datasets/matches.csv', row.names = F)

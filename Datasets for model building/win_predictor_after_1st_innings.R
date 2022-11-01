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
#3. Head to Head results of chasing team
#4. Home Ground advantage
#5. Target
#6. Batting team points
#7. Bowling team points
#8. DL Applied or not
#9. Chase win percentage at the ground
#10. Chase win percentage of batting team
#11. Defense win percentage of bowling team
#12. First half average

df %>% 
  select('venue') %>% 
  distinct()

#Preparing dataset
df1 <- df %>% 
  filter(inning == 1) %>% 
  group_by(match_id) %>% 
  summarise(
    batting = as.character(bowling_team),
    bowling = as.character(batting_team),
    winner = as.character(winner),
    dl = as.integer(dl_applied),
    target = sum(total_runs),
    date = as.Date(date, format="%d-%m-%Y"),
    season = as.character(Season),
    city = as.character(city),
    ground = as.character(venue)
  ) %>% 
  distinct()

#Head to Head results of chasing team
head.to.head.wins <- matches %>% 
  group_by(team1, team2) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  select('team1', 'team2', 'winner', 'count') %>% 
  group_by(team1, team2, winner) %>% 
  mutate(count_w = n(),
         head_to_head = count_w/count) %>% 
  ungroup() %>% 
  distinct()

head_to_head_func <- function(df, batting, bowling) {
  x <- df %>% 
    filter(team1 == bowling, team2 == batting, winner == batting)
  if(length(x) == 0) {
    return(0)
  } else {
    return(x$head_to_head[1])
  }
}

df1 <- df1 %>% 
  mutate(head_to_head = head_to_head_func(head.to.head.wins, batting, bowling))

#Home Ground Advantage Chasing team - 1 Defending team - -1 Neutral ground 0
home.grounds <- matches %>% 
  group_by(team1, city) %>% 
  summarise(count = n()) %>% 
  mutate(home = max(count)) %>% 
  filter(home == count) %>% 
  select(team1, city)

#Adding few extra rows manually
#CSK - ranchi
#KXIP - mohali
#GL - Kanpur
#SRH - vishakapatnam
#DC - raipur
manual.home.grounds = data.frame(
  team1 = c('Sunrisers Hyderabad', 'Delhi Capitals', 'Gujarat Lions', 'Kings XI Punjab', 
            'Chennai Super Kings'),
  city = c('Visakhapatnam ', 'Raipur', 'Kanpur', 'Mohali', 'Ranchi')
)

home.grounds <- rbind(home.grounds, manual.home.grounds)
#Adding the advantage index to df1
df1 <- df1 %>% 
  mutate(home_ground_advantage = case_when(
    city %in% (home.grounds %>% 
      filter(team1 == batting) %>% 
      .$city) ~ 1,
    city %in% (home.grounds %>% 
      filter(team1 == bowling) %>% 
      .$city) ~ -1,
    T ~ 0
  ))

#Batting team points
#Changing date column to date datatype
matches <- matches %>% 
  mutate(date = as.Date(date, format="%d-%m-%Y"))

points_calc <- function(matches, season, win, Date) {
  pts.w <- matches %>% 
          filter(Season == season, winner == win, date<Date, result == "normal") %>% 
          nrow()*2
  pts.d <- matches %>% 
    filter(Season == season, (team1 == win | team2 == win),date<Date, result == "tie") %>% 
    nrow()
  return(pts.w + pts.d)
}

df1 <- df1 %>% 
  mutate(
    batting_team_points = points_calc(matches, season, batting, date)
  )

#Bowling points
df1 <- df1 %>% 
  mutate(
    bowling_team_points = points_calc(matches, season, bowling, date)
  )

#Chase win percentage at the ground
df1 <- df1 %>% 
  mutate(
    chase_win_perc = (
      matches %>% 
        filter(venue == ground, winner == batting) %>% 
        nrow()/nrow(matches %>% 
                      filter(venue == ground))))

#Chase win percentage of the batting team
df1 <- df1 %>% 
  mutate(
    chase_win_perc_bat = (
      matches %>% 
        filter(team2 == batting, winner == batting) %>% 
        nrow()/nrow(matches %>% 
                      filter(team2 == batting))
    )
  )

#Defense win percentage of the bowling team
df1 <- df1 %>% 
  mutate(
    defense_win_perc_bowl = (
      matches %>% 
        filter(team1 == bowling, winner == bowling) %>% 
        nrow()/nrow(matches %>% 
                      filter(team1 == bowling))
    )
  )

#First half average score in the venue
df1 <- df1 %>% 
  group_by(ground) %>% 
  mutate(first_inng_avg = mean(target)) %>% 
  ungroup()

#Dropping unwanted columns
df1 <- df1 %>% 
  select(-c("date","ground","city","season"))

#Encoding team values
library(CatEncoders)

#Batting team
team_labs = LabelEncoder.fit(df1$batting)
df1$batting = transform(team_labs, df1$batting)

#Bowling team
df1$bowling = transform(team_labs, df1$bowling)

#Winner
df1$winner = transform(team_labs, df1$winner)
df1 <- df1 %>% 
  mutate(winner = case_when(
    winner == batting ~ 1,
    winner == bowling ~ 0
  ))

#Writing Dataset
df1 %>% 
  write.csv(., file='./Datasets for model building/first_innings.csv', row.names = F)

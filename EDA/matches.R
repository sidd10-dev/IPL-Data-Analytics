library('tidyverse')

df <- read_csv('./dataset/matches.csv')

dim(df)
#Inference - 756 x 18

summary(df)

#Null Values
for (c in colnames(df)) {
  n = df %>% 
    mutate(c = is.na(.[c])) %>% 
    filter(c == TRUE) %>% 
    summarise(n = n())
  print(paste(c, n$n, sep=":"))
}
#Inference - city - 7, winner - 4, umpire1 - 2, 
#            umpire2 - 2, umpire3 - 637 missing values 

#Number of normal results vs tie
p1 <- df %>% 
  group_by(result) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=result,
             y=n,
             fill=result)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n,
            y=n+50)) +
  scale_color_viridis_d() +
  theme_bw() +
  ggtitle("Result Distribution")

ggsave(filename = "./EDA/plots/matches/Dist_Result.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)
#Inference - 9 Tie matches; 4 No result matches ; 743 proper matches

#City wise matches
df %>% 
  group_by(city) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=city,
             y=n,
             fill=city)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n,
                y=n+10)) +
  scale_fill_viridis_d() +
  theme_bw() +
  ggtitle("City Distribution")

ggsave(filename = "./EDA/plots/matches/City_Result.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)

#Inference - Most matches happen in mumbai, kolkata and chennai

#Relation between toss winner and match winner
df %>% 
  filter(!is.na(winner)) %>% 
  mutate(toss_match_relation = case_when(
    toss_winner == winner ~ "Toss Winner Wins Match",
    toss_winner != winner ~ "Toss Winner loses Match"
  )) %>% 
  group_by(toss_match_realtion) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=toss_match_relation,
             y=n,
             fill=toss_match_relation)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n,
                y=n+30)) +
  scale_fill_viridis_d() +
  theme_bw() +
  ggtitle("Toss Match Distribution")

ggsave(filename = "./EDA/plots/matches/Toss_Match_Relation.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)

#Inference - There is no significant difference between two counts
#Match winning depends slightly on Winning the toss

#Analysing win by runs distribution
df %>% 
  filter(win_by_runs != 0) %>% 
  ggplot(aes(x=win_by_runs)) +
  geom_density(color='black',
               fill='red',
               alpha=0.2) +
  theme_bw() +
  ggtitle("Win by runs distribution")

ggsave(filename = "./EDA/plots/matches/Win_By_Runs_Density.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)

#Inference - Data is Positively skewed and is not normal

#Analyzing win by wickets
df %>% 
  filter(win_by_wickets != 0) %>% 
  mutate(win_by_wickets = as.factor(win_by_wickets)) %>% 
  ggplot(aes(win_by_wickets,
             fill=win_by_wickets)) +
  geom_bar() + 
  scale_fill_viridis_d(direction=-1) +
  ggtitle("Win by wickets distribution")

ggsave(filename = "./EDA/plots/matches/Win_By_Wickets_Distribution.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)
#Inference - Most matches are won by 6 wickets

#Matches won with bowling first and batting first
library(plotrix)
bat.first.win <- df %>% 
  filter(win_by_runs > 0)

bowl.first.win <- df %>% 
  filter(win_by_wickets > 0)

png(file = "./EDA/plots/matches/Matches_won.png")
pie3D(c(nrow(bat.first.win), nrow(bowl.first.win)), 
    c("Batting First", "Bowling First"),
    main="Matches won with bowling first and batting first")
dev.off()

#Top 10 players with most player_of_the_match
df %>% 
  group_by(player_of_match) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  head(10)

#Inference - Chris Gayle - 21; ABD - 20; D Warner - 17
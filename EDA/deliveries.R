library('tidyverse')

df <- read_csv('./dataset/deliveries.csv')
matches <- read_csv('./dataset/matches.csv')

dim(df)
#Inference - 179078x21

#Total Number of wickets by each team
df %>%
  filter(!is.na(player_dismissed)) %>% 
  group_by(bowling_team) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=bowling_team,
             y=n,
             fill=bowling_team)) +
  geom_bar(stat="identity",
           color="black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  geom_text(aes(label=n,
                y=n+150)) +
  ggtitle("Total Number of Wickets by teams across all seasons") +
  scale_fill_viridis_d()

ggsave(filename = "./EDA/plots/deliveries/Total Wickets.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)

#Inference - Mumbai Indians(1138) and CSK have the most number of wickets

#Win rate if total > 200
library(plotrix)
total.greater.than.200 <- df %>% 
  filter(inning == 1) %>% 
  group_by(match_id) %>% 
  summarise(total = sum(total_runs),
            team = batting_team) %>% 
  ungroup() %>% 
  filter(total > 200) %>% 
  distinct() %>% 
  merge(., matches, by.x='match_id', by.y='id')

n1 = total.greater.than.200 %>% 
  filter(winner == team) %>% 
  nrow()

png(file = "./EDA/plots/deliveries/Win rate with tot greater than 200.png")
pie(c(n1, nrow(total.greater.than.200) - n1), 
      c("Won", "Lost"),
      main="Win rate if total > 200",
    radius = 1,
    col=c("lightblue","lightgreen"))
dev.off()


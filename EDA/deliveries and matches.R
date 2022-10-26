library('tidyverse')

deliveries <- read_csv('./dataset/deliveries.csv')
matches <- read_csv('./dataset/matches.csv')

#Merging both datasets
df <- merge(deliveries, matches, by.x='match_id', by.y='id')

#Highest run scored in each season
df %>% 
  group_by(Season, batsman) %>% 
  summarise(total.runs = sum(batsman_runs)) %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  summarise(max.runs = max(total.runs)) %>% 
  ungroup() %>% 
  mutate(Season = as.Date(str_sub(Season, 5, 9),format="%Y")) %>% 
  ggplot(aes(x=Season,
             y=max.runs)) +
  geom_line(color="black") +
  geom_point(color="blue3") +
  theme_bw() +
  geom_text(aes(label=max.runs,
                y=max.runs + 30)) +
  ggtitle("Highest Runs Scored per season")

ggsave(filename = "./EDA/plots/deliveries and matches/Highest run scorers.png", 
       plot = last_plot(),
       units = "cm", width = 29, height = 21, dpi = 300)
#Inference - Highest runs was scored by Virat Kohli in 2016


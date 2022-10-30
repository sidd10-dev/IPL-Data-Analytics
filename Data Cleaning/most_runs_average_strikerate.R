library('tidyverse')

df <- read_csv('./dataset/most_runs_average_strikerate.csv')

#Null Values
for (c in colnames(df)) {
  n = df %>% 
    mutate(c = is.na(.[c])) %>% 
    filter(c == TRUE) %>% 
    summarise(n = n())
  print(paste(c, n$n, sep=":"))
}

#Average has 34 null values
#players who have never got out
#Mostly bowlers so considering average to be zero
df <- df %>% 
  mutate(average = case_when(
    is.na(average) ~ 0,
    TRUE ~ as.numeric(average)
  ))

#Checking for duplicate entries
df %>% 
  select('batsman') %>% 
  distinct()

#No duplicate entries

#Writing the cleaned dataset
df %>% 
  write.csv(., file='./Cleaned Datasets/most_runs_average_strikerate.csv', row.names = F)

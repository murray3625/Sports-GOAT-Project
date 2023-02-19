
library(janitor)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringi)
library(r2r)
library(purrr)
library(base)
library(stringr)
library(tidyr)

# creates a data frame given input data 
create.df <- function(Players, Year) {
  df <- data.frame(Players, Year, seq(1, length(Players)))
  colnames(df) = c("Player", "Years", "Rank")
  df <- df %>% 
    mutate(Years = strsplit(as.character(Years), ", ")) %>% 
    unnest(Years)
  return(df)
}

# modifying strings so that 'present' is changed is 2022/2023
presentto2023 <- function(df) {
  if("Years" %in% colnames(df)){
    rep_str <- c("present" = "2023")
    df$Years <- str_replace_all(df$Years, rep_str)
    return (df)
  }
  else {
    return (NULL)
  }
}

# changing the intervals from a string to actual series of integers with a row for each year
# doing that for each sport and placing those rows into the new dataframe, df
rowforyear <- function(big.df, sport.df, string){
  rownum = 0
  for (i in 1:nrow(sport.df)){
    seq = seq(strtoi(substr(sport.df[i, "Years"], 1, 4)),
              strtoi(substr(sport.df[i, "Years"], 6, 9)))
    for (x in 1:length(seq)){
      rownum = rownum + 1
      big.df[rownum, ] = c(sport.df[i, 1], seq[x], string, sport.df[i, 3], sport.df[i, 2])
    }
}
  return(big.df)
}

# filter df for sport
# take the min oldest year for each player
# create empty dataframe for goats
# if the player has the best ranking at that point in his career, put him into goats dataframe

sportgoats <- function(df, string){
  filter <- df[df$Sport == string, ]
  filter <- filter %>% group_by(Player) %>% summarise(Year = min(Year),
                                                      Rank = min(Rank),
                                                      Years = unique(Interval))
  goats <- data.frame(matrix(nrow = 0, ncol = 4))
  
  for (i in 1:nrow(filter)){
    filter.2 <- filter[filter$Year <= as.numeric(filter[i, 2])[[1]], ]
    if(as.numeric(min(filter.2$Rank)) == as.numeric(filter[i, 3][[1,1]])){
      goats <- rbind(goats, filter[i,])
    }
  }
  return(goats)
}

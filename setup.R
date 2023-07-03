library(dplyr)
library(tidyr)

# Function to extract list of investors from column 'investors'
get_list_columns <- function(dataframe){
  big_list <- list()
  for (i in 1:nrow(dataframe)) {
    x <- dataframe[i, 7]
    l <- list()
    for (s in strsplit(as.character(x), ", ")[[1]]) {
      star <- gsub("[\\[\\]]", "", s)
      l <- c(l, substring(star, 2, nchar(star) - 1))
    }
    big_list[[i]] <- l
  }
  new_df <- data.frame(investor_list = unlist(big_list))
  dataframe$investor <- new_df$investor_list
  return(dataframe)
}

# Reading data, cleaning column names
df <- fread('unicorn2021.csv')
df <- df %>% 
  rename(date = `Date Joined`,
         investors = `Select Investors`,
         BValuation = `Valuation ($B)`) %>% 
  mutate(date = as.Date(date),
         year = format(date, "%Y")) %>% 
  drop_na()

# Cleaning Investors column
df <- df[grep("^\\[", df$investors),] %>% 
  select(-1)

df <- get_list_columns(df) %>% 
  select(-investors) %>% 
  unnest(cols = c(investor)) %>% 
  distinct()

write.csv(df, 'unicorn-clean.csv', row.names = FALSE)


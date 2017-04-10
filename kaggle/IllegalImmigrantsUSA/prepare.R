library(tidyverse) # Data manupulation library

# Load actual dataset provided by border protection team
arrests <- read.csv("~/kaggle/IllegalImmigrantsUSA/arrests.csv", stringsAsFactors = FALSE)
colnames(arrests) <- gsub("[.]", " ", colnames(arrests))

# Load lat,Long for different states
stateusa <- read.csv("~/kaggle/IllegalImmigrantsUSA/stateusa.csv", stringsAsFactors = FALSE)

# Separate state based on comma
df <- arrests %>%
  mutate(`State Territory` = strsplit(`State Territory`,", ")) %>%
  unnest(`State Territory`) %>%
  gather(key, no_of_arrest, 3:36) %>%
  separate(key, into = c("Year","Identity"), sep = "  ")

colnames(df)[3] <- "State"
df$Year <- gsub("[X]","",df$Year)

df <- left_join(df, stateusa, by = "State")
df$Identity[df$Identity == "All Illegal Immigrants "] <- "all"
df$Identity[df$Identity == "Mexicans Only "] <- "mexican"

# Store actual dataset for further investigation
write.csv(df, file = "~/kaggle/IllegalImmigrantsUSA/df_arrests.csv", row.names = F)
library(tidyverse)
library(ggpubr)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

## Section 2  ---- 
#----------------------------------------------------------------------------#
total_jail_population_1970 <- incarceration %>%
  filter(year == "1970") %>%
  select(year, total_jail_pop) %>%
  summarize(n = round(sum(total_jail_pop, na.rm = TRUE))) %>%
  pull(n)

total_jail_population_2018 <- incarceration %>%
  filter(year == "2018") %>%
  select(year, total_jail_pop) %>%
  summarize(n = round(sum(total_jail_pop, na.rm = TRUE))) %>%
  pull(n)

highest_jail_population_state <- incarceration %>%
  select(state, total_jail_pop) %>%
  group_by(state) %>%
  summarize(n = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(n == max(n)) %>%
  pull(state)

lowest_jail_population_state <- incarceration %>%
  select(state, total_jail_pop) %>%
  group_by(state) %>%
  summarize(n = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(n == min(n)) %>%
  pull(state)

total_jail_population_change <- total_jail_population_2018 - total_jail_population_1970

white_jail_population_2018 <- incarceration %>%
  filter(year == "2018") %>%
  select(year, white_jail_pop) %>%
  summarize(n = round(sum(white_jail_pop, na.rm = TRUE))) %>%
  pull(n)

black_jail_population_2018 <- incarceration %>%
  filter(year == "2018") %>%
  select(year, black_jail_pop) %>%
  summarize(n = round(sum(black_jail_pop, na.rm = TRUE))) %>%
  pull(n)

black_white_ratio_2018 <- black_jail_population_2018 / white_jail_population_2018
#----------------------------------------------------------------------------#


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# This function creates a dataframe of the total jail population of each year
get_year_jail_pop <- function() {
  df <- incarceration %>% 
    select(year, total_jail_pop)
  return(df)
}

# This function creates a bar chart with above function
plot_jail_pop_for_us <- function()  {
  df <- get_year_jail_pop()
  c <- ggplot(df) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         x = "Year", y = "Total Jail Population", 
         caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).
         This chart shows that the jail population in U.S. increased steadily between 1980 and 2010."
    )
  return(c)
}

chart_1 <- plot_jail_pop_for_us()
chart_1
#----------------------------------------------------------------------------#


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function creates a dataframe of the total jail population of each state
get_jail_pop_by_states <- function(states) {
  df <- incarceration %>% 
    select(year, state, total_jail_pop) %>%
    filter(state == states)
  return(df)
}

# This function creates a chart with above function
plot_jail_pop_by_states <- function(states) {
  df <- get_jail_pop_by_states(states)
  c <- ggplot(df) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(title = "Jail Population in U.S. by State (1970-2018)", 
         x = "Year", y = "Total Jail Population", 
         caption = "Figure 2. Jail Population in U.S. by State (1970-2018).
         This chart shows that there is a huge difference in jail population, but the growth trend is similar for each state.")
  return(c)
}

chart_2 <- plot_jail_pop_by_states(c("WA", "OR", "CA", "NY"))
chart_2
#----------------------------------------------------------------------------#


## Section 5  ---- 
#----------------------------------------------------------------------------#
# General Population and Prison Population by Race

# This function creates a dataframe of general/jail population of white/black people in 2018
get_general_jail_pop_by_race <- function() {
  df <- incarceration %>%
    filter(year == 2018) %>%
    select(total_pop_15to64, white_pop_15to64, black_pop_15to64, total_jail_pop, white_jail_pop, black_jail_pop) %>%
    mutate(white_general_percentage = (white_pop_15to64/total_pop_15to64)*100, black_general_percentage = (black_pop_15to64/total_pop_15to64)*100, white_jail_percentage = (white_jail_pop/total_jail_pop)*100, black_jail_percentage = (black_jail_pop/total_jail_pop)*100) %>%
    filter(white_jail_percentage < 100, black_jail_percentage < 100) %>%
    select(white_general_percentage, black_general_percentage, white_jail_percentage, black_jail_percentage)
  return(df)
}

# This function creates a chart with above function
plot_general_jail_pop_by_race <- function() {
  df <- get_general_jail_pop_by_race()
  c1 <- ggplot(df, aes(x = white_general_percentage, y = black_general_percentage)) + geom_point() +
    labs(title = "General Population Proportion of White on Black in U.S. (2018)", x = "Percentage of White (%)", y = "Percentage of Black (%)")
  c2 <- ggplot(df, aes(x = white_jail_percentage, y = black_jail_percentage)) + geom_point() +
    labs(title = "Jail Population Proportion of White on Black in U.S. (2018)", x = "Percentage of White (%)", y = "Percentage of Black (%)",
         caption = "Figure 3. General/Jail Population Proporion of White on Black in U.S. (2018).
         This chart shows that the ratio of black people to white people is larger for jail population compared to general popluation.")
  c <- ggarrange(c1, c2, ncol=1)
  return(c)
}

chart_3 <- plot_general_jail_pop_by_race()
chart_3
#----------------------------------------------------------------------------#


## Section 6  ---- 
#----------------------------------------------------------------------------#
# Map of Jail Population by State

state_names <- read.csv("~/info201/assignments/a4-JUGKlNG/source/state_names_and_codes.csv")
state_names <- state_names %>% select(Code, State)

jail_pop_by_states_2018 <- incarceration %>%
  filter(year == 2018) %>%
  select(state, total_jail_pop) %>%
  rename("Code" = state) %>%
  left_join(state_names) %>%
  rename("states" = State) %>%
  mutate(state = tolower(states))

map_state <- map_data("state") %>%
  rename(state = region) %>%
  left_join(jail_pop_by_states_2018, by = "state")

# This function creates a dataframe of jail population of each state in 2018
get_jail_pop_by_states_2018 <- function() {
  df <- map_state
  return(df)
}

# This function creates a map with above function
plot_jail_pop_by_states_2018 <- function() { 
  df <- get_jail_pop_by_states_2018()
  ggplot(df, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = total_jail_pop), color = "white") +
    scale_fill_continuous(name = "Jail Population", limits = c(0, 4000), breaks=c(0, 1000, 2000, 3000, 4000)) +
    labs(title = "Jail Population by State in U.S. (2018)", caption = "Figure 4. Jail Population by State in U.S. (2018).
         This chart shows that the jail population differs across the country and it is the largest in California.")
}

chart_4 <- plot_jail_pop_by_states_2018()
chart_4
#----------------------------------------------------------------------------#

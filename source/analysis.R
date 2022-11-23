library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function calculates the total jail pop of each year
get_year_jail_pop <- function() {
  df <- incarceration %>% 
    select(year, total_jail_pop)
  return(df)
}

# This function creates a bar chart with above function
plot_jail_pop_for_us <- function()  {
  df <- get_year_jail_pop()
  labels <- labs(
    title = "Increase of Jail Population in U.S. (1970-2018)",
    x = "Year", y = "Total Jail Population",
    caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018). This chart shows that the jail population in U.S. increased steadily between 1980 and 2010."
  )
  barchart <- ggplot(df) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(labels = scales::comma) +
    labels 
  return(barchart)
}

chart <- plot_jail_pop_for_us()
chart

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 



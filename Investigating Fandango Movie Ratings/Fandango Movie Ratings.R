
#  title: "Analyzing Recent Movie Ratings on Fandango"

#   ## Introduction
#   In 2015, Walt Hickey from FiveThirtyEight analyzed movie ratings on Fandango and uncovered a significant discrepancy between the ratings displayed to users and the actual ratings stored in the HTML of the page. He found that Fandango consistently rounded up movie ratings, leading to inflated ratings displayed on the site. The analysis revealed that Fandango's system often rounded ratings up to the nearest half-star, and in some cases, even to the nearest whole star, resulting in higher displayed ratings compared to the actual ratings.
# Fandango's response to Hickey's findings was that the biased rounding was due to a bug in their system, which they promised to fix. In this project, we aim to analyze more recent movie ratings data to determine whether Fandango has made any adjustments to their rating system following Hickey's analysis. Our goal is to assess whether the rounding practices have changed and if the displayed ratings are now more accurately reflecting the actual ratings.


# Load necessary libraries
library(dplyr)
library(readr)
# Read in the datasets
fandango_pre <- read_csv("fandango_score_comparison.csv")
fandango_post <- read_csv("movie_ratings_16_17.csv")

# View the first few rows of each dataset
head(fandango_pre)
head(fandango_post)

# FILM:	The film in question
# Fandango_Stars:	The number of stars the film had on its Fandango movie page
# Fandango_Ratingvalue:	The Fandango ratingValue for the film, as pulled from the HTML of each page. This is the actual average score the movie obtained.# Metacritic_user_nom	The Metacritic user score for the film, normalized to a 0 to 5 point system
#Fandango_votes:	The number of user votes the film had on Fandango
# Fandango_Difference	The difference between the presented Fandango_Stars and the actual Fandango_Ratingvalue

# movie:	the name of the movie
# year:	the release year of the movie
# fandango:	the Fandango rating of the movie (user score)

# Select relevant columns for pre-Hickey dataset
fandango_pre_selected <- fandango_pre %>%
  select(FILM, Fandango_Stars, Fandango_Ratingvalue, Fandango_votes, Fandango_Difference)

# View the first few rows to confirm
head(fandango_pre_selected)
# Select relevant columns for post-Hickey dataset
fandango_post_selected <- fandango_post %>%
  select(movie, year, fandango)

# View the first few rows to confirm
head(fandango_post_selected)


#we changed our goal to finding out whether there's any difference between Fandango's ratings for popular movies in 2015 and Fandango's ratings for popular movies in 2016. 

# Load the stringr package
library(stringr)
#isolating the movies released in 2015
# Extract the year from the FILM column and filter for movies released in 2015
fandango_2015 <- fandango_pre %>%
  mutate(year = str_sub(FILM, -5, -2)) %>%  # Extract the year from the FILM column
  filter(year == "2015")  # Filter for movies released in 2015

# View the first few rows of the filtered dataset
head(fandango_2015)

fandango_2016 <- fandango_pre %>%
  mutate(year = str_sub(FILM, -5, -2)) %>%  # Extract the year from the FILM column
  filter(year == "2016")  # Filter for movies released in 2016

# View the first few rows of the filtered dataset
head(fandango_2016)

#Check for Popular Movies
# Filter for movies with more than 30 fan ratings
fandango_2015_popular <- fandango_2015 %>%
  filter(Fandango_votes > 30)

# View the first few rows of the filtered dataset
head(fandango_2015_popular)

# Check the proportion of popular movies
nrow(fandango_2015_popular) / nrow(fandango_2015)
# #Check the Fan Rating System in the Post-Hickey Dataset
# # The movie_ratings_16_17.csv dataset (post-Hickey) doesn't include the number of fan ratings, 
# so we'll need to find an alternative way to ensure that it contains popular movies.
# # One approach is to compare the movies in this dataset with those in the pre-Hickey dataset. 
# If there’s significant overlap with the movies that had a high number of fan ratings in 2015, 
# this suggests that the dataset is likely to contain popular movies.

# Extract the year from the movie column for the post-Hickey dataset
fandango_2016_2017 <- fandango_post %>%
  mutate(year = as.character(year))

# Check for overlap with the 2015 popular movies
common_movies <- intersect(fandango_2015_popular$FILM, fandango_2016_2017$movie)

# View the common movies
common_movies

# Isolate movies released in 2016
fandango_2016 <- fandango_2016_2017 %>%
  filter(year == "2016")

# Isolate movies released in 2017
fandango_2017 <- fandango_2016_2017 %>%
  filter(year == "2017")

# View the first few rows of each dataset
head(fandango_2016)
head(fandango_2017)


# Combine the 2015 and 2016 datasets
fandango_combined <- bind_rows(
  fandango_2015_popular %>% select(Fandango_Stars) %>% mutate(year = "2015"),
  fandango_2016 %>% select(fandango) %>% rename(Fandango_Stars = fandango) %>% mutate(year = "2016")
)
fandango_combined
# Load ggplot2 for visualization
library(ggplot2)

# Generate the kernel density plots
ggplot(fandango_combined, aes(x = Fandango_Stars, color = year, fill = year)) +
  geom_density(alpha = 0.3) +  # alpha controls the transparency
  scale_x_continuous(breaks = seq(0, 5, 0.5), limits = c(0, 5)) +
  labs(
    title = "Kernel Density Plot of Fandango Ratings for 2015 and 2016",
    x = "Fandango Star Rating",
    y = "Density"
  ) +
  theme_minimal()


# Load necessary library for calculating mode
library(dplyr)

#Function to calculate mode
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
# The match() function returns a vector of the positions of each element of x in uniq_x.
#tabulate(): This function takes a vector of positive integers and returns a frequency count of each integer in the vector. For the previous example, tabulate(c(1, 1, 2, 3, 3, 3)) would return c(2, 1, 3).
#which.max(): This function returns the index of the first maximum value in the input vector. 

# Calculate mean, median, and mode for 2015
summary_2015 <- fandango_combined %>%
  filter(year == "2015") %>%
  summarize(
    mean = mean(Fandango_Stars),
    median = median(Fandango_Stars),
    mode = calculate_mode(Fandango_Stars)
  )

# Calculate mean, median, and mode for 2016
summary_2016 <- fandango_combined %>%
  filter(year == "2016") %>%
  summarize(
    mean = mean(Fandango_Stars),
    median = median(Fandango_Stars),
    mode = calculate_mode(Fandango_Stars)
  )

# Combine the summaries into one dataframe
summary_stats <- bind_rows(
  summary_2015 %>% mutate(year = "2015"),
  summary_2016 %>% mutate(year = "2016")
)

# View the summary statistics
summary_stats



# Reshape the data for plotting
library(tidyr)
summary_stats_long <- summary_stats %>%
  pivot_longer(cols = c(mean, median, mode), names_to = "statistic", values_to = "value")

# Generate the grouped bar plot
ggplot(summary_stats_long, aes(x = statistic, y = value, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Mean, Median, and Mode of Fandango Ratings (2015 vs 2016)",
    x = "Statistic",
    y = "Value"
  ) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.5)) +
  theme_minimal()







#question: Which countries have reported the highest number of positive cases in relation to the number of tests conducted?
#The dataset of our study contains daily & cumulative number of COVID-19 tests conducted, 
#number of positive, hospitalized, recovered & death cases reported by country
#1. briefly explore it
# Load necessary libraries
library(readr)
library(tibble)
# Load the data
covid_df <- read_csv("covid19.csv")
dim(covid_df)
# Determine the column names of the dataframe
vector_cols <- colnames(covid_df)
print(vector_cols)
head(covid_df)
# Display the summary of the dataframe
glimpse(covid_df)

# 2. Filter the rows related to "All States" and remove the Province_State column
library(dplyr)
covid_df_all_states <- covid_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State)

# Select specific columns related to daily measures
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
#summarize the covid_df_all_states_daily dataframe by computing the sum of the number of tested, positive,
#active and hospitalized cases grouped by the Country_Region column.
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
  group_by(Country_Region) %>%
  summarise( tested = sum(daily_tested),
             positive = sum(daily_positive),
             active = sum(active),
             hospitalized = sum(hospitalizedCurr)) %>%
  arrange(desc(tested)) 

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
covid_top_10 
# Getting vectors
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized
# Name the vectors with country names
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

# Identify the top three positive against tested cases
positive_tested_ratio <- positive_cases / tested_cases
positive_tested_top_3 <- sort(positive_tested_ratio, decreasing = TRUE)[1:3]
print(positive_tested_top_3)

# Create vectors for specific countries
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
# Combine vectors into a matrix
covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat
# Create a question and answer
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)
# Create lists containing the data structures
dataframes_list <- list(covid_df_all_states_daily_sum = covid_df_all_states_daily_sum)
matrices_list <- list(covid_mat = covid_mat)

vectors_list <- list(
  countries = countries,
  tested_cases = tested_cases,
  positive_cases = positive_cases,
  active_cases = active_cases,
  hospitalized_cases = hospitalized_cases
)

# Combine all lists into a named list
data_structure_list <- list(
  dataframes = dataframes_list,
  matrices = matrices_list,
  vectors = vectors_list
)
# Create a final list containing everything
covid_analysis_list <- list(question = question, answer = answer, data_structure_list = data_structure_list)
# Display the second element of this list
print(covid_analysis_list[[2]])


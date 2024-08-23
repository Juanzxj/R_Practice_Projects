# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
#1: Load the Dataset and Explore Its Structure
# Load the dataset
sales_df <- read_csv("sales2019.csv")
# Explore the dataset
glimpse(sales_df)
# Notes to make based on the output:
# - How big is the dataset? (Number of rows and columns)
# - What are the column names, and what do they represent?
# - What are the data types of each column?
#2: Check for Missing Data and Remove Rows with Missing 
# Check for missing data in the user_submitted_review column and remove those rows.
missing_review_count <- sum(is.na(sales_df$user_submitted_review))
cat("Number of rows with missing user_submitted_review:", missing_review_count, "\n")

# Remove rows with missing user_submitted_review
sales_df_clean <- sales_df %>%
  filter(!is.na(user_submitted_review))

# Record how many rows were removed
rows_removed <- nrow(sales_df) - nrow(sales_df_clean)
cat("Number of rows removed:", rows_removed, "\n")

#3: Calculate the Average Number of Books Purchased
avg_books_purchased <- mean(sales_df_clean$total_purchased, na.rm = TRUE)
cat("Average number of books purchased per order:", avg_books_purchased, "\n")

# Fill missing values in total_purchased with the average
sales_df_clean <- sales_df_clean %>%
  mutate(total_purchased_filled = if_else(is.na(total_purchased), 
                                          avg_books_purchased, 
                                          total_purchased))
sales_df_clean
#4: Analyze user_submitted_review for Sentiment
# Define a function to classify review sentiment
detect_sentiment <- function(review) {
  case_when(
    str_detect(review, regex("awesome|ok|good|great|excellent|recommended", ignore_case = TRUE)) ~ "positive",
    str_detect(review, regex("not good|not recommended|bad|poor", ignore_case = TRUE)) ~ "negative",
    TRUE ~ "neutral"
  )
}
#case_when(): A function from the dplyr package that allows you to vectorize multiple if-else statements in a concise and readable way.
#str_detect(): A function from the stringr package that checks if a string contains a specified pattern.
#~ "positive"  If str_detect returns TRUE, case_when assigns the value "positive".

# Apply the sentiment detection to create a new column
sales_df_clean <- sales_df_clean %>%
  mutate(review_sentiment = detect_sentiment(user_submitted_review))
sales_df_clean
#5:
# Convert the date column to a proper date format
sales_df_clean <- sales_df_clean %>%
  mutate(sale_date = mdy(date))
sales_df_clean
# Create a new grouping column for sales before and after July 1, 2019
sales_df_clean <- sales_df_clean %>%
  mutate(sale_period = if_else(sale_date < ymd("2019-07-01"), "Before July 1, 2019", "After July 1, 2019"))
sales_df_clean
#6: # Summarize the total number of books purchased before and after July 1, 2019
sales_summary <- sales_df_clean %>%
  group_by(sale_period) %>%
  summarize(total_books_purchased = sum(total_purchased_filled))

print(sales_summary)


# Summarize by sale period and customer type
sales_summary_by_type <- sales_df_clean %>%
  group_by(sale_period, customer_type) %>%
  summarize(total_books_purchased = sum(total_purchased_filled))

print(sales_summary_by_type)


# Summarize the number of positive reviews before and after July 1, 2019
positive_review_summary <- sales_df_clean %>%
  filter(review_sentiment == "positive") %>%
  group_by(sale_period) %>%
  summarize(count_positive_reviews = n())

positive_review_summary



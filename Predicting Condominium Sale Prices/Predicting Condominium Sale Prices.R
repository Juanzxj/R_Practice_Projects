# Title:
#   Predicting Condominium Sale Prices in New York City Using Linear Regression
# 
# Introduction:
#   This project aims to analyze the relationship between the size of a condominium, measured in gross square feet, and its sale price across the five boroughs of New York City: Bronx, Brooklyn, Manhattan, Staten Island, and Queens. By leveraging linear regression models, we will investigate how well the size of a condominium can predict its sale price both citywide and within each borough individually.

# Load the required libraries
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(readr)

# Load each borough's data, skipping the first four rows
bronx <- read_excel("rollingsales_bronx.xlsx", skip = 4)
brooklyn <- read_excel("rollingsales_brooklyn.xlsx", skip = 4)
manhattan <- read_excel("rollingsales_manhattan.xlsx", skip = 4)
staten_island <- read_excel("rollingsales_statenisland.xlsx", skip = 4)
queens <- read_excel("rollingsales_queens.xlsx", skip = 4)

#replace the borough numbers with their respective names:
# Function to replace borough numbers with names
replace_borough <- function(df, borough_name) {
  df %>% mutate(BOROUGH = borough_name)
}

# Apply the function to each dataframe
bronx <- replace_borough(bronx, "Bronx")
brooklyn <- replace_borough(brooklyn, "Brooklyn")
manhattan <- replace_borough(manhattan, "Manhattan")
staten_island <- replace_borough(staten_island, "Staten Island")
queens <- replace_borough(queens, "Queens")

# #ALTERNATIVELY
# # Replace borough numbers with names
# manhattan <- manhattan %>% mutate(BOROUGH = "Manhattan")
# brooklyn <- brooklyn %>% mutate(BOROUGH = "Brooklyn")
# bronx <- bronx %>% mutate(BOROUGH = "Bronx")
# staten_island <- staten_island %>% mutate(BOROUGH = "Staten Island")
# queens <- queens %>% mutate(BOROUGH = "Queens")
# 
# #combine all the borough data frames into a single data frame
# NYC_property_sales <- bind_rows(manhattan, brooklyn, bronx, staten_island, queens)
# 
# #If the BOROUGH variable originally contained numbers, you could replace them with names using case_when().
# NYC_property_sales <- NYC_property_sales %>%
#   mutate(BOROUGH = case_when(
#     BOROUGH == "1" ~ "Manhattan",
#     BOROUGH == "2" ~ "Bronx",
#     BOROUGH == "3" ~ "Brooklyn",
#     BOROUGH == "4" ~ "Queens",
#     BOROUGH == "5" ~ "Staten Island",
#     TRUE ~ BOROUGH  # Retain original value if not one of the expected numbers
#   ))


# Combine all borough dataframes into one
NYC_property_sales <- bind_rows(bronx, brooklyn, manhattan, staten_island, queens)

# Remove individual borough dataframes from memory
rm(bronx, brooklyn, manhattan, staten_island, queens)


# Convert column names to lower case with underscores
colnames(NYC_property_sales) %<>% str_replace_all("\\s", "_") %>% tolower()
# ` %<>% `used to both modify and assign the result back to the left-hand side.

# Convert selected columns to Title Case
NYC_property_sales <- NYC_property_sales %>%
  mutate(across(where(is.character), str_to_title))
# across() is a function that allows you to apply a function to multiple columns at once within a mutate(), summarize(), or similar dplyr function.
#str_to_title() is a function from the stringr package that converts text to Title Case.

# Retain distinct observations and drop the 'ease-ment' column if it exists
NYC_property_sales <- NYC_property_sales %>%
  distinct() %>%
  select(-easement)


# Apply the filtering operations : Filter out property exchanges below $10,000, remove rows with gross square feet of zero, and drop NA values:
NYC_property_sales <- NYC_property_sales %>%
  filter(sale_price > 10000) %>%
  filter(gross_square_feet > 0) %>%
  drop_na(gross_square_feet, sale_price) %>%
  arrange(borough, neighborhood)


# Save the cleaned data to a CSV file
write_csv(NYC_property_sales, "NYC_property_sales_cleaned.csv")

#Explore Bivariate Relationships with Scatterplots
# Filter for "Condominiums with Elevators" (R4)
NYC_property_sales <- read_csv("NYC_property_sales.csv")
glimpse(NYC_property_sales)
NYC_condos <- NYC_property_sales %>%
  filter(building_class_at_time_of_sale == "R4")
NYC_condos

# Load ggplot2 for plotting
library(ggplot2)

# Create the scatterplot with customizations
ggplot(NYC_condos, aes(x = gross_square_feet, y = sale_price, color = borough)) +
  geom_point(alpha = 0.6) +  # Add transparency with alpha
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000000)) +  # Remove scientific notation and set y-axis limits
  xlim(0, 5000) +  # Set x-axis limits
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a linear trend line without confidence intervals
  labs(title = "Relationship Between Condo Size and Sale Price in NYC",
       x = "Gross Square Feet",
       y = "Sale Price (in USD)") +  # Add descriptive labels
  theme_minimal()  # Change the ggplot theme

#Zoom In Further on the Scatterplot
# Create the scatterplot with adjusted axis limits
ggplot(NYC_condos, aes(x = gross_square_feet, y = sale_price, color = borough)) +
  geom_point(alpha = 0.6) +  # Add transparency with alpha
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000000)) +  # Further adjust y-axis limits
  xlim(0, 4000) +  # Further adjust x-axis limits
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a linear trend line without confidence intervals
  labs(title = "Zoomed In: Condo Size vs Sale Price in NYC",
       x = "Gross Square Feet",
       y = "Sale Price (in USD)") +  # Add descriptive labels
  theme_minimal()  # Change the ggplot theme


# Facet wrap by borough
ggplot(NYC_condos, aes(x = gross_square_feet, y = sale_price)) +
  geom_point(alpha = 0.6) +  # Add transparency with alpha
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a linear trend line without confidence intervals
  labs(title = "Condo Size vs Sale Price by Borough",
       x = "Gross Square Feet",
       y = "Sale Price (in USD)") +  # Add descriptive labels
  facet_wrap(~ borough, scales = "free", ncol = 2) +  # Facet by borough with free scales o that the axis limits are specifically tailored to each borough.
  theme_minimal()  # Change the ggplot theme


#Outliers and Data Integrity Issues
# Create a backup of the current dataset
NYC_condos_original <- NYC_condos

# Remove the sale record for 165 East 66th Street
NYC_condos <- NYC_condos %>%
  filter(!str_detect(address, "165 East 66th Street"))

# Retain the Outlier: retain the sale, it accurately represents an actual sale in the NYC market. This may reflect the true range of the market.
# Remove the Outlier: remove it, because it skews the linear model, making it less generalizable for more typical properties.
# Decision: Let's assume you decide to remove it for this analysis
# NYC_condos <- NYC_condos %>%
#   filter(!str_detect(address, "220 Central Park South") | sale_price != 238000000)

# Investigate specific sales in Brooklyn
library(lubridate)
suspicious_sales <- NYC_condos %>%
  filter(borough == "Brooklyn", 
         sale_price == 29620207, 
         date(sale_date) == as.Date("2019-04-08"))

# Display the suspicious sales
suspicious_sales
#  remove all 40 observations from the dataset because sale prices for each unit are erroneous. 

# Identify multi-unit sales
multi_unit_sales <- NYC_condos %>%
  group_by(sale_price, sale_date) %>%
  filter(n() >= 3) %>%
  ungroup()

# Remove multi-unit sales records from the NYC_condos dataframe.
# Remove multi-unit sales using an anti-join
NYC_condos <- anti_join(NYC_condos, multi_unit_sales)
# or use filter(n() <= 2)

#Linear Regression Model
# Generate linear model for cleaned data
NYC_condos_lm <- lm(sale_price ~ gross_square_feet, data = NYC_condos)
# Generate linear model for original data
NYC_condos_original_lm <- lm(sale_price ~ gross_square_feet, data = NYC_condos_original)

#Summarize the Models
# Summary of the cleaned data model
summary(NYC_condos_lm)
# Summary of the original data model
summary(NYC_condos_original_lm)

# use the confint() function to extract the confidence intervals for the slope in both models.
# Confidence intervals for cleaned data model
confint(NYC_condos_lm)
# Confidence intervals for original data model
confint(NYC_condos_original_lm)
# 
# For each model, the t-statistic was high enough, and the p-value was low enough, to declare that there is, in fact, a relationship between `gross_square_feet` and `sale_price`. 
# In each case the p-value was well below the 0.05 cutoff for significance meaning that it is extremely unlikely that the relationship between condominium size and sale price is due to random chance. 
# The confidence interval for the slope is [1155.699, 1221.839] for the `NYC_condos` dataset compared to only [1154.636, 1230.802] for the `NYC_condos_original` dataset. 
# This difference can likely be attributed to the removal of many multi-million dollar sale records for smaller units which impacted price predictions in the original dataset. The measure for *lack of fit*, or residual standard error (RSE) was lower for the cleaned dataset at 2,945,000 compared to 4,745,000 for the original dataset. However, it must be noted that the `NYC_condos` is smaller than the `NYC_condos_original` by 150 observations. Finally, the R-squared, or the proportion of the variability in `sale_price` that can be explained by `gross_square_feet` is 0.6166 for the cleaned `NYC_condos`. This is nearly double the R-squared value estimated for the `NYC_condos_original` dataset at 0.3177. 
# 
# Below is the updated scatterplot that uses the cleaned `NYC_condos` data. For the Brooklyn borough we are better able to see the spread of the data and how the trend line fits the data because we removed the \$30 million outliers. The same is true for the Manhattan borough because the $200 million multi-unit sale was removed.
# 


#Extract the residual standard error (RSE) using the sigma() function, which measures the standard deviation of the residuals (errors).
# Residual standard error for cleaned data model
sigma(NYC_condos_lm)
# Residual standard error for original data model
sigma(NYC_condos_original_lm)

# Re-generate the Faceted Scatterplot
# Facet wrap by borough
ggplot(NYC_condos, aes(x = gross_square_feet, y = sale_price)) +
  geom_point(alpha = 0.6) +  # Add transparency with alpha
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a linear trend line without confidence intervals
  labs(title = "Condo Size vs Sale Price by Borough (After Cleaning)",
       x = "Gross Square Feet",
       y = "Sale Price (in USD)") +  # Add descriptive labels
  facet_wrap(~ borough, scales = "free", ncol = 2) +  # Facet by borough with free scales
  theme_minimal()  # Change the ggplot theme

#Linear Regression Models for each Borough - Coefficient Estimates
# Nest the NYC_condos dataframe by borough
NYC_nested <- NYC_condos %>%
  group_by(borough) %>%
  nest()
# Print the nested dataframe to inspect the data structure
print(NYC_nested)

# Extract and print the nested dataframe for one borough (e.g., Manhattan)
manhattan_data <- NYC_nested$data[[which(NYC_nested$borough == "Manhattan")]]
print(manhattan_data)

# Fit linear models to each borough individually
library(purrr)
library(broom)
NYC_coefficients <- NYC_nested %>%
  mutate(model = map(data, ~lm(sale_price ~ gross_square_feet, data = .)))
# Print the NYC_coefficients dataframe to inspect the structure
print(NYC_coefficients)

# Extract and summarize the model for one borough (e.g., Manhattan)
manhattan_model <- NYC_coefficients$model[[which(NYC_coefficients$borough == "Manhattan")]]
summary(manhattan_model)

# Generate a tidy dataframe of coefficient estimates with confidence intervals
NYC_coefficients <- NYC_coefficients %>%
  mutate(tidy_coefficients = map(model, ~tidy(., conf.int = TRUE)))
# Print the NYC_coefficients dataframe to inspect the tidy coefficients
print(NYC_coefficients)

# Extract and print the tidy coefficients for one borough (e.g., Manhattan)
manhattan_tidy <- NYC_coefficients$tidy_coefficients[[which(NYC_coefficients$borough == "Manhattan")]]
print(manhattan_tidy)


# Unnest the tidy coefficients and filter to return only the slope estimates
NYC_slope_estimates <- NYC_coefficients %>%
  select(borough, tidy_coefficients) %>%
  unnest(cols = tidy_coefficients) %>%
  filter(term == "gross_square_feet")

# Print the resulting dataframe to inspect the slope estimates
print(NYC_slope_estimates)

# Summary:
# Direction of Relationship: The slope estimate for each borough indicates the expected change in sale_price for each additional square foot of space. 
# A positive slope indicates that as gross_square_feet increases, sale_price also increases, which is expected.
# Confidence Intervals: The confidence intervals for each slope estimate provide a range within which the true slope is likely to fall 
#with a certain level of confidence (e.g., 95%). Narrower intervals suggest more precise estimates.


# Load necessary libraries
library(dplyr)
library(tidyr)
library(broom)
library(purrr)

# Nest the NYC_condos dataframe by borough
NYC_nested <- NYC_condos %>%
  group_by(borough) %>%
  nest()

# Print the nested dataframe to inspect the structure
print(NYC_nested)

# Fit linear models to each borough individually
NYC_nested <- NYC_nested %>%
  mutate(model = map(data, ~lm(sale_price ~ gross_square_feet, data = .)))

# Print the NYC_nested dataframe to inspect the structure
print(NYC_nested)



# Generate a tidy dataframe of regression summary statistics
NYC_nested <- NYC_nested %>%
  mutate(summary_stats = map(model, glance))
# The glance() function extracts model-level statistics like R-squared, adjusted R-squared, p-value, AIC, BIC, etc.
# Print the NYC_nested dataframe to inspect the structure
print(NYC_nested)

# Unnest the summary statistics to a single tidy dataframe
NYC_summary_stats <- NYC_nested %>%
  select(borough, summary_stats) %>%
  unnest(cols = summary_stats)
# Print the resulting tidy dataframe
print(NYC_summary_stats)

# Add Regression Statistics with augment()
# The augment() function provides detailed regression diagnostics, such as residuals and fitted values, which can help you understand how well your model fits the data within each borough.
# Add regression statistics for each borough using augment
NYC_augmented <- NYC_nested %>%
  mutate(augmented_stats = map(model, augment))

# Unnest to a single dataframe
NYC_augmented_stats <- NYC_augmented %>%
  select(borough, augmented_stats) %>%
  unnest(cols = augmented_stats)
# Inspect the augmented statistics
print(NYC_augmented_stats)

# Perform Multivariate Linear Regression
# Fit a multivariate linear model including land_square_feet
NYC_multivariate_models <- NYC_condos %>%
  group_by(borough) %>%
  nest() %>%
  mutate(model = map(data, ~lm(sale_price ~ gross_square_feet + land_square_feet, data = .)))

# Extract and tidy the regression summary statistics
NYC_multivariate_summary <- NYC_multivariate_models %>%
  mutate(summary_stats = map(model, glance)) %>%
  select(borough, summary_stats) %>%
  unnest(cols = summary_stats)

# Print the multivariate model summary
print(NYC_multivariate_summary)


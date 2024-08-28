
#  title: "Probability Analysis for the 6/49 Lottery Game"


  
  ## Introduction
#   
#   This project is focused on developing the underlying logic for a mobile app designed to 
#   help lottery addicts understand their chances of winning in the 6/49 lottery game. The app will 
#   guide users through exercises that allow them to realistically estimate their odds of winning, 
#   aiming to discourage excessive gambling by demonstrating the low probability of success.
# 
# The main goal of this project is to build functions that can answer key probability questions, 
# such as the chances of winning the big prize with a single ticket, the probability of winning if
# multiple tickets are purchased, and the likelihood of having a certain number of winning numbers 
# on a ticket. Additionally, we will consider historical data from the Canadian 6/49 lottery game 
# to provide empirical insights alongside theoretical probabilities. By the end of this project, 
# the institute will have a robust set of probability calculations to incorporate into their app, 
# helping users make more informed decisions about lottery participation.


# Define the factorial function
factorial <- function(n) {
  if (n == 0) {
    return(1)  # By definition, 0! is 1
  } else {
    result <- 1
    for (i in 1:n) {
      result <- result * i
    }
    return(result)
  }
}
# Test the factorial function
factorial(5)  # Should return 120



# Define the combination function
combination <- function(n, k) {
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}
# Test the combination function
combination(5, 2)  # Should return 10

# Function to calculate and print the probability of winning with one ticket
one_ticket_probability <- function(ticket_numbers) {
  # Calculate the total number of possible combinations
  total_combinations <- combination(49, 6)
  
  # Since there is only one successful combination
  successful_combinations <- 1
  
  # Calculate the probability of winning
  probability <- successful_combinations / total_combinations
  
  # Format the probability as a percentage
  probability_percentage <- sprintf("%.10f", probability * 100)
  
  # Print the result in a user-friendly way
  print(sprintf("The probability of winning the big prize with the ticket numbers %s is 1 in %d, or approximately %s%%.",
                paste(ticket_numbers, collapse = ", "), total_combinations, probability_percentage))
}

# Test the function with an example ticket
one_ticket_probability(c(13, 22, 24, 27, 42, 44))


# Load the necessary libraries
library(readr)
library(dplyr)

# Load the dataset into a tibble
lottery_data <- read_csv("649.csv")
lottery_data <- as_tibble(lottery_data)

# Print the number of rows and columns
num_rows <- nrow(lottery_data)
num_cols <- ncol(lottery_data)
print(sprintf("The dataset has %d rows and %d columns.", num_rows, num_cols))

# Print the first three rows
print("First three rows of the dataset:")
head(lottery_data, 3)

# Print the last three rows
print("Last three rows of the dataset:")
tail(lottery_data, 3)


# Provided vectors
data1 <- c(1, 3, 5)
data2 <- c(2, 4, 6)
data3 <- c(8, 9, 7)
#Create an unnamed list
unnamed_list <- list(data1, data2, data3)
# Access the first vector in unnamed_list
first_vector <- unnamed_list[[1]]
#Create a named list
named_list <- list(first = data1, second = data2, third = data3)
#Add up the first numbers in all the vectors in named_list
first_item_sum <- named_list$first[1] + named_list$second[1] + named_list$third[1]
# Display results
print(first_vector)  # Should print c(1, 3, 5)
print(first_item_sum)  # Should print 11 (1 + 2 + 8)



# Load the necessary package
library(purrr)

# Provided vectors
data1 <- c(1, 3, 5)
data2 <- c(2, 4, 6)
data3 <- c(8, 9, 7)

#  Use pmap to create a list of averages
# averages <- pmap(list(data1, data2, data3), ~ mean(c(..1, ..2, ..3)))
# ~ (Tilde): Introduces a formula or anonymous function in a concise way.
# ..1, ..2, ..3:
#   Represent the first, second, and third arguments passed to the function in each iteration.
# Correspond to the elements from data1, data2, and data3 respectively.

averages <- pmap(
  list(data1, data2, data3),
  function(x, y, z) {
    mean(c(x, y, z))
  }
)


# Convert the list of averages into a vector
averages_vector <- unlist(averages)

# Assign the first item of the vector to first_average
first_average <- averages_vector[1]

# Display the results
print(averages_vector)  # Should print the vector of averages
print(first_average)    # Should print the first average, which is 3.666667




# Load necessary libraries
library(dplyr)
library(purrr)

# Load the dataset if not already loaded
# lottery_data <- read_csv("649.csv")

# Extract the six NUMBER DRAWN columns into a list of vectors
winning_numbers_list <- lottery_data %>%
  select(`NUMBER DRAWN 1`, `NUMBER DRAWN 2`, `NUMBER DRAWN 3`, `NUMBER DRAWN 4`, `NUMBER DRAWN 5`, `NUMBER DRAWN 6`) %>%
  pmap(~ c(..1, ..2, ..3, ..4, ..5, ..6))

# Function to calculate combinations
combination <- function(n, k) {
  factorial(n) / (factorial(k) * factorial(n - k))
}

# Function to check historical occurrence
check_historical_occurence <- function(user_numbers, winning_numbers_list) {
  # Check if the user numbers match any of the winning combinations
  occurrences <- map_lgl(winning_numbers_list, ~ setequal(.x, user_numbers))
  # `~` This is a shorthand way to define an anonymous function in R using the ~ 
  # setequal(.x, user_numbers) checks if two sets (i.e., two vectors) have exactly the same elements, regardless of order.
  # map_lgl() applies a function to each element of a list or vector and returns a logical vector 
  
  # Count how many times the user's numbers have won
  num_occurrences <- sum(occurrences)
  
  # Calculate the probability of winning in the next drawing with the given numbers
  total_combinations <- combination(49, 6)
  probability <- 1 / total_combinations
  
  # Print the results
  print(sprintf("The combination %s occurred %d time(s) in the past.", 
                paste(user_numbers, collapse = ", "), num_occurrences))
  print(sprintf("The probability of winning the big prize in the next drawing with this combination is approximately 1 in %d, or %.10f%%.", 
                total_combinations, probability * 100))
}

# Test the function
check_historical_occurence(c(3, 11, 12, 14, 41, 43), winning_numbers_list)
check_historical_occurence(c(1, 2, 3, 4, 5, 6), winning_numbers_list)



# Function to calculate and print the probability based on the number of tickets
multi_ticket_probability <- function(n_tickets) {
  # Calculate the total number of possible combinations
  total_combinations <- combination(49, 6)
  
  # Calculate the probability of winning with the given number of tickets
  probability <- n_tickets / total_combinations
  
  # Determine the probability in percentage
  probability_percentage <- sprintf("%.10f", probability * 100)
  #%.10f: Together, this means "format the number as a floating-point number with 10 decimal places."
  # Print the results in a user-friendly way
  if (n_tickets == 1) {
    print(sprintf("With %d ticket, the probability of winning the big prize is 1 in %d, or approximately %s%%.", 
                  n_tickets, total_combinations, probability_percentage))
  } else {
    print(sprintf("With %d tickets, the probability of winning the big prize is 1 in %d, or approximately %s%%.", 
                  n_tickets, total_combinations, probability_percentage))
  }
}

# Test the function with the specified inputs
multi_ticket_probability(1)
multi_ticket_probability(10)
multi_ticket_probability(100)
multi_ticket_probability(10000)
multi_ticket_probability(1000000)
multi_ticket_probability(6991908)
multi_ticket_probability(13983816)


# Function to calculate combinations (already implemented earlier)
combination <- function(n, k) {
  factorial(n) / (factorial(k) * factorial(n - k))
}

# Function to calculate and print the probability for matching 3, 4, or 5 numbers
probability_less_6 <- function(k) {
  if (k < 3 | k > 5) {
    stop("Please input an integer between 3 and 5.")
  }
  
  # Calculate the number of successful outcomes (combinations of k from 6)
  successful_outcomes <- combination(6, k)
  
  # Calculate the number of total possible outcomes (combinations of k from 49)
  total_possible_outcomes <- combination(49, k)
  
  # Calculate the probability of matching exactly k numbers
  probability <- successful_outcomes / total_possible_outcomes
  
  # Format the probability as a percentage
  probability_percentage <- sprintf("%.10f", probability * 100)
  
  # Print the results in a user-friendly way
  print(sprintf("The probability of matching exactly %d numbers is 1 in %.0f, or approximately %s%%.", 
                k, 1 / probability, probability_percentage))
}

# Test the function with inputs 3, 4, and 5
probability_less_6(3)
probability_less_6(4)
probability_less_6(5)


# Function to generate the next state based on the current state and a random number
generate_next_state <- function(current_state, random_number) {
  if (current_state == 0) {
    if (random_number <= 0.5) {
      return(0)
    } else {
      return(2)
    }
  } else if (current_state == 1) {
    if (random_number <= 0.5) {
      return(0)
    } else {
      return(2)
    }
  } else if (current_state == 2) {
    if (random_number <= 0.5) {
      return(1)
    } else {
      return(3)
    }
  } else {
    if (random_number <= 0.5) {
      return(2)
    } else {
      return(3)
    }
  }
}

# Generating 50,000 random numbers
set.seed(123)  # Setting seed for reproducibility
random_numbers <- runif(50000)
print(random_numbers[1:40])  # Printing the first 40 random numbers

# (b) Choosing the initial state X0 at random
initial_state <- sample(0:3, 1)  # Randomly choose initial state
cat("Initial state (X0):", initial_state, "\n\n")

# (c) Generating Xn recursively
states <- numeric(50000)
states[1] <- initial_state
for (i in 2:50000) {
  states[i] <- generate_next_state(states[i-1], random_numbers[i])
}

# Printing the first 40 values of the random variables (states)
print("First 40 states:")
for (i in 1:40)
{
  cat(states[i], ", ")
}

# (d) Computing the proportion of times for which weather is in each state
proportions <- prop.table(table(states))
print("Proportions of states:")
print(proportions)

print('Program results compared to my results(first row being states, second row being program results, and last row being my results from part 1')
print(proportions)
cat("  0.25     0.15    0.15    0.45")

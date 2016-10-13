# Exercise 3: Vector and function practice

# Create a vector `marbles` with 6 different colors in it (representing marbles)
marbles <- c('Blue', 'Green', 'Red', 'Yellow', 'Black', 'White')

# Use the `sample` function to select a single marble
sample(marbles, size = 1)

# Write a function MarbleGame that does the following:
# - Takes in a `guess` of a marble color
# - Randomly samples a marble
# - Returns whether or not the person guessed accurately (preferrably a full phrase)
MarbleGame <- function (guess){
  random.marble <- sample(marbles, size = 1, replace = FALSE, prob = NULL)
  
  if (tolower(random.marble) == tolower(guess)){
    return ("You choose wisely.")
  } else {
    return ("You choose poorly.")
  }
}

# Play the marble game!
MarbleGame('red')


# Bonus: Play the marble game until you win, keeping track of how many tries you take
MarbleGameRuns <- function (guess){
  count <- 1
  random.marble <- sample(marbles, size = 1, replace = FALSE, prob = NULL)
  
  while (tolower(random.marble) != tolower(guess)){
    count <- count + 1
    random.marble <- sample(marbles, size = 1, replace = FALSE, prob = NULL)
  }
  
  return (paste ("It took", count, "to randomly match your guess"))
}

MarbleGameRuns('red')

## Double bonus(answer not provided): play the game 1000X (until you win) and track the average number of tries
# Is it what you expected based on the probability

ThousandRun <- function (guess){
  count <- 10000
  cycles <- 0
  random.marble <- sample(marbles, size = 1, replace = FALSE, prob = NULL)
  
  while (count > 0){
    count <- count - 1
    
    if (tolower(random.marble) == tolower(guess)) {
      append(cycles, abs(count - cycles))
    }
  }
  
  return (cycles)
}
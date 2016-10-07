# Exercise 1: Creating and Indexing Vectors

# Create a vector `x` that has the values 10 through 20 in it (using the `:` operator)
x <- 10:20

# Create a vector `y` that has the values 21 through 30 in it (using the `seq` operator)
y <- seq(21, 30)

# Create a vector `z` by combining the vectors `x` and `y`
z <- c(x, y)

# Create a variable `w` that is equal to the 11th element in your vector `z`
w <- z[11]

# Create a vector `q` that is equal to the second through the 5 elements of `z`
q <- z[2:5]

# Create a variable `len` that is equal to the length of your vector `q`
len <- length(q)

### Bonus ###

# Create a vector `odd` that holds the odd numbers from 1 to 100
odd <- c(1:100)
odd <- odd[odd %% 2 != 0]

# Using the `all` function, confirm that all of the numbers in your `odd` vector are odd
all(odd %% 2 !=0)

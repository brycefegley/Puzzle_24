rm(list=ls())

# Puzzle:
# Using all of the numbers 1, 3, 4, 6 exactly once, and any combination of:
# addition, substraction, multiplication and division (and parenthesis to group
# operations however you like), arrive at the number: 24.

# Solution:
# Brute force search for combination of numbers, operations, and groupings
# that provide the correct solution. For this problem, the brute force solution
# happens to work very quickly, much less than a second, and typically less
# than 10,000 iterations, depending on seed.

numbers <- c(1, 3, 4, 6)

ops <- c("add", "subtract", "multiply", "divide")

# Because the order of intermediate results matters, there are six orders 
# in which the operations may be performed: 
orders <- c("(((a, b), c), d)",
            "((c, (a, b)), d)",
            "((d, (a, b)), c)",
            "(d, (c, (a, b)))",
            "((a, b), (c, d))",
            "((c, d), (a, b))")

# Operation function performs the operations
fOp <- function(op, x) {
  if (op == ops[1]) do.call('+', as.list(x))
  else if (op == ops[2]) do.call('-', as.list(x))
  else if (op == ops[3]) do.call('*', as.list(x))
  else if (op == ops[4]) do.call('/', as.list(x))
}

i <- 1  # Counter
result <- 0  # Initialize a result checker

system.time(
  # while loop through results until success is achieved
  while (result != 24) {
    n <- sample(numbers, 4)  # sample from the numbers w/o replacement
    op <- sample(ops, 3, replace=TRUE)  # sample from operations w/replacement
    
    # sample from the order of operations
    g <- sample(1:6, 1)
    
    # build the order conditions
    if (g == 1) {
      r1 <- fOp(op[1], n[c(1, 2)])
      r2 <- fOp(op[2], c(r1, n[3]))
      result <- fOp(op[3], c(r2, n[4]))
    } else if (g == 2) {
      r1 <- fOp(op[1], n[c(1, 2)])
      r2 <- fOp(op[2], c(n[3], r1))
      result <- fOp(op[3], c(r2, n[4]))
    } else if (g == 3) {
      r1 <- fOp(op[1], n[c(1, 2)])
      r2 <- fOp(op[2], c(r1, n[3]))
      result <- fOp(op[3], c(n[4], r2))
    } else if (g == 4) {
      r1 <- fOp(op[1], n[c(1, 2)])
      r2 <- fOp(op[2], c(n[3], r1))
      result <- fOp(op[3], c(n[4], r2))
    } else if (g == 5) {
      r1 <- fOp(op[1], n[c(1, 2)])
      r2 <- fOp(op[2], n[c(3, 4)])
      result <- fOp(op[3], c(r1, r2))
    } else if (g == 6) {
      r1 <- fOp(op[1], n[c(1, 2)])
      r2 <- fOp(op[2], n[c(3, 4)])
      result <- fOp(op[3], c(r2, r1))
    }
    
    if (result == 24) {
      print(paste0("Success on iteration # ", i))
      print(n)
      print(op)
      print(paste("order:", orders[g]))
    }
    
    # Thousand iterations counter
    if (i %% 1000 == 0) print(i)
    i <- i+1
  }
)

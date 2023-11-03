# Fibonacci Function

    #' Generate Fibonacci Sequence
    #'
    #' Generates a Fibonacci sequence with a specified number of terms. The sequence starts with two given starting values and each subsequent term is the sum of the previous two.
    #'
    #' @param n The number of terms in the Fibonacci sequence to generate.
    #'   This parameter is named `n` as it is a conventional way to denote the number of terms in sequences and iterations.
    #' @param start_values A numeric vector of length 2 to specify the first two starting values of the sequence.
    #'   The name `start_values` was chosen to clearly indicate its role in initializing the sequence.
    #' @return A numeric vector containing the first `n` terms of the Fibonacci sequence.
    #' @examples
    #' generate_fibonacci(n = 10)
    #' generate_fibonacci(n = 10, start_values = c(2, 3))
    #' 
    #' @export

    generate_fibonacci <- function(n, start_values = c(0, 1)) {
      if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
        stop("The number of terms 'n' must be a positive integer")
      }
      
      # Validate the start values
      if (!is.numeric(start_values) || length(start_values) != 2){
        stop("Start values must be a numeric vector length 2")
      }
      if (n == 1) {
        return(start_values[1])
      }
      if (n == 2) return(start_values)
      
      # Initialize the Fibonacci sequence with the start values
      fibonacci <- numeric(n)
      fibonacci[(1:length(start_values))] <- start_values
      
      # Calculate the rest of the Fibonacci numbers
      for (i in (length(start_values) + 1): n) {
        fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
      }
      
      return(fibonacci)
    }

# Sample Use

    # Example 1: Generate the first 10 Fibonacci numbers
    fibonacci_sequence <- generate_fibonacci(n = 10)
    print(fibonacci_sequence)

    ##  [1]  0  1  1  2  3  5  8 13 21 34

    # Example 2: Generate a Fibonacci sequence with custom starting values
    custom_start_sequence <- generate_fibonacci(n = 10, start_values = c(2, 3))
    print(custom_start_sequence)

    ##  [1]   2   3   5   8  13  21  34  55  89 144

    # Example 3 of an error when a negative number is provided for 'n'
    generate_fibonacci(n = -5)

    ## Error in generate_fibonacci(n = -5): The number of terms 'n' must be a positive integer

    # Example 4 of an error when n is a string
    generate_fibonacci(n = "ten")

    ## Error in generate_fibonacci(n = "ten"): The number of terms 'n' must be a positive integer

    # Example 5: Generate a Fibonacci sequence with only one term
    single_term_sequence <- generate_fibonacci(n = 1)
    print(single_term_sequence)

    ## [1] 0

# Test

    library(testthat)

    test_that("generate_fibonacci behaves correctly", {
      
      # Test 1: Check correct output with default start values
      expect_equal(generate_fibonacci(n = 5), c(0, 1, 1, 2, 3))
      
      # Test 2: Check correct output with custom start values
      expect_equal(generate_fibonacci(n = 5, start_values = c(2, 3)), c(2, 3, 5, 8, 13))
      
      # Test 3: Check that the function handles non-numeric input correctly
      expect_error(generate_fibonacci(n = "five"))
      
      # Test 4: Check that the function handles negative numbers correctly
      expect_error(generate_fibonacci(n = -5))
      
      # Test 5: Check that the function handles a single term correctly
      expect_equal(generate_fibonacci(n = 1), 0)
      
      # Test 6: Check that the function handles a zero term correctly
      expect_error(generate_fibonacci(n = 0))
      
      # Test 7: Check that the function handles non-numeric start values correctly
      expect_error(generate_fibonacci(n = 5, start_values = c("a", "b")))
    })

    ## Test passed 🌈

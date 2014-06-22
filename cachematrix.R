## The function is created for Assignment 2 in R Programming course on Coursera
## 2014-06-22

## The function creates a list with original matrix
## and it's inverse as a second element of the list

makeCacheMatrix <- function(x = matrix()) {
    y <- list(matrix = x, inverse = solve(x))
    y
}

## Function cacheSolve tests if a second element in the list is matrix
## If the second element in the list is matrix, both elements are multiplied
## and the result of multiplication is tested if it is an identity matrix
## to confirm that the second element is indeed an inverse of the first element

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(is.matrix(x$inverse)) {
        z <- round(x$matrix %*% x$inverse, 10)
        if (all(diag(z) == 1)) {
            print("Available inverse. Returning... ")
            x$inverse
        }
        else {
            print("still have to solve matrix")
            solve(x$matrix)
        }
    }       
}



# Test the function

matrixLength <- 1000
data <- matrix(round(runif(matrixLength^2, min = 0, max = 10)), nrow = matrixLength, ncol = matrixLength)
x <- makeCacheMatrix(data)

# Test if the inverse is returned from cache
cacheSolve(x)
# Test if inverse is calculated if not available
x <- list(matrix = data, inverse = data)
cacheSolve(x)
# Coursera: R-Programming Week 3

# Matrix inversion is usually a costly computation and 
# there may be some benefit to chaching the invserve of
# a matrix, rather than compute it repeatedly. 

# Assignment: Write a pair of functions that cache the 
# inverse of a matrix.

# Function 1: makeCacheMatrix
# This function creates a special matrix object that can 
# cache its inverse. The function does the following:
# 1. Creates a matrix A
# 2. Sets the value A 
# 3. Gets the value A
# 4. Sets the value of the A's inverse matrix B
# 5. Gets the value of B
# 6. Creates a list in which it assigns names to the 
#    setter and getter functions and returns this list 
#   to the parent environment.

makeCacheMatrix <- function(A = matrix()) {
    B <- NULL
    set <- function(C) {
        A <<- C
        B <<- NULL
    }
    
    get <- function() A 
    setInverseMatrix <- function(InverseMatrix) B <-- InverseMatrix
    getInverseMatrix <- function() B
    list (set = set, get = get,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)
    
}

# Function 2: This function can do two things:
# 1. It retrieves the inverse matrix B cached by 
#    makeCacheMatrix.
# 2. It computes the values for B itself if the cache of
#    makeCacheMatrix is empty.
# Important: It is assumed in this assignment that A is 
#            always an invertible matrix. There are therefore   
#            no operations in cacheSolve that deal with the 
#            possibility of A not being invertible.

cacheSolve <- function(A, ...) {
    B <-A$getInverseMatrix()
    if(!is.null(B)) {
        message ("getting cached data")
        return(B)
    }
    
    computeInverse <- A$get()
    B <- solve(computeInverse, ...)
    A$setInverseMatrix(B)
    B
}

# If you want to test whether the functions work
# correctly, you can run the following commands
# in your console. 

# A <- matrix(c(1, 0, 5, 2, 1, 6, 3, 5, 0), 
#      nrow = 3, ncol = 3)
# B <- makeCacheMatrix(A)
# S <- cacheSolve(B)
# print(S)
# This should give you the following result: 

#      [,1] [,2] [,3]
# [1,]   -6  3.6  1.4
# [2,]    5 -3.0 -1.0
# [3,]   -1  0.8  0.2

# Thanks a lot for grading my assignment!
# Have a nice weekend! :-) 
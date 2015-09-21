## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function represents some implementation of object with data 
## and primitive setters and getters functions.

## Of course in the real life we should check integrity of data. So it means that 
## the set() function for inverted matrix should calculate inverted matrix based upon
## x value
makeCacheMatrix <- function(x = matrix()) {
  # default value for inverted matrix is NULL
  invMatrix <- NULL
  # function set() is setting a new matrix to a variable and cleaning old value of inverted matrix  
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  # function get() returns the original matrix
  get <- function() x
  # function setInvMatrix() sets a new inverted matrix
  setInvMatrix <- function(inverted) invMatrix <<- inverted
  # function getInvMatrix() returns inverted matrix
  getInvMatrix <- function() invMatrix
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
## The function cacheSolve() incapsulates getting of inverted matrix for "object" x
## It checks is the inverted matrix existing and returns that value or calculates and
## returns a new value 
## Recall that object x  contains an original matrix, inverted matrix and functions 
## for getting/setting these values  
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
## 1. Create m object
## > m<-makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))

## 2. Get the original matrix
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

## 3. Calculate inverted matrix 
## The first time the inverted matrix will be calculated really  
##> invM<-cacheSolve(m)

## 4. Display inverted matrix from interim variable
##> invM
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## 5. Display original matrix
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

## 6. Display inverted matrix from "object" variable.
##> m$getInvMatrix()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## 7. Set a new original matrix
## > m$set(invM)

## 8. Prove that inverted matrix is deleted
##> m$getInvMatrix()
##NULL

## 9. Calculate a new inverted matrix based upon old inverted matrix 
##> invM<-cacheSolve(m)

## 10. Prove that after double inversion we get the original matrix 
##> invM
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4


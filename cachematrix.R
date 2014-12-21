## Programming Assignment 2
## ------------------------------------------------------------------------------

## Purpose:     As matrix inversions can be a taxing computation, the purpose of 
##              this exercise is to demonstrate the use of lexical scoping to 
##              cache an inverse matrix after calculation, and as a result,  
##              save system resources on additional computations using the result. 

## Description: The 'makeCacheMatrix' and 'CacheSolve' functions shall compute,
##              cache, and output the inverse of a defineable matrix, 'x'. After
##              the initial computation, all following outputs shall retrieve the
##              inverse matrix from the cache.

## Note: Both the below functions have been modeled based on the example provided
##       within the Assignment by Roger D. Peng.
## ------------------------------------------------------------------------------

## The 'makeCacheMatrix' function creates a matrix object, as defined by  
## 'x', and is able to cache the inverse after the subsequent  
##  execution of the 'cacheSolve' function (as defined below).  

makeCacheMatrix <- function(x = matrix( )) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmtrx <- function(solve) m <<- solve
  getmtrx <- function() m
  list(set = set, get = get, setmtrx = setmtrx, getmtrx = getmtrx)
}

## The 'cacheSolve' function will compute and output the inverse of the matrix 
## returned by the 'makeCacheMatrix' function. Note: if the inverse has  
## already been calculated, the function will output the text,  
## "getting cached data", and retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        
  m <- x$getmtrx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmtrx(m)
  m
}



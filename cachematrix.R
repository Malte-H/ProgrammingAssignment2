## Assignment Week 3 by Malte-H

## Put comments here that give an overall description of what your
## functions do

## in this Assignment we write two functions to cache a reversed matrix. The idea behind that is
## that in more complicated scenarios it may be of help to run the code more efficient to cache results of
## a calculation to later on have access to the result again without having to compute it over and over again. 
## so these two functions are designed to inverse a matrix, save the inversion if not cached before, and if already
## cached just print the cached result. 

## Write a short comment describing this function
## This function basically includes the cache and several functions to get the cache or to set it. 
## Without the cacheSolve function it would be not of much use since, the calculation we want to do
## namely to inverse a matrix is done in the cacheSolve matrix.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse, getinverse =getinverse)
}


## The cacheSolve requires a matrix as its x. Then it checks whether we already have the reversed matrix cached
## in our makeCacheMatrix formula. If not it reverses the matrix with the solve function, sets the result in the cache
## of the makeCachefunction and prints the inverted matrix. If we call the function a second time, it will
## indicate that it is getting the cached data and returns it. 

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        datama <- x$get()
        inver <- solve(datama, ...)
        x$setinverse(inver)
        inver
        
}

## function test with test matrix according to the help section of the course.


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)

############################################################################################################
## This file contains 2 functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## See the details on the comments or each function below.
##
## Author: Bruno Alessi
############################################################################################################

## Examples of use (">" is the prompt in R Console):
##
## (1) Valid matrix (square, with nonzero determinant)
##
## > m1 <- matrix(c(1,2,2,1),2,2)
## > cm1 <- makeCacheMatrix(m1)
## > cacheSolve(cm1)
## Calculating the inverse matrix and puting it on the cache...
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > cacheSolve(cm1)
## Getting the inverse matrix from the cache
##   
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## 
## (2) Invalid matrix (square, determinant is zero)
## > m2 <- matrix(c(1,1,1,1),2,2)
## > cm2 <- makeCacheMatrix(m2)
## > cacheSolve(cm2)
## Calculating the inverse matrix and puting it on the cache...
## Error in solve.default(data, ...) : 
##   Lapack routine dgesv: system is exactly singular: U[2,2] = 0
##
## (3) Invalid matrix (non square: 2 rows x 3 columns)
## > m3 <- matrix(1:6,2,3)
## > cm3 <- makeCacheMatrix(m3)
## > cacheSolve(cm3)
## Calculating the inverse matrix and puting it on the cache...
## Error in solve.default(data, ...) : 'a' (2 x 3) must be square
##


## This function creates a list of acessor functions that will
## be used to set the matrix and its inverse into the cache and
## to get the matrix and its inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <- NULL
	}
	get <- function() x
	set_i <- function(i) inv <<- i
	get_i <- function() inv
	list(get_matrix=get, set_matrix=set, set_inv=set_i, get_inv=get_i)
}

## This function returns a matrix that is the inverse of the givem matrix 'x'.
## If the matrix x is passed for the first time, its inverse (providing it
## exists - see comments below) is calculated and the result is cached.
## The next calls for the same 'x' will return the previously calculated
## inverse from the cache, instead of calculating it again.
## The inverse matrix of a matrix A is a matrix B such that AB = BA = In
## A and B must be square matrixes and the determinant of A must be nonzero
## To test if the determinant of x is or isn't 0, call det(x)
## If det(x) != 0, the following error will be thrown:
## "Lapack routine dgesv: system is exactly singular: U[<nrows>,<ncols>] = 0"
cacheSolve <- function(x, ...) {
	i <- x$get_inv()
	if (!is.null(i)) {
		message("Getting the inverse matrix from the cache")
		return(i)
	}
	message("Calculating the inverse matrix and puting it on the cache...")
      data <- x$get_matrix()
      i <- solve(data, ...)
      x$set_inv(i)
      i
}


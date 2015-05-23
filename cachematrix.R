## Objective: To write a pair of functions that cache the inverse
## of a matrix.

#Create a matrix

>a <- matrix(c(1,2,3,4), nrow=2, ncol=2)

#Test matrix

>a
[,1] [,2]
[1,]    1    3
[2,]    2    4

#Create makecachematrix() function

## makecachematrix () will create a special "matrix" object that can cache its inverse.

>makecachematrix <- function(x =matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#Create a value for makecachematrix() result.

> b <- makecachematrix(a) #a is the matrix

#create a cacheSolve() function

## cacheSolve() computes the inverse of the special "matrix" returned by makecachematrix().

>cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                message("cache data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
        ## Return a matrix that is the inverse of 'x'

#test cacheSolve()

>cacheSolve(b) #b are the results from the makecachematrix function.

[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

#test cacheSolve() again to get the cached value

cache data #message that says that the values comes from cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


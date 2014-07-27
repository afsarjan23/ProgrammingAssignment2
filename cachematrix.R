## If the two matrices are same then it returns the values from the memory, it doesn't calculate 
## it again.

## This function calculates the inverse

makeCacheMatrix<- function(x = matrix) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve){i <<-solve} 
        get_inverse <- function() i
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## If the two matrices are same then it returns the result from the memory instead of calculating it again.

cacheSolve  <- function(x, ...) {
        i <- x$get_inverse()
        y<- set()
        if(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))  {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inverse(i)
        i   
        
        
}
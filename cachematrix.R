makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {			## Stores the matrix that should be inverted in the variable "x" into the main function "makeCacheMatrix"
                x <<- y
                s <<- NULL			## Cleans any stored inverted matrix from a previous matrix stored in "x" in the main function "makeCacheMatrix"
        }
        get <- function() x			## Returns the matrix that should be inverted (stored in "x")
        setsolve <- function(solve) s <<- solve ## Stores the inverted matrix in the variable "s" into the main function "makeCacheMatrix"
        getsolve <- function() s 		## Returns the inverted matrix stored in "s" in the "setsolve" function
        list(set = set, get = get, 		## Function list to store the four functions in the function "makeCacheMatrix"
             setsolve = setsolve,
             getsolve = getsolve)
}


cacheSolve <- function(x, ...) { 		## Function that checks if the matrix has already been inverted before and retrieves data if it has, otherwise it will invert the matrix
        s <- x$getsolve() 			## Retrieves what is stored in the "getsolve" function from "makeCacheMatrix" function and assigns it to "s"
        if(!is.null(s)) { 			## If an inverted matrix was stored in "getsolve" the inverted matrix will be retrieved and printed
                message("getting cached data")
                return(s) 			## Retrieved inverted matrix will be printed and the function stopped, instead of inverting the matrix again
        }
        data <- x$get() 			## If there is no saved inverted matrix the matrix in "get" will be retrieved and assigned to "data"
        s <- solve(data, ...) 			## Inverting the matrix in "data" and assigning it to s
        x$setsolve(s) 				## Printing the inverted matrix
        s
}

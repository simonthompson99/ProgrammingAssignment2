#function to calculate inverse of matric and cache for future use
#makecachematrix creates the object
#cachesolve returns either a new solving of the matrix, or a cached version

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL #reset inv
        setmat <- function(y){ #allows setting of a new matrix
                x <<- y #overwrites original matrix
                inv <<- NULL #resets inv
        }
        getmat <- function() x #function to return the current matrix
        setinv <- function(invmat) inv <<- invmat #accepts inverted matrix from cacheSolve, and writes it into the makeCacheMatrix object
        getinv <- function() inv #returns the cached matrix
        list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv) #output is list of objects, allows use of $ to refer to individual objects
}

cacheSolve <- function(x, ...){
        inv <- x$getinv() #gets a cached matrix (if there is one)
        if(!is.null(inv)) { #checks to see if there is a cached matrix, returns it, then exits
                message("getting cached inverse")
                return(inv)
        }
        datamat <- x$getmat() #if no cached stored, then gets the matrix
        inv <- solve(datamat) #gets the inverse
        x$setinv(inv) #writes it to the cached inverse matrix
        inv #returns the inverse
}

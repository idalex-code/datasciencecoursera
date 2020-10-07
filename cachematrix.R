#install package matlib for using inv() function
install.packages("matlib")
library(matlib)


makeCacheMatrix <- function(x=matrix()){
        #cached inverse of our matrix x
        invmat<- NULL
        get <- function() x
        set <- function(y){
                x <<- y
                invmat <- NULL
        }
        getInverse <- function() invmat
        setInverse <- function(inverse) invmat <- inverse
        
        #here is our list with four functions
        list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}

# these function calculates the inverse of the matrix
cacheSolve <- function(x, ...){
        
        #we assign to invmat the value from the getInverse function
        invmat <- x$getInverse()
        
        #if invmat is not a null object, than it will be return the value 
        #of the inverse of the matrix
        if(!is.null(invmat)){
                message("getting cached data")
                return(invmat)
        }
        
        #if invmat is an null object, than we assign to k the value
        #of the matrix x and calculate with inv() the inverse of x
        k <- x$get()
        invmat <- inv(k)
        
        # we save the value of the inverse in the cache and return the inverse
        x$setInverse(invmat)
        return(invmat)
}

#here is an example for the 2x2 matrix with values 1,2,3,4, sorted by rows:
cacheSolve(makeCacheMatrix(matrix(1:4,ncol = 2,nrow = 2, byrow = TRUE)))
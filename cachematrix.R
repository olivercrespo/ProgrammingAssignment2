makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting data from cache...\n")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## makeCacheMatrix should create a special matrix object and cache the values generated for x




makeCacheMatrix <- function(x = matrix()) {
        inv=NULL
        set=function(y)
        {
                x<<-y
                inv<<-NULL
        }
        get=function(){return(x)}
        setInverse=function(i){inv<<-i}
        getInverse=function(){return(inv)}
        output=list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
        return(output) 
        
}


## CacheSolve should return the inverse of the Matris created by the makeCacheMatrix function.
## If the inverse value is avaiable in the cache, it will pull it from there, otherwise, it should calculate it, cache it and then return it.
## it also give a nice message telling you it is retrieving data from cache if there is any data there :)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(inv))
        {
                message("getting data from cache...\n")
                return(inv)
        }
        data=x$get()
        inv=solve(data,...)
        x$setInverse(inv)
        return(inv)
}

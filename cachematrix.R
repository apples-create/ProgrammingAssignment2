##A pair of functions that cache the inverse of a matrix.
##1. makeCacheMatrix()->creates a special 'matrix' object that can cache its inverse.
##2. cacheSolve()->computes the inverse of the 'matrix' in makeCacheMatrix().


## The function below, 'makeCacheMatrix', creates a 'special' matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  #Parent function, creates a special 'matrix' object that can cache its inverse. 
        #X is a square invertible matrix.
        inv<-NULL #Initializing inv as an object within the 'makeCacheMatrix' environment to be used by later code in the function.
        set<-function(y){ #Setting the value of the matrix named as y. 
                x<<-y  #Double arrow assignment operator used to assign the value y to the object x in the parent environment.
                inv<<-NULL #Assigning the NULL value to the inv object to clear any values of inv cached in the memory of the object 
                #when x is reset.
        }
        get<-function() {x} #Getting the value of the matrix and returning the matrix.
        setinv<-function(inverse) {inv<<-inverse} #Setting the value of the inverse and assigning inverse to the value of inv 
        #in the parent environment.
        getinv<-function() {inv} #Getting the value of the inverse of the matrix and returning the inverse property.
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv) #Creating a list that assigns each of these functions as elements within it, it then  returns it 
        #to the parent environment. We can now make use of the $.
#This list will be used as the input to the function 'cacheSolve' below.
}


##Computing the inverse of the 'special' matrix created by the function 'Makecachematrix':

cacheSolve <- function(x, ...) { #X is the output of the function 'makeCacheMatrix'. Ellipsis is used to allow for the caller to pass 
        #additional arguments into the function.
        inv<-x$getinv() #Returning a matrix that is the inverse of 'x' and assigning it into inv. Afterwards it will check to see if 
        #the result is NULL.
        if(!is.null(inv)){
                message("getting cached data") #If the inverse has been calculated already and the matrix has not changed 
                #then the 'cacheSolve' function will show the inverse from the cache skipping the computation.
                return(inv)
        }
        #If the inverse has not been already calculated, we calculate the inverse:
        matdata<-x$get() #Getting the matrix from our object.
        inv<-solve(matdata, ...) #Computing the inverse of the matrix using the solve function.
        x$setinv(inv) #Using the setinv function to set the value of the inverse in the cache.
        return(inv) #Return a matrix that is the inverse of 'x'.
}


        


##Teting our functions
mymatrix<-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))#Assigning the 'makeCacheMatrix' function to a variable called mymatrix.
cacheSolve(mymatrix) #Returning inverse after computation
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

cacheSolve(mymatrix)#Returning inverse from cache and skipping computation
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

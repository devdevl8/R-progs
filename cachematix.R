makeCacheMatrix<- function(x = matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInv<-function(p) m<<-p
  getInv<-function() m
  list(set=set,get = get,
       setInv=setInv,
       getInv=getInv)
}
#i used the solve fn to get the inverse
cacheSolve<- function(x, ...){
  m<-x$getInv()
  if (!is.null(m)){message("getting cached data")
    return(m)}
  mat<-x$get()
  inv<-solve(mat)
  x$setInv(inv)
  print(inv)
}
my_mat<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(my_mat)
#1 
#(a)
tsEwma <- function( tsDat, m0=0, delta=0.7)
{
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=start(tsDat), frequency=frequency(tsDat))
}
#(b)
tsEwma2 <- function( tsDat, m0=0, delta=0.7)
{
  tsPars <- tsp(tsDat)
  tsDat <- c(tsDat)
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=tsPars[1], frequency=tsPars[3])
}

#2 
#(a)
myListFn <- function(n)
{
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}
#(b) 
myList <- lapply( rep(10,4), myListFn ) 
myMatrix <- sapply( rep(10,4), myListFn ) 
#(c) 
myList <- lapply( rep(10,1000), myListFn )
#Here are three equivalent answers: 
lapply(myList, FUN=function(x){x[[2]]})
lapply(myList, FUN="[[", 2)
lapply(myList, FUN="[[", "yVec")
#(d) 
#Here are six equivalent answers:
sapply(myList, FUN="[[", 2)
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), 2)
sapply(myList, FUN=function(x){x[[2]]})
vapply(myList, FUN=function(x){x[[2]]}, FUN.VALUE=rep(0,10))
sapply(mList, FUN="[[", "yVec")
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), "yVec")
#(e)
myList2 <- lapply(myList, function(x){list(xVec=x$xVec, yVec=x$yVec)})
#(f) This code picks out the indices of those lists which satisfy the condition: 
which( unlist( lapply(myList, function(x){x[[3]]>2}) ) )
#So this is an answer:
myList[which(  unlist(lapply( myList, function(x){x[[3]]>2} ))  )]

#3 
#(a)
partA <- sapply(myList, function(x){ sum(x$xVec*(1:10))/sum(x$yVec*(1:10)) })
#(b) 
#3 possible solutions:
myMat <- t(sapply( myList, function(x){x$xVec-x$yVec})) 
myMat2 <- matrix( unlist( lapply(myList, FUN="[[",1) ) - 
                    unlist( lapply(myList, FUN="[[",2) ), nc=10, by=T  )
myMat3 <- matrix(  unlist(lapply(myList, function(x){x$xVec-x$yVec})), nc=10, by=T  )
#(c) 
#Here is a quick solution using sapply:
sum(sapply(myList, function(x){x$xVec[2]})*(1:1000)) /
  sum(sapply(myList, function(x){x$yVec[2]})*sapply(myList, function(x){x$count}))

myDf <- data.frame(myList)
myDf[2,  seq(1,3000,by=3)]

myMat <- as.matrix(data.frame(myList))
names(myMat) <- NULL
sum((1:1000) * myMat[2,seq(1,3000,by=3)])/
  sum(myMat[2,  seq(3,3000,by=3)] * myMat[2,  seq(2,3000,by=3)])
 
sum( (1:1000) * myMat[2,c(T,F,F)] )/sum( myMat[2,c(F,F,T)] * myMat[2,c(F,T,F)] ) 

#4
#(a) 
#The code 
apply(testArray, c(2,3), min)    

sweep(testArray, c(2,3), apply(testArray, c(2,3), min))

apply(testArray, c(2,3), sum) - apply(testArray, c(2,3), max)
#or
#apply(testArray, c(2,3), FUN=function(x){ sum(x) - max(x)})

testFn2 <- function(xArray)
{
  wArray <- sweep(testArray, c(2,3), apply(testArray, c(2,3), min))
  zArray <- apply(testArray, c(2,3), FUN=function(x){ sum(x) - max(x)})
  list(wArray=wArray, zArray=zArray)
}
#(b) 
tmp <- apply(testArray, c(1,2), FUN=function(x){ x^(1:length(x))})

apply(tmp, c(3,1), sum)
#Hence our function is
testFn <- function( xArray)
{
  apply( apply(xArray, c(1,2), FUN=function(x){x^(1:length(x))}), c(3,1), sum )
}

#5 
#(a)
shift <- function(X,a,b){
  X[,1] <- X[,1] + a
  (b)
  X[,2] <- X[,2] + b
  X
}
#(b)
rotate <- function(X,r){
  X%*%matrix(c(cos(r), -sin(r), sin(r), cos(r)), nrow = 2)
}

A <- cbind(c(0,1,2,4/9,14/9), c(0,3,0,4/3,4/3)) 

#(c) 
arrayA<-vapply(1:25,
               FUN=function(i){
                 rotate(A,2*pi*(i-1)/24)
               },
               matrix(0,nrow=5, ncol=2)
)
#(1)
plot(c(-10,10), c(-10,10), ann=F, type='n')
for(i in 1:25)
  drawA(arrayA[,,i])
#or
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))

plot(arrayA[2,1,], arrayA[2,2,])
#(3)
plot(1:25, arrayA[2,1,])

#(d)
scale <- function(X,a,b){
  X%*%matrix(c(a,0,0,b), nrow=2)
}
arAscaled <- vapply(1:25,
                    FUN=function(i){
                      scale(arrayA[,,i],2,3)
                    },
                    matrix(0,nrow=5, ncol=2)
)
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
invisible(sapply( 1:25, FUN=function(i){ drawA(arAscaled[,,i]) } ))

#(e) 
arArandom <- array(0, dim=c(5,2,25))
arArandom[,,1] <- A
for(i in 2:25){
  arArandom[,,i] <-
    shift(
      rotate(
        scale(arArandom[,,i-1], runif(1,0.5,1.5),runif(1,0.5,1.5)),
        2*pi*runif(1,-1,1)
      ),
    runif(1,-1,1), runif(1,-1,1)
    )
  }
#Then create an animation:
oopt = ani.options(interval = 0.2, nmax = 25)
for (i in 1:ani.options("nmax")){
  plot(c(-10,10), c(-10,10), ann=F, type='n')
  drawA(arArandom[,,i])
  ani.pause()
}





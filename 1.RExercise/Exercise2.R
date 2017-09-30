#1
#(a)
( tmp <- matrix( c(1,5,-2,1,2,-1,3,6,-3),nr=3) )
tmp%*%tmp%*%tmp
#(b) 
tmp[,3] <- tmp[,2]+tmp[,3]

#2
tmp <- matrix(c(10,-10,10), b=T, nc=3, nr=15)
t(tmp)%*%tmp
#or 
crossprod(tmp)

#3
matE <- matrix(0,nr=6,nc=6)
matE[ abs(col(matE)-row(matE))==1 ] <- 1

#4 
outer(0:4,0:4,"+")

#5
#(a) 
outer(0:4,0:4,"+")%%5
#or
matrix(0:4+rep(0:4,times=rep(5,5)),nc=5)
#(b) 
outer(0:9,0:9,"+")%%10
#(c) 
outer(0:8,0:8,"-")%%9

#6
yVec <- c(7,-1,-3,5,17)
AMat <- matrix(0,nr=5, nc=5)
AMat <- abs(col(AMat)-row(AMat))+1

#7
set.seed(75)
aMat <- matrix( sample(1:10, size=60, replace=T), nr=6)
#(a) 
apply(aMat, 1, function(x){sum(x>4)})
#(b) 
which( apply(aMat,1,function(x){sum(x==7)==2}) ) 
#(c) Here are two solutions:
aMatColSums <- colSums(aMat)

cbind( rep(1:10,rep(10,10)), rep(1:10,10) ) [outer(aMatColSums,aMatColSums,"+")>75,]
#or
aMatColSums <- colSums(aMat)
which( outer(aMatColSums,aMatColSums,"+")>75, arr.ind=T )

aMatColSums <- colSums(aMat)
logicalMat <- outer(aMatColSums,aMatColSums,"+")>75
logicalMat[lower.tri(logicalMat,diag=F)] <- F
which(logicalMat, arr.ind=T)

#8
#(a) 
sum( (1:20)^4 ) * sum( 1/(4:8) ) 
#or 
sum(outer((1:20)^4,4:8,"/"))
#(b) 
sum( (1:20)^4 / (3 + outer(1:20,1:5,"*")))
#(c) 
sum( outer(1:10,1:10,function(i,j){ (i>=j)*i^4/(3+i*j) }) )







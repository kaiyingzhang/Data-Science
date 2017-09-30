#1 
#(a) 
1:20 
#(b) 
20:1
#(c) 
c(1:20,19:1)
#(d) 
tmp <- c(4,6,3)
#(e) 
rep(tmp,10)
#(f) 
rep(tmp,l=31)
#(g) 
rep(tmp,times=c(10,20,30))
rep(tmp,each=10)

#2
tmp <- seq(3,6,by=0.1) 
exp(tmp)*cos(tmp)

#3 
#(a)
(0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3)) 
#(b) 
(2^(1:25))/(1:25)

#4 
#(a)
tmp <- 10:100
sum(tmp^3+4*tmp^2)
#(b)
tmp <- 1:25
sum((2^tmp)/tmp + 3^tmp/(tmp^2))

#5
#(a) 
paste("label", 1:30)
#(b) 
paste("fn", 1:30, sep="")

#6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
#(a) 
yVec[-1] - xVec[-length(xVec)]
#(b) 
sin(yVec[-length(yVec)]) / cos(xVec[-1])
#(c) 
xVec[-c(249,250)] + 2*xVec[-c(1,250)]-xVec[-c(1,2)] 
#or, for an answer which works whatever the length of xVec,
xVecLen <- length(xVec)
xVec[-c(xVecLen-1,xVecLen)] + 2*xVec[-c(1,xVecLen)] - xVec[-c(1,2)] 
#(d) 
sum(exp(-xVec[-1])/(xVec[-length(xVec)]+10))

#7 
#(a) 
yVec[yVec>600]
#(b) 
(1:length(yVec))[yVec>600]
#or 
which(yVec>600) 
#(c) 
xVec[yVec>600]
#(d) 
sqrt(abs(xVec-mean(xVec)))
#(e) 
sum( yVec>max(yVec)-200 )
#(f) 
sum(xVec%%2==0)
#(g) 
xVec[order(yVec)]
#(h) 
yVec[c(T,F,F)]

#8 
1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))





#-------------------------------------------------------------------------------------------------

# GAMMA DISTRIBUTION
# USE CASE:
#Suppose you are in a casino and you expect to get a reward of game machine once every 1/2 hour. Compute the 
#probability of the time that you will have to wait before you get 5 rewards.

x<-seq(0,24,length.out=100)
y<-dgamma(x,5,2)

plot(x,y,xlim=c(0,24),ylim=c(0,1),type='l',ylab='probability',xlab='time to wait',main="The Gamma Density Distribution")

#done
#-------------------------------------------------------------------------------------------------


# NORMAL DISTRIBUTION
# USE CASE:
# An average light bulb manufactured by a company lasts 23 days with a standard deviation of 10 days. 
# Assuming that bulb life is normally distributed, what is the probability that an Acme light bulb will last at most 1 to 40 days			

Xaxis_NoOfDays <- seq (1, 40, by=1)
Yaxis_Probablity <- dnorm(Xaxis_NoOfDays,23,10,FALSE)
plot( Xaxis_NoOfDays ,Yaxis_Probablity,col="red",type ="l",
      xlab='days',ylab='probability',main="The Normal Density Distribution")

#done
#-------------------------------------------------------------------------------------------------


# BINOMIAL DISTRIBUTION
# USE CASE:
#lndividuals with a certain gene have a 0.3 probability of eventually contracting a 
#heart disease. If 100 individuals with the gene participate in a lifetime study, then the 
#distribution describing the number of individuals who will contract the disease.

x<-seq(0,100,1)
y<-dbinom(x,100,0.3)
plot(x,y,type ="h",xlab='number of individuals',ylab = 'probability of contracting heart disease',
     main="The Binomial Density Distribution")


#done
#-------------------------------------------------------------------------------------------------


# BETA Distribution
# USE CASE:
#Suppose the batting average of hitting baseballs is 0.26, and the professional athlete can get above 0.3.
#Now before an athlete batting the ball, the history show his hitting rate is between 0.21 to 0.35, how do 
#we estimate his hit rate of this year in advance?

x<-seq(0,1,length.out=10000)
y<-dbeta(x,27,73)# 27/27 + 73= 0.27

plot(x,y,col="red",xlim=c(0,0.5),ylim=c(0,20),type='l',
     xaxs="i", yaxs="i",ylab='density of hitting rate',xlab='hittint rate',
     main="The Beta Density Distribution")

#done
#-------------------------------------------------------------------------------------------------
# Chi-SQUARE Distribution
# USE CASE:
#In some little casino, they will show people about the probability of getting rewards.
#Now we got the paper about the probability and do some research about customers and get the practical data
#How can we know that if the casino tells a lie?

x<-seq(0,15,length.out=1000)
y<-dchisq(x,5)

plot(x,y,col="red",xlim=c(0,15),ylim=c(0,2),type='l',
     xaxs="i", yaxs="i",ylab='density ',xlab='',
     main="The Chisq Density Distribution")

#done
#-------------------------------------------------------------------------------------------------
# UNIFORM Distribution
# USE CASE:
# There is a busy bus station in boston, everyday there are lots of buses pass through this station.
# Every 10 minutes a bus will pass through this station,how to know the probability of the time that
# the passengers should wait?

x<-seq(0,10,length.out=1000)
y<-dunif(x,0,10)

plot(x,y,col="red",xlim=c(0,10),ylim=c(0,1.2),type='l',
     xaxs="i", yaxs="i",ylab='density',xlab='',
     main="The Uniform Density Distribution")
#done
#-------------------------------------------------------------------------------------------------
# POISSON Distribution
# USE CASE:
# There is a busy bus station in boston, everyday there are lots of buses pass through this station.
# The probability of a car cause a car accident in a day is 0.0001, and there are about 100 cars pass
# this station every day, what is the probability of over 2 car accidents happened today ?

x <- seq (0,1000,by=1)
y <- dpois(x, 0.01, log = FALSE) 
plot(x,y,col="blue",ylab='probability',xlab='time of car accidents',xlim=c(0,10),ylim=c(0,1.000),type ="h",main="The Poisson Density Distribution")


#done
#-------------------------------------------------------------------------------------------------



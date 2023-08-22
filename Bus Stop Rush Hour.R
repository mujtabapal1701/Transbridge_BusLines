#Bus Stop on the Street
#Rush Hour
N=50 # Number of passengers

busCapacity= 20 # Trans bridge capacity
busStopCapacity= 10
passengers=0
passArrival= sample(0:120, N , replace=TRUE) ##sort the arrival times in ascending order
passArrival= sort(passArrival)
buses=0
busArrival=matrix(0,1,N)
wait=matrix(0,1,N)
maxWait=0
waitStart=passArrival[1]


for (i in 1:N)
{
  #every time a passenger arrives, add one to existing number of passengers
  passengers=passengers+1
  if(waitStart==0)
  {
    waitStart=passArrival[i]
  }
  else
  {
    maxWait=passArrival[i]-waitStart
  }
  
  if (busCapacity/2<passengers && maxWait >15 )
  {
    buses=buses+1
    maxWait=0
    waitStart=0
    busArrival[buses]=passArrival[i]
    for (k in 0:passengers)
    {
      wait[i-k]=busArrival[buses]-passArrival[i-k]
    }
    passengers=0
    
  }
  
  if(busCapacity<passengers) 
  {
    buses=buses+1
    maxWait=0
    waitStart=0
    busArrival[buses]=passArrival[i]
    for (j in 0:(busCapacity-1))
    {
      wait[i-j]=busArrival[buses]-passArrival[i-j]
    }
    passengers=0
  }
  
  else if(busStopCapacity<passengers) 
  {
    buses=buses+1
    maxWait=0
    waitStart=0
    busArrival[buses]=passArrival[i]
    for (j in 0:(busStopCapacity-1))
    {
      wait[i-j]=busArrival[buses]-passArrival[i-j]
    }
    passengers=0
    
  }
  
}
max(wait)
averageWait=sum(wait)/N

hist(wait,ylim=c(0,20),xlim=c(0,40),xlab="Mins Elapsed",ylab="Number of Passengers")


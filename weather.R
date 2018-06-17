library("markovchain")
weatherStates <- c("sunny", "cloudy", "rain")
byRow <- TRUE
weatherMatrix <- matrix(data = c(0.70, 0.2, 0.1,
                                 0.3, 0.4, 0.3,
                                 0.2, 0.45, 0.35), byrow = byRow, nrow = 3,
                                 dimnames = list(weatherStates, weatherStates))
mcWeather <- new("markovchain", states = weatherStates, byrow = byRow,
                    transitionMatrix = weatherMatrix, name = "Weather")

initialState <- c(0, 1, 0)
after2Days <- initialState * (mcWeather * mcWeather)
after7Days <- initialState * (mcWeather ^ 7)
after2Days

round(after7Days,3)

fvals<-function(mchain,initialstate,n) {
  out<-data.frame()
  names(initialstate)<-names(mchain)
  for (i in 0:n)
   {
       iteration<-initialstate*mchain^(i)
       out<-rbind(out,iteration)
       }
  out<-cbind(out, i=seq(0,n))
  out<-out[,c(4,1:3)]
  return(out)
  }

fvals(mchain=mcWeather,initialstate=c(80,10,5),n=4)

states(mcWeather)
names(mcWeather)
dim(mcWeather)

name(mcWeather)
name(mcWeather) <- "New Name"
name(mcWeather)

markovchain:::sort(mcWeather)
transitionProbability(mcWeather, "cloudy", "rain")
mcWeather[2,3]

plot(mcWeather)
mcDf <- as(mcWeather, "data.frame")
mcDf

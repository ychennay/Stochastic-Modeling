#function for time to failure (TTF)
F_X <- function(a, alpha=2, beta=812){
  return(1 - exp(-1*(a/ beta)^alpha))}

#inverse function for time to failure (TTF), generates a time to failure
F_X_inverse <- function (alpha=2, beta=812){
  U <- runif(1,0,1)
  a = beta * ((-log(1-U))^(1/2))
  return (a)}

#run the F_X_inverse function 50000 times and plot histogram to find out the distribution
# of TTF times
#hist(replicate(50000,F_X_inverse()))

#function for time to repair (TTR)
F_R <- function(lower_bound=4, higher_bound=48){
  return(runif(1, lower_bound, higher_bound))}

#run F_R function 50000 and plot in histogram the uniform distribution
#hist(replicate(50000,F_R()))

#plot(F_X(seq(0,1000,.01)))
#plot(F_X_inverse(seq(.01,1,.01)))



system_failure <- function(iterations){
TTF <- vector()
for (i in 1:iterations){
n <- 0
T_n <- 0 #set time of each event
S <- 0 # set state initially at 0 computers failing
state <- 0 #vector of states
while (S < 2){ #while the S is less than 2
C_failure = F_X_inverse() #clock to failure
C_repair = F_R() # clock to repair
if (S == 0){
  C_repair = Inf # if there are no failures, then C_repair is infinity
}

# select whatever the minimum time to be time elapsed until the next event
time_elapsed = min(C_failure, C_repair) 
if (time_elapsed == C_failure){
  S = S + 1 #if the failure comes earlier, then add a failed computer to the state
}
else if (time_elapsed == C_repair){
  S = S - 1 # if the repair comes earlier, then subtract a failed computer to the state 
}

#append the time elapsed to the overall time
T_n = append(T_n, (time_elapsed + T_n[length(T_n)])) 
#append the number of events (add one to the previous vector)
n = append(n, (1+n[length(n)]))
#append the state at n
state = append(state,S)
}
sprintf("Total system failure has occured on day %i", round(T_n[length(T_n)],0))
TTF <- append(TTF,round(T_n[length(T_n)]/365,0))
}
return(TTF)
}
TTF

TTF= system_failure(1000)
hist(TTF, breaks=50)

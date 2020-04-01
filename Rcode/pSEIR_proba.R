library(deSolve)

pseirmod = function(t, y, parms) {
  # Pull state variables from y vector
  S = y[1]
  E = y[2]
  I = y[3]
  R = y[4]
  # Pull parameter values from parms vector beta = parms["beta"]
  R0 = parms["R0"]
  Tinc = parms["Tinc"]
  Tinf = parms["Tinf"]
  sigma = 1/Tinf
  gamma = 1/Tinc
  beta = sigma*(R0/Tinc-1) 
  # Define equations
  dS = - beta * S * I
  dE = beta * S * I - sigma * E
  dI = sigma * E  - gamma * I 
  dR = gamma * I
  res = c(dS, dE, dI, dR)
  # Return list of gradients
  list(res)
}

times = seq(0, 30, by = 1/10)
parms = c(R0 =3,Tinc = 5.2, Tinf=2.9)
start = c(S = 0.99, E=0.01, I = 0, R = 0)

out = ode(y=start, times=times, func=pseirmod, parms= parms)
out = as.data.frame(out) 

plot(x=out$time, y=out$S, ylab="People", xlab="Time",type="l",ylim=c(0,1))
lines(x=out$time, y=out$E, col="purple") 
lines(x=out$time, y=out$I, col="red") 
lines(x=out$time, y=out$R, col="green") 
legend("topright", legend=c("S","E" ,"I", "R"), 
  lty=c(1,1,1,1), col=c("black","purple" , "red", "green"))
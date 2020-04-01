library(deSolve)

seirmod = function(t, y, parms) {
  # Pull state variables from y vector
  S = y[1]
  E = y[2]
  I = y[3]
  R = y[4]
  # Pull parameter values from parms vector beta = parms["beta"]
  mu = parms["mu"]
  gamma = parms["gamma"]
  N = parms["N"]
  beta = parms["beta"]
  sigma = parms["sigma"]
  # Define equations
  dS = mu * (1 - S) - beta * S * I
  dE = beta * S * I - (mu + sigma) * E
  dI = sigma * E  - (mu + gamma) * I 
  dR = gamma * I - mu * R
  res = c(dS, dE, dI, dR)
  # Return list of gradients
  list(res)
}

times = seq(0, 30, by = 1/10)
parms = c(mu = 0.01, N = 1, beta = 2, gamma = 1/3, sigma = 0.1)
start = c(S = 0.90, E=0.05, I = 0.05, R = 0)

out = ode(y=start, times=times, func=seirmod, parms= parms)
out = as.data.frame(out) 
head(round(out, 3))

R0=(parms["beta"]*parms["sigma"])/((parms["gamma"]+parms["mu"])*(parms["sigma"]+parms["mu"]))
#Adjust margins to accommodate a second right axis
par(mar = c(5,5,2,5))
#Plot state variables
plot(x=out$time, y=out$S, ylab="Probability", xlab="Time",type="l")
lines(x=out$time, y=out$E, col="purple") 
lines(x=out$time, y=out$I, col="red") 
lines(x=out$time, y=out$R, col="green") 
#Add vertical line at turnover point 
xx = out$time[which.max(out$I)] 
lines(c(xx,xx), c(1/R0,max(out$I)), lty=3)
#prepare to superimpose 2nd plot
par(new=TRUE)
#plot effective reproductive ratio (w/o axes) 
plot(x=out$time, y=R0*out$S, type="l", lty=2, lwd=2,
     col="black", axes=FALSE, xlab=NA, ylab=NA,
ylim=c(-.5, 4.5))
lines(c(xx, 26), c(1,1), lty=3)
#Add right-hand axis for RE
axis(side = 4)
mtext(side = 4, line = 4, expression(R[E])) #Add legend
legend("topright", legend=c("S","E" ,"I", "R",expression(R[E])), 
  lty=c(1,1,1,1,2), col=c("black","purple" , "red", "green", "black"))


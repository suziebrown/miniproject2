p <- seq(0,1,0.001)
h <- function(p, a) {(sqrt(p*(1-a))-sqrt(a*(1-p)))^2}

plot(NA, xlim=c(0,1), ylim=c(0,1), ylab="h_a", xlab="p")
a <- seq(0.1,0.5,0.1)
for (i in 1:length(a)) {
  lines(p, h(p, a[i]), type='l', lty=i, lwd=2)
}
legend("topleft", paste("a=",a,sep=""), lty=1:5, lwd=2)

### random-walk model

nreps <- 10000
nsamples <- 2000

drift <- 0.0 # non informative stimulus
sdrw <- 0.3
criterion <- 3

latencies <- rep(0,nreps)
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1)

for (i in c(1:nreps)) {
  evidence [i,] <- cumsum(c(0,rnorm(nsamples, drift, sdrw)))
  p <- which(abs(evidence[i,])>criterion)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i] <- p
}


### plot up to 5 random-walk path

# Generar colores aleatorios
generate_random_color <- function() {
  rgb(runif(1), runif(1), runif(1))
}


tbpn <- min(nreps,5)
plot(1:max(latencies[1:tbpn])+10,type = "n",las=1, ylim=c(-criterion-.5, criterion+.5),
     ylab = "evidence", xlab="decision time")
for (i in c(1:tbpn)) {
  lines(evidence[i,1:(latencies[i]-1)])
}
for (i in 1:tbpn) {
  lines(evidence[i, 1:(latencies[i] - 1)], col = generate_random_color())
}
abline(h=c(criterion,-criterion),lty="dashed")


### plot histograms of latencies
par(mfrow=c(2,1))

toprt <- latencies[responses > 0]
topprop <- length(toprt) / nreps
botrt <- latencies[responses < 0]
botprop <- length(botrt) / nreps

all_latencies <- c(toprt, botrt)
breaks <- hist(all_latencies, plot = FALSE)$breaks

hist(toprt, breaks = breaks, col = "gray", xlab = "Decision time", xlim = c(0, max(latencies)),
     main = paste("Top responses(", as.numeric(topprop), ") m=", as.character(signif(mean(toprt), 4)),
                  sep = ""), las = 1)

hist(botrt, breaks = breaks, col = "gray", xlab = "Decision time", xlim = c(0, max(latencies)),
     main = paste("Bottom responses(", as.numeric(botprop), ") m=", as.character(signif(mean(botrt), 4)),
                  sep = ""), las = 1)
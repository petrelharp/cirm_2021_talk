##########
# sigma effective for big/small

d_varL <- function (p, q) {
    return(p * q * (1 - (p + q)) / (3 * (p + q)))
}

pqvals <- seq(0, 1, length.out=101)[2:99]
pq <- expand.grid(p=pqvals,
                  q=pqvals)
pq$dv <- d_varL(pq$p, pq$q)


png(file="bigsmall_dvar.png", width=64*7, height=64*7, res=64)
par(mar=c(5,4,1,1)+.1)
    cols <- colorspace::diverging_hcl(101,
      h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))
    image(pqvals, pqvals, matrix(pq$dv, nrow=length(pqvals)),
          zlim=c(-1, 1) * max(abs(pq$dv)),
          col=cols, xlab='p', ylab='q')
    contour(pqvals, pqvals, matrix(pq$dv, nrow=length(pqvals)), add=TRUE,
            labcex=2)
dev.off()


##########
# stationary distribution for lineages in the PME

xvals <- seq(-10, 0, length.out=1001)

# wavefront
w <- function (x) {
    y <- pmax(0, 1 - exp(x / 2))
    return(y / sum(y))
}

# stationary distribution for lineages
f <- function (x) {
    y <- exp(x) * (1 - exp(x/2))
    return(y / sum(y))
}

png(file='pme_dists.png', width=64*5, height=64*7, res=64)
par(mar=c(5,3,1,1)+.1)
    plot(xvals, f(xvals), type='l',
         ylab='', xlab='position', lwd=2)
    lines(xvals, w(xvals), col='red', lwd=2)
    legend("topleft", lty=1, lwd=2, col=c("black", "red"),
           legend=c("lineage", "population"))
dev.off()

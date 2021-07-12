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
# diagram of continuous-space model

draw_circle <- function (xy, r, radius_label, ...) {
    xy <- unlist(xy)
    theta <- seq(0, 2*pi, length.out=101)
    polygon(xy[1] + r * cos(theta),
            xy[2] + r * sin(theta), ...)
    if (!missing(radius_label)) {
        segments(x0=xy[1], y0=xy[2], x1=xy[1] + r)
        text(xy[1] + r/2, xy[2], labels=radius_label, pos=3, cex=2)
    }
}

N <- 100
epsilon <- 0.4
sigma <- 0.2
H <- 1
W <- 5/3

set.seed(25)
xy <- data.frame(
            x=W * (runif(N) - 0.5),
            y=H * (runif(N) - 0.5)
        )

xy <- xy[order(xy$x^2 + xy$y^2),]

png(file="scales_diagram.png", width=144*5, height=144*3, res=144)
par(mar=c(1,1,1,1))
k <- 10
    plot(xy$x, xy$y, type='n',
         xlim=c(-1, 1) * W/2, ylim=c(-1, 1) * H/2,
         asp=1, xaxt='n', xlab='', yaxt='n', ylab='')
    draw_circle(xy[k,], epsilon, col=adjustcolor("red", 0.5), lwd=2, radius_label=expression(sqrt(epsilon)))
    points(xy[k,1], xy[k,2], cex=2, pch=20)
    in_circle <- setdiff(which((xy$x - xy[k,1])^2 + (xy$y - xy[k,2])^2 < epsilon^2), k)
    arrows(x0=xy$x[in_circle], y0=xy$y[in_circle],
           x1=0.8 * xy[k,1] + 0.2 * xy$x[in_circle],
           y1=0.8 * xy[k,2] + 0.2 * xy$y[in_circle],
           col='red', lwd=2)
    points(xy$x, xy$y, pch=20)
k <- 15
    n <- 12
    draw_circle(xy[k,], sigma, border="blue", col=adjustcolor("blue", 0.25), lwd=2, radius_label=expression(sigma))
    points(xy[k,1], xy[k,2], cex=2, pch=20)
    arrows(
        x0=xy[k,1], y0=xy[k,2],
        x1=xy[k,1] + rnorm(12, sd=sigma),
        y1=xy[k,2] + rnorm(12, sd=sigma),
        col='blue', lwd=2
    )
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

pdf(file='pme_dists.pdf', width=5, height=7)
par(mar=c(5,3,1,1)+.1)
    plot(xvals, f(xvals), type='l',
         ylab='', xlab='position', lwd=2)
    lines(xvals, w(xvals), col='red', lwd=2)
    legend("topleft", lty=1, lwd=2, col=c("black", "red"),
           legend=c("lineage", "population"))
dev.off()

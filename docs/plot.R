xs <- c(111.2,139.0,158.9,208.6,222.5)

ys <- c(4781.7,4142.6,3686.2,3074.5,2592.3)

ls <- c("a","b","c","d","e")
ls <- rbind(xs, ys)

plot(xs,ys,col=1,title("Energy by makespan"), ylab = "Energy[J]", xlab = "Time[s]", cex=2, xlim=c(100,250))
abline(v = 183.45, untf = FALSE, col=2 )

axis(side = 1, labels = xs, cex.axis=2, at=xs)

text(xs,ys,ys, pos=3)
text(xs,ys,xs, pos=2)




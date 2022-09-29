#File saving and loading directory####


setwd("F:/R Programming/Statistical Test")


##H0 (null hypothesis) and H1 (alternative hypothesis)####
#Type I error
colorie <- function (x, y1, y2, N=1000, ...) {
  for (t in (0:N)/N) {
    lines(x, t*y1+(1-t)*y2, ...)
  }
}
# No, there is already a function to do this
colorie <- function (x, y1, y2, ...) {
  polygon( c(x, x[length(x):1]), c(y1, y2[length(y2):1]), ... )
}
x <- seq(-6,6, length=100)
y <- dnorm(x)
plot(y~x, type='l')
i = x<qnorm(.025)
colorie(x[i],y[i],rep(0,sum(i)) ,col='red')
i = x>qnorm(.975)
colorie(x[i],y[i],rep(0,sum(i)) ,col='red')
lines(y~x)
title(main="Type I error")




#p-value####
#Type II error
x <- seq(-6,6, length=1000)
y <- dnorm(x)
plot(y~x, type='l')
y2 <- dnorm(x-.5)
lines(y2~x)
i <- x>qnorm(.025) & x<qnorm(.975)
colorie(x[i],y2[i],rep(0,sum(i)), col='red')
segments( qnorm(.025),0,qnorm(.025),dnorm(qnorm(.025)), col='red' )
segments( qnorm(.975),0,qnorm(.975),dnorm(qnorm(.975)), col='red' )
lines(y~x)
lines(y2~x)
title(main="High risk of type II error")





x <- seq(-6,6, length=1000)
y <- dnorm(x)
plot(y~x, type='l')
y2 <- dnorm(x-3.5)
lines(y2~x)
i <- x>qnorm(.025) & x<qnorm(.975)
colorie(x[i],y2[i],rep(0,sum(i)), col='red')
segments( qnorm(.025),0,qnorm(.025),dnorm(qnorm(.025)), col='red' )
segments( qnorm(.975),0,qnorm(.975),dnorm(qnorm(.975)), col='red' )
lines(y~x)
lines(y2~x)
title(main="Lower risk of type II error")




delta <- seq(-1.5, 1.5, length=500)
p <- NULL
for (d in delta) {
  p <- append(p, 
              power.t.test(delta=abs(d), sd=1, sig.level=0.05, n=20,
                           type='one.sample')$power)
}
plot(1-p~delta, type='l',
     xlab='mean difference', ylab="propability of a type II error",
     main="type II error in a Student T test")
abline(h=0,lty=3)
abline(h=0.05,lty=3)
abline(v=0,lty=3)




curve(dnorm(x), from=-5, to=5, add=F, col="orange", lwd=3, lty=2)  
curve(dt(x,100), from=-5, to=5, add=T, col=par('fg'))  
curve(dt(x,5),  from=-5, to=5, add=T, col="red")  
curve(dt(x,2),  from=-5, to=5, add=T, col="green")  
curve(dt(x,1),  from=-5, to=5, add=T, col="blue")  
legend(par('usr')[2], par('usr')[4], xjust=1,
       c('gaussian', 'df=100', 'df=5', 'df=2', 'df=1'),
       col=c('orange', par('fg'), 'red', 'green', 'blue'),
       lwd=c(3,1,1,1,1),
       lty=c(2,1,1,1,1))
title(main="Student's T probability distribution function")






















































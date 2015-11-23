set.seed(1)
n = 500

x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 0, 1)
y <- x1 + x2 + rnorm(n, 0, 0.1)
x1mcar <- ifelse(rnorm(n, 1, sd = 1) > 0, x1, NA)
x1marx <- ifelse(rnorm(n, sd=3) < x2, x1, NA)
x1mary <- ifelse (rnorm(n, sd=1.5) > y, x1, NA)
x1mnar <- ifelse (rnorm(n, sd=.8) < x1, x1, NA)

x1 <- c(x1, x1mcar, x1marx, x1mary, x1mnar)
random <- gl(5, n, n*5, labels = c("None missing", "x1 MCAR", "x1 MAR on x2", "x1 MAR on y", "x1 MNAR"))
y <- x1 + x2 + rnorm(n, 0, 0.1)

data <- data.frame(x1, x2 = rep(x2, 5), y, random)

x1plot <- xyplot(y ~ x1 | random, data = data, layout = c(5,1),
                 panel = function(x1, y, ...) {
                   panel.xyplot(x1, y, ...)
                   lm1 <- lm(y ~ x1)
                   lm1sum <- summary(lm1)
                   a <- lm1$coefficients[1]
                   b <- lm1$coefficients[2]
                   panel.abline(a = lm1$coefficients[1], 
                                b = lm1$coefficients[2])
                   panel.text(labels = 
                                bquote(italic(r) == 
                                         .(format(b, 
                                                  digits = 2))),
                              x = 1.8, y = -4)
                 },
                 as.table = TRUE)

x2plot <- xyplot(y ~ x2 | random, data = data, layout = c(5,1),
                 panel = function(x2, y, ...) {
                   panel.xyplot(x2, y, ...)
                   lm1 <- lm(y ~ x2)
                   lm1sum <- summary(lm1)
                   a <- lm1$coefficients[1]
                   b <- lm1$coefficients[2]
                   panel.abline(a = lm1$coefficients[1], 
                                b = lm1$coefficients[2])
                   panel.text(labels = 
                                bquote(italic(r) == 
                                         .(format(b, 
                                                  digits = 2))),
                              x = 1.8, y = -4)
                 },
                 as.table = TRUE)

obj <- c(x1plot, x2plot, layout = c(5,2), x.same = T, y.same = T)
plot <- update(obj, ylab = c("y ~ x1", "y ~ x2"), xlab = "x")

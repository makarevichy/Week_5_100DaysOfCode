library(pryr)
object_size(1:10)
object_size(mean)
object_size(mtcars)
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")
object_size(numeric())
object_size(logical())

plot(0:50, sizes - 40, xlab = "Length", 
     ylab = "Bytes excluding overhead", type = "n")
abline(h = 0, col = "grey80")
abline(h = c(8, 16, 32, 48, 64, 128), col = "grey80")
abline(a = 0, b = 4, col = "grey90", lwd = 4)
lines(sizes - 40, type = "s")

x <- 1:1e6
object_size(x)
y <- list(x, x, x)
object_size(y)
object_size(x, y)
x1 <- 1:1e6
y1 <- list(1:1e6, 1:1e6, 1:1e6)

object_size(x1)
object_size(y1)
object_size(x1) + object_size(y1) == object_size(x1, y1)
object_size("banana")
object_size(rep("banana", 10))

vec <- lapply(0:50, function(i) c("ba", rep("na", i)))
str <- lapply(vec, paste0, collapse = "")
object_size(vec)
object_size(str)
object_size(1:5)
object_size(list(1:5))

library(pryr)
mem_used()
mem_change(x <- 1:1e6)
mem_change(rm(x))
mem_change(NULL)
mem_change(x <- 1:1e6)
mem_change(y <- x)
mem_change(rm(x))
mem_change(rm(y))

f1 <- function() {
  x <- 1:1e6
  10
}
mem_change(x <- f1())
object_size(x)
f2 <- function() {
  x <- 1:1e6
  a ~ b
}
mem_change(y <- f2())
object_size(y)

f3 <- function() {
  x <- 1:1e6
  function() 10
}
mem_change(z <- f3())
object_size(z)


library(pryr)
x <- 1:10
c(address(x), refs(x))

y <- x
c(address(y), refs(y))
x <- 1:10
y <- x
c(address(x), address(y))
x[5] <- 6L
c(address(x), address(y))

x <- 1:10
# Prints the current memory location of the object
tracemem(x)
# [1] "<0x7feeaaa1c6b8>"

x[5] <- 6L

y <- x
# Prints where it has moved from and to
x[5] <- 6L
# tracemem[0x7feeaaa1c6b8 -> 0x7feeaaa1c768]:

f <- function(x) x
{x <- 1:10; f(x); refs(x)}
{x <- 1:10; sum(x); refs(x)}

f <- function(x) 10
g <- function(x) substitute(x)

{x <- 1:10; f(x); refs(x)}
{x <- 1:10; g(x); refs(x)}

x <- data.frame(matrix(runif(100 * 1e4), ncol = 100))
medians <- vapply(x, median, numeric(1))

for(i in seq_along(medians)) {
  x[, i] <- x[, i] - medians[i]
}

for(i in 1:5) {
  x[, i] <- x[, i] - medians[i]
  print(c(address(x), refs(x)))
}

y <- as.list(x)

for(i in 1:5) {
  y[[i]] <- y[[i]] - medians[i]
  print(c(address(y), refs(y)))
}
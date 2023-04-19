set.seed(0)

generate_data_with_slope <- function() {
    f <- function(x) x
    add_noise <- function(x) x + rnorm(1, 0, 1)
    generator <- function(x) f(x) |> add_noise()
    result <- data.frame(
        x = rnorm(30, 0, 100)
    )
    result$y <- generator(result$x)
    return(result)
}

generate_data_without_slope <- function() {
    f <- function(x) 0
    add_noise <- function(x) x + rnorm(1, 0, 1)
    generator <- function(x) f(x) |> add_noise()
    result <- data.frame(
        x = rnorm(30, 0, 100)
    )
    result$y <- generator(result$x)
    return(result)
}

data_with_slope <- generate_data_with_slope()
data_without_slope <- generate_data_without_slope()

lm1 <- lm(y ~ x, data_with_slope)
lm2 <- lm(y ~ x, data_without_slope)

print(summary(lm1))
print(summary(lm2))

View(summary(lm1))

rm(generate_data_with_slope)
rm(generate_data_without_slope)
rm(data_with_slope)
rm(data_without_slope)
rm(lm1)
rm(lm2)
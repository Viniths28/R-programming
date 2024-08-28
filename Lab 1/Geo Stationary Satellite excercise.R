dist1 <- seq(from = 0, to = 35880 * 1000, by = 1000)

r1 <- r_earth + dist1

G <- 6.6728 * 10^-11

m <- 5.9736 * 10^24

a1 <- G * m / r^2

dist_km1 <- dist1 / 1000

plot(x = dist_km, y = a, type = "l", col = "red")

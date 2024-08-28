# Constants


# gravity constant
G <- 6.6728 * 10^-11

# Earth radius from core
r_earth <- 6.371 * 10^6

# mass Constant
m <- 5.9736 * 10^24

#------------------------------------------------------------------------------

# Create distance from 0 to 20350km(203508*1000)
dist <- seq(from = 0, to = 20350 * 1000, by = 10)

r <- r_earth + dist
#------------------------------------------------------------------------------

# accleration formulae
a <- G * m / r^2

#------------------------------------------------------------------------------

# ploting
dist_km <- dist / 1000
plot(x = dist_km, y = a, type = "l", col = "blue")
# points(x=dist, y=a, type="p",col="red")




Calc_gravity <- function(distance) {
  # constants
  G <- 6.6728 * 10^-11
  r_earth <- 6.371 * 10^6
  m <- 5.9736 * 10^24
  # formulae
  r <- r_earth + distance
  a <- G * m / r^2

  return(a)
}

Calc_gravity(distance = 0)
Calc_gravity(distance = 25000)
Calc_gravity(distance = 25000 * 1000)
Calc_gravity(distance = 1000) 
                                                                               


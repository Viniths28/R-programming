# calculation of gravity on surface of each planet

calc_gravity_each_planet <- function(mass, radius) {
  # distance=0,since the we calculate gravity only on the surface

  distance <- 0
  r <- radius + distance
  Gravity <- 6.674 * 10^-11

  # calcultions
  a2 <- Gravity * mass / r^2

  return(a2)
}

# gravity on mars
calc_gravity_each_planet(mass = 6.42 * 10^23, radius = 3.390 * 10^6)
# gravity on saturn
calc_gravity_each_planet(mass = 5.68 * 10^26, radius = 5.823 * 10^7)
# gravity on Jupiter
calc_gravity_each_planet(mass = 1.90 * 10^27, radius = 6.991 * 10^7)





# phobos excercise
dist2 <- seq(from = 0, to = 5989 * 1000, by = 1000)

radius_mars <- 3.90 * 10^6
G <- 6.6728 * 10^-11

r_mars <- radius_mars + dist2

dist2km <- dist2 / 1000

a_mars <- G * m / r_mars^2

plot(x = dist2km, y = a_mars, type = "p", col = "yellow")







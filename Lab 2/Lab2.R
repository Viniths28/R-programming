# 2.1----
library(dplyr)
planet <- data.frame(
  name = c(
    "Mercury", "Venus", "Earth", "Mars",
    "Jupiter", "saturn", "Uranus", "Neptune"
  ),
  mass = c(
    3.30 * 10^23, 4.87 * 10^24, 5.97 * 10^24,
    6.42 * 10^23, 1.90 * 10^27, 5.68 * 10^26,
    8.68 * 10^25, 1.02 * 10^26
  ),
  radius = c(
    2.440 * 10^6, 6.052 * 10^6, 6.371 * 10^6,
    3.390 * 10^6, 6.991 * 10^7, 5.823 * 10^7,
    2.536 * 10^7, 2.462 * 10^7
  )
)

class(planet)

View(planet)

planet$name

planet$mass

planet$radius

planet$volume <- 4 / 3 * pi * planet$radius^3

View(planet)

names(planet)
# Method1

planet <- mutate(planet,
  gravity = calc_gravity_each_planet(planet$mass, planet$radius)
)
# eitherways possible- method2
planet$gravity2 <- calc_gravity_each_planet(planet$mass, planet$radius)




planet <- mutate(planet, mass_earths = planet$mass / planet$mass[3])
planet <- mutate(planet, volume_earths = planet$volume / 1.083207 * 10^21)
# planet <- mutate(planet,volume_earths2=planet$volume/planet$volume[3])
planet$volume[3]
planet <- mutate(planet, gravity_earths = planet$gravity / planet$gravity[3])


planet_earths <- data.frame(
  names = planet$name, mass_earths = planet$mass_earths,
  gravity_earths = planet$gravity_earths,
  volume_earths = planet$volume_earths
)


View(planet_earths)
ascending <- arrange(planet, gravity2)


-------------------------------------------------------------------------------

  mat <- matrix(1:15, ncol = 5, nrow = 4, byrow = F)
mat








# Temperature calculations

# deg to farenhite

degF_to_cel <- function(degreeF) {
  degreeC <- (degreeF - 32) * 5 / 9

  return(degreeC)
}

degF_to_cel(degreeF = 270)


# Farenhite to Clesius

degC_to_far <- function(degreeC) {
  degreeF <- (degreeC * 9 / 5) + 32
  return(degreeF)
}


degC_to_far(degreeC = 0)


# clesius to gasmark

degC_to_gas <- function(celsius) {
  gasmark <- (celsius - 121) / 14
  return(gasmark)
}


degC_to_gas(177)

# test Cel To  farenhite
temp <- c(140, 150, 160, 180, 190, 200, 220, 230, 240)

test_c_to_f <- degC_to_far(temp)
test_c_to_f
# test farenhite to celsius
test_f_to_c <- degF_to_cel(test_c_to_f)
test_f_to_c

# cels to gasmark
test_c_to_gasmark <- degC_to_gas(temp)
test_c_to_gasmark

ceiling(test_c_to_gasmark)

floor(test_c_to_gasmark)

round(test_c_to_gasmark)

round(test_c_to_gasmark, 3)




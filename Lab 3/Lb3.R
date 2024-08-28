# fibonaci----------------------------------------------------------------------
n <- 100
r <- c(1, 1)
for (i in 3:n)
{
  r[i] <- r[i - 1] + r[i - 2]
}
r
# Owl Ex------------------------------------------------------------------------
p0 <- 0.11
p1 <- 0.71
p2 <- 0.94
f <- 0.24
years <- 20

J <- 1200
S <- 800
A <- 2000

for (t in 1:(years - 1))
{
  # subadultequation
  J[t + 1] <- f * A[t]
  S[t + 1] <- p0 * J[t]
  A[t + 1] <- p1 * S[t] + p2 * A[t]
}
S
J
A
group1 <- c("J", "S", "A")

owl_pop <- data.frame(
  time = rep(1:20, 3),
  group = rep(group1, each = 20), size = c(J, S, A)
)

groupfactor <- levels(owl_pop$group)
groupfactor
owl_pop$group <- factor(owl_pop$group,
  levels = groupfactor,
  labels = c("Adult", "Juvi", "SubAdult")
)

library(ggplot2)
ggplot(owl_pop, aes(x = time, y = size, col = group)) +
  geom_line(aes(linetype = group)) +
  geom_point() + geom_text(aes(
    label = round(size),
    vjust = -.1, hjust = -.1, angle = 60
  )) +
  theme(legend.position = "top")

ggplot(owl_pop) + geom_line(aes(x = time, y = size, group = group, col = group))









# squareloop--------------------------------------------------------------------



square_x_loop <- function() {
  x <- 1:10^7

  for (i in x)
  {
    x[i] <- x[i]^2
  }
  return(x)
}

square_x_loop()

square_x_vector <- function() {
  x <- 1:10^7
  x <- x^2
  return(x)
}


square_x_vector()
system.time(square_x_loop())
system.time(square_x_vector())



for (animal in c("human", "cat", "turtle", "zebra"))
{
  if (animal == "cat") {
    food <- "fish"
  }
  else {
    food <- "Pizza"
  }


  # compose msg
  msg <- paste("Give a", animal, "some", food, "for lunch")
  print(msg)
}


for (animal in c("human", "cat", "turtle", "zebra", "baby"))
{
  if (animal == "cat" | animal == "turtle") {
    food <- "fish"
  }
  else if (animal == "human") {
    food <- "pizza"
  }
  else if (animal == "zebra") {
    food <- "Grass"
  }
  else {
    food <- "soup"
  }

  # compose msg
  msg <- paste("Give a", animal, "some", food, "for lunch")
  print(msg)
}

# roll_dice---------------------------------------------------------------------

roll_dce <- function() {
  p <- runif(1)
  if (p < 1 / 6) {
    face <- 1
  }
  else if (p < 2 / 6) {
    face <- 2
  }
  else if (p < 3 / 6) {
    face <- 3
  }
  else if (p < 4 / 6) {
    face <- 4
  }
  else if (p < 5 / 6) {
    face <- 5
  }
  else {
    face <- 6
  }
  return(face)
}
roll_dce()
roll_dce()

# roll_dice function------------------------------------------------------------

roll_dce2 <- function(n) {
  results <- c()
  for (i in 1:n)
  {
    p <- runif(1)
    if (p < 1 / 6) {
      face <- 1
    }
    else if (p < 2 / 6) {
      face <- 2
    }
    else if (p < 3 / 6) {
      face <- 3
    }
    else if (p < 4 / 6) {
      face <- 4
    }
    else if (p < 5 / 6) {
      face <- 5
    }
    else {
      face <- 6
    }

    results[i] <- face
  }

  return(results)
}

rolls <- roll_dce2(200)

library(ggplot2)
ggplot() +
  geom_bar(aes(x = factor(rolls)))


# Probabality Sampling-----------------------------------------------------------

sample(7)
sample(2, 9, replace = TRUE)
sample(7, replace = TRUE)
sample(5, 9, replace = TRUE, prob = NULL)
sample(c(0, 1), 100, replace = TRUE)

x <- 1:10
x
sample(x[x > 8])
sample(x[x > 9])
sample(x[1])
x[x > 9]
sample(10, 9)
###################

rps <- function(x = c("Rock", "Paper", "Scissors"), p1, p2) {
  p1 <- sample(x, 1)
  p2 <- sample(x, 1)
  msg <- paste("player1 selected", p1, "player2 Slected", p2)
  return(msg)
}




result <- function() {
  a <- {
    if (rps(p1) == rps(p2)) {
      print("Draw")
    }
    else if (rps(p1) == "Rock" & rps(p2) == "Scissors" | rps(p1) == "Paper" &
      rps(p2) == "Rock" | rps(p1) == "Scissors" & rps(p2) == "Paper") {
      print("p1 won")
    }
    else {
      print("p2 Won")
    }
  }
}

rps()
result()
############### 3

result <- function() {
  x <- c("Rock", "Paper", "Scissors")

  p1 <- sample(x, 1)
  p2 <- sample(x, 1)
  msg <- paste("player1 selected", p1, "player2 Slected", p2)
  print(msg)

  if (p1 == p2) {
    print("Draw")
  }
  else if (p1 == "Rock" & p2 == "Scissors" | p1 == "Paper" &
    p2 == "Rock" | p1 == "Scissors" & p2 == "Paper") {
    print("p1 won")
  }
  else {
    print("p2 Won")
  }
}



result()



game <- function(p1) {
  x <- c("Rock", "Paper", "Scissors")
  p2 <- sample(x, 1)
  msg <- paste("Vinith selected", p1, "Computer Slected", p2)
  print(msg)

  if (p1 == p2) {
    print("Draw")
  }
  else if (p1 == "Rock" & p2 == "Scissors" | p1 == "Paper" &
    p2 == "Rock" | p1 == "Scissors" & p2 == "Paper") {
    print("vinith won")
  }
  else {
    print("Computer Won")
  }
}
game("Paper")
#################################



#_____________________________________________________________________________
rbinom(100,5,0.50)
table(rbinom(100,5,0.50))
dbinom(4,5,0.50)

size <- 30
n_obs <- 100
prob <- 0.59

tiny_schools <- data.frame("sci-prop"=rbinom(n_obs,size,prob)/size,
                           "size"=size)
size <- 100
small_schools <- data.frame("sci-prop"=rbinom(n_obs,size,prob)/size,
                           "size"=size)
size <- 200
medium_schools <- data.frame("sci-prop"=rbinom(n_obs,size,prob)/size,
                            "size"=size)
size <- 500
large_schools <- data.frame("sci-prop"=rbinom(n_obs,size,prob)/size,
                             "size"=size)
size <- 1000
mega_schools <- data.frame("sci-prop"=rbinom(n_obs,size,prob)/size,
                            "size"=size)


schools <- rbind(tiny_schools,small_schools,medium_schools,large_schools,
                 mega_schools)
library(ggplot2)
ggplot(schools)+geom_jitter(aes(x=size,y=sci.prop))



table(rbinom(100,30,0.59))

dbinom(19,30,0.59)

rbinom(100,30,0.59)

dbinom(23,30,0.59)

table(rbinom(100,5,0.50))
dbinom(3,5,0.50)

library(dplyr)
schools_desc <- arrange(schools,desc(sci.prop))
schools_desc[1:25,]


runif(5, 5, 15)
round(rnorm(5, 3.5, 2.56))

.Random.seed[1:5]
runif(5,5,15)


set.seed(59810)
x <- rnorm(100,25,8)
quantile(x,c(0.25,0.75))
y <- c(0.25,0.75)
qnorm(y,25,8)
qnorm(0.75,25,8)
pnorm(19.60408,25,8)

round(rnorm(10,25,8))





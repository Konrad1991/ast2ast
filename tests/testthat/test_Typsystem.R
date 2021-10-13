x <- 1
y <- x + 3
y

x <- c(1, 1, 2)
y <- x + 3
y

x <- c(1, 1, 2, 2)
y <- c(2, 3)
z <- x + 3*y
z

x <- matrix(1.3, ncol = 3, nrow = 2)
y <- x  + 3
y


x <- matrix(1.3, ncol = 3, nrow = 2)
y <- c(2, 3)
z <- x + y
z


x <- matrix(1.3, ncol = 3, nrow = 2)
y <- rep(4, 6)
z <- x + y
z

x <- matrix(1.3, ncol = 3, nrow = 2)
y <- matrix(5, ncol = 3, nrow = 2)
z <- x + y
z


x <- matrix(1:6, ncol = 3, nrow = 2)
y <- matrix(5, ncol = 3, nrow = 2)
z <- c(1,2)
a <- x + y*z + 5
a




# Vector initialisierung 

## Nur Speicher belegen
v <- vector(mode = "numeric", length = 3) # vec: nur längen argument 

## Speicher und Daten initialisierung
v <- 1:3
v <- c(1,2,3)

# initialisierter vector <- vector_new
v <- 1:3
v <- 2 # überschreibt alten vektor
# in C++ wird/muss in diesem Fall der copy constructor gerufen

v <- 1:3
v <- v + v 
# in C++ wird in diesem Fall operator= aufgerufen

v <- 1:3
v[1] <- 2 # überschreibt alten vektor nicht
# in C++ muss in diesem Fall der operator= aufgerufen werden


# Kombinationen nicht passender elemente
v1 <- 1:4
v2 <- 1:5
v3 <- v1 + v2 # only warning
v3 <- v2 + v1 # only warning
v3 <- v1 + 3 # no warning
v4 <- 1:2
v3 <- v1 + v2 # only warning
v3 <- v2 + v1 # only warning


# Außerhalb vector bereich
v <- 1:3
print(v[-1]) # -1 verkürzt vektor

v <- 1:3
v[-1] <- 2 # dann fängt er von hinten an zu zählen?

v <- 1:3 
v[-3] <- 4 # ab -3 sind alle elemente 4 --> sehr komisches verhalten

v <- 1:3
v[1] <- v[5] + v[6] # das geht da v[5] und v[6] NA sind

v <- 1:3
v[1] <- v[2] + v[5] # das geht wohl auch --> resultiert in NA

# verhalten außerhalb vektor bereich nicht unterstüzten!!!
# Stattdessen: Stop wenn außerhalb von vektor. 
# Für das entfernen von Elementen eigene funktion erstellen --> remove




# Matrix initialisierung

m1 <- matrix()
m2 <- matrix(2,2,2) 
m1 <- m2


m1 <- matrix()
m2 <- matrix(2,2,2)
m3 <- matrix(4,4, 2)
m1 <- m2 + m3 #error



m1 <- matrix()
m2 <- matrix(2,2,2)
m3 <- matrix(1, 4, 2)
m1 <- m2 + m3 # error


m1 <- matrix()
m2 <- matrix(2,2,2)
m3 <- matrix(2,2, 2)
m1 <- m2 + m3 # works


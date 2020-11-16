# code for lab 2

# verifying values of zbar and s_z^2
mpg <- epa$MPG
s <- sd(mpg)
m <- mean(mpg)
z = (mpg - m) / s
zbar <- mean(z)
sz2 = sum(((z-zbar)^2)/99)
zbar
sz2

# possible outliers using z score
po <- mpg[abs(z) >= 2 & abs(z) <= 3]
po

# outliers using z score
o <- mpg[abs(z) >= 3]
o

# dotplot
library(lattice)
ocol = with(epa, ifelse(mpg == po, "Blue",
                        ifelse(mpg == o, "Red", "Black")))
d <- data.frame(x = epa$MPG, y = c(1:100))
dotplot(y~x, data = d, col = ocol, xlab = "Miles per Gallon")


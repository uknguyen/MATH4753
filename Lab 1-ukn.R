# code used in lab 1

# create barplot from table
sp <- ddt$SPECIES
tab <- table(sp)
barplot(tab, col = 1:3)

# subsetting data
with(ddt, ddt[SPECIES == "LMBASS" & WEIGHT > 800,])
with(ddt, ddt[RIVER == "SCM" & DDT > 4.0,])

# plot length x weight using ddt data
plot(WEIGHT~LENGTH, data = ddt)

# coplot with custom colors
rivcol=with(ddt, ifelse(RIVER == "FCM", "Red",
ifelse(RIVER == "LCM", "Orange",
       ifelse(RIVER == "SCM", "Green", "Blue"))))
coplot(LENGTH~WEIGHT|RIVER, data = ddt, col = rivcol, ylim = c(0, 60))

# ggplot
library(ggplot2)
b = ggplot(ddt,aes(x = SPECIES, y = WEIGHT))
b = b + geom_boxplot(aes(fill = RIVER)) + labs(title = "U. Kristine Nguyen")
b

v = ggplot(ddt, aes(x = RIVER, y = LENGTH))
v = v + geom_violin(aes(fill = SPECIES)) + labs(title = "U. Kristine Nguyen")
v

s = ggplot(ddt, aes(x = WEIGHT, y = LENGTH, fill = SPECIES))
s = s + geom_point(aes(colour = SPECIES)) + labs(title = "U. Kristine Nguyen")
s




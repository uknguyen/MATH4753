# code for lab 11

# 95% confidence - using r as calculator (t-test)
std <- sd(d) # sample standard deviation
ta <-qt(1-0.05/2, 25-1) # t multiplier
mp <- c(-1,1) # -/+
mean(d)+ mp*ta*std/sqrt(25) # formula

# using r as calculator for chi statistic
chi1a = qchisq(0.05/2, 25-1)
chi1b = qchisq(1-0.05/2,25-1)
l1 = ((25-1)*(std^2))/chi1a
u1 = ((25-1)*(std^2))/chi1b

# using r as calculator for diff of means
sps = (19*(sd(blue)^2)+14*(sd(snap)^2))/33
tss = qt(1-0.05/2, 33)
mean(blue) - mean(snap) + mp*tss*sqrt(sps*(1/20 + 1/15))


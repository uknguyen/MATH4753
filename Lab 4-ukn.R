# code for lab 4

# getting the last 6 rows of data
spr.df <- read.csv("SPRUCE.csv")
tail(spr.df)

# trendscatter plot
library(s20x)
trendscatter(Height~BHDiameter, data = spr.df, f = 0.5)

# linear model
spruce.lm = with(spr.df, lm(Height~BHDiameter))

# residuals
hght.res = residuals(spruce.lm)

# fitted values
hght.fit = fitted(spruce.lm)

#Res vs Fit
plot(y = hght.res, x = hght.fit)

#Res vs Fit w/ Trendscatter
trendscatter(y = hght.res, x = hght.fit)

# checking normality
normcheck(spruce.lm, shapiro.wilk = TRUE)

# quadratic
quad.lm = lm(Height~BHDiameter + I(BHDiameter ^2), data = spr.df)
summary(quad.lm)
coef(quad.lm)
plot(spr.df)
np = function(x) {
  quad.lm$coef[1] + quad.lm$coef[2] * x + quad.lm$coef[3] * x^2
}
curve(np, lwd = 2, add = TRUE, col = "Purple")
quad.fit = fitted(quad.lm)

# interval estimates
ciReg(quad.lm)

# predictions
predict(quad.lm, data.frame(BHDiameter = c(15, 18, 20)))

# anova
anova(spruce.lm)
anova(quad.lm)
anova(spruce.lm, quad.lm)

# cook's distance
cooks20x(quad.lm, main = "Cook's Plot (quad.lm)")
quad2.lm = lm(Height~BHDiameter + I(BHDiameter ^ 2), data = spr.df[-24,])

# reproducing plot
library(ggplot2)
xk = 18
dff <- spr.df
df <- within(dff, X <- (BHDiameter - xk)*(BHDiameter>xk))
head(df)

ylm <- lm(Height ~ BHDiameter + X, data = df)
summary(ylm)
gg <- ggplot(df, aes(x = BHDiameter, y = Height), xlim = c(4, 31), ylim = c(7, 23)) + geom_point() + geom_smooth(method = "lm", formula = 'y ~ x + I((x-xk)*(x>xk))', se = F, data = df)
gg


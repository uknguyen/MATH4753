# code for lab 3

## Task 2

spr <- read.table(SPRUCE.csv)
head(spr)

### All the code I needed ever

## Task 3
#Download and install ggplot2
library(ggplot2)
g=ggplot(spr, aes(x=BHDiameter,y=HEIGHT,colour=BHDiameter))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("Tree Height Vs BHDiameter")


#new plotting window
windows()
plot(HEIGHT~BHDiameter,bg="Blue",pch=21,cex=1.2,ylim=c(0,1.1*max(HEIGHT)),xlim=c(0,1.1*max(BHDiameter)), data=spr)

#Load library
#make a new plot
library(s20x)
tsone = trendscatter(HEIGHT~BHDiameter,f=0.5, data=spr)
tstwo = trendscatter(HEIGHT~BHDiameter,f=0.6, data=spr)
tsthree = trendscatter(HEIGHT~BHDiameter,f=0.7, data=spr)

#Create linear model for spruce.df
spruce.lm = with(spruce.df, lm(HEIGHT~BHDiameter))

#Regression line
abline(spruce.lm)

#lm stands for linear model

#layout
lo = layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))

## Task 4

#Lets look at where the plots will go
layout.show(4)

#Total Sum of Squares; Model Sum of Squares R Sum of Squares
TSS = MSS + RSS

#![](name of file){with}

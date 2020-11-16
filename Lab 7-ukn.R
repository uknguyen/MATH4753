#Task 2
  mychisim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){
    
    #generates vector with normally distributed random variables that fit those parameters
    y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1
    
    #creates data matrix using vector created above
    data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
    
    #applies function to margins of matrix
    ssq1=apply(data1.mat,2,var)
    
    #chi-square statistic
    w=(n1-1)*ssq1/sigma1^2
    
    hist(w,freq=FALSE, ylim=c(0,ymax), main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)), xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
    
    #line of best fit for the histogram
    lines(density(w),col="Blue",lwd=3)
    
    #theoretical normal curve
    curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3)
    title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2))
    
    #places legend on click
    legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) 
    
    return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq"))
  }

quartz()

# (a)
mychisim(n1 = 10, iter = 1000, mean1 = 10, sigma1 = 4, ymax = 0.1)

# (b)
mychisim(n1 = 20, iter = 1000, mean1 = 10, sigma1 = 4)

# (c)
mychisim(n1 = 100, iter = 1000, mean1 = 10, sigma1 = 4)

# (d)
mychisim(n1 = 200, iter = 1000, mean1 = 10, sigma1 = 4)

# (e) the histogram included is of chisq$w
chisq1 = mychisim(n1 = 10, iter = 1500, mean1 = 20, sigma1 = 10, ymax = 0.11)

# Task 3
#function
myTsim <- function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){
  
  #y vector
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  
  #create matrix
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  
  #apply function w/ matrix
  sd1=apply(data1.mat,2,sd)
  ybar=apply(data1.mat,2,mean)
  
  w=(ybar-mean1)/(sd1/sqrt(n1))
  
  #histogram
  hist(w,freq=FALSE, ylim=c(0,ymax), main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",T," iterations= ",iter)), xlab=expression(paste(T, "Statistic",sep=" ")), las=1)
  
  #density line
  lines(density(w),col="Blue",lwd=3)
  
  #theoretical distribution curve
  curve(dt(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3)
  
  title=expression(T==frac((bar(y)-mu),s/sqrt(n1)))
  
  #legend
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)
  
  #big table of data
  return(list(w=w,summary=summary(w),sd=sd(w),fun="T"))
}

quartz()

## (a) $n_1 = 10, iter = 1000, \mu_1 = 10, \sigma_1 = 4$
myTsim(n1 = 10, sigma1 = 4, mean1 = 10, iter = 1000, ymax = 0.45)

## (b) $n_1 = 20, iter = 1000, \mu_1 = 10, \sigma_1 = 4$
myTsim(n1 = 20, sigma1 = 4, mean1 = 10, iter = 1000, ymax = 0.45)

## (c) $n_1 = 100, iter = 1000, \mu_1 = 10, \sigma_1 = 4$
myTsim(n1 = 100, sigma1 = 4, mean1 = 10, iter = 1000, ymax = 0.45)

## (d) $n_1 = 200, iter = 1000, \mu_1 = 10, \sigma_1 = 4$
myTsim(n1 = 200, sigma1 = 4, mean1 = 10, iter = 1000, ymax = 0.45)

## (e) $n_1 = 10, iter = 1500, \mu_1 = 20, sigma_1 = 10)$
myTsim(n1 = 10, iter = 1500, mean1 = 20, sigma1 = 10, ymax = 0.45)

# Task 4
mychisim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.07,...){ 
  
  #two different vectors using the data
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  
  #making two matrices using the vectors
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  
  #applies function to margin of matrices
  ssq1=apply(data1.mat,2,var)
  ssq2=apply(data2.mat,2,var)
  spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2)
  w=(n1+n2-2)*spsq/(sigma1^2)
  
  #histogram
  hist(w,freq=FALSE, ylim=c(0,ymax), main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",chi^2)), xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  
  #line following rough shape of histogram
  lines(density(w),col="Blue",lwd=3)
  
  #line showing how data so theoretically look
  curve(dchisq(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3)
  title=expression(chi^2==frac((n[1]+n[2]-2)*S[p]^2,sigma^2))
  
  #places legend on click
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) 
  
  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}

quartz()

## (a)
mychisim2(n1 = 10, n2 = 10, mean1 = 5, mean2 = 10, sigma1 = 4, sigma2 = 4, iter = 1000)

## (b)
mychisim2(n1 = 20, n2 = 10, mean1 = 3, mean2 = 5, sigma1 = 10, sigma2 = 10, iter = 1000)

## (c)
mychisim2(n1 = 50, n2 = 50, mean1 = 5, mean2 = 10, sigma1 = 4, sigma2 = 4, iter = 10000)

## (d)
mychisim2(n1 = 80, n2 = 50, mean1 = 3, mean2 = 5, sigma1 = 10, sigma2 = 10, iter = 10000)

## (e)
mychisim2(iter = 10000)

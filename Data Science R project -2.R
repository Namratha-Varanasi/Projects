require(data.table)

#Q1a
# return: string(“cylinders”, “displacement”, “horsepower”, “weight”, “acceleration”, “year”, “origin”), 
# represents the covariate providing the best prediction
SLR <- function(path='../data/hw23R-linear.txt'){
  dat=read.csv(path)
  #print(dat)
  
  
  x1=dat$cylinders
  y=dat$mpg
  
  #plot(x1,y,main="cylinders Vs mpg",xlab="cylinder",ylab="mpg",col="blue")
  #abline(lm(y~x1),col="red")
  #lm(y~x1)

  sse1=sum((y-fitted(lm(y~x1)))^2)
  #print(sse1)
  
  
  x2=dat$displacement
  y=dat$mpg
  #plot(x2,y,main="displacement Vs mpg",xlab="displacement",ylab="mpg",col="blue")
  #abline(lm(y~x2),col="red")
  #print(lm(y~x2))
  #rsq2=summary(lm(y~x2))$r.squared
  sse2=sum((y-fitted(lm(y~x2)))^2)
  #print(sse2)
  
  
  
  x3=dat$horsepower
  y=dat$mpg
  #plot(x3,y,main="horsepower Vs mpg",xlab="horsepower",ylab="mpg",col="blue")
  #abline(lm(y~x3),col="red")
  #r3=lm(y~x3)
  #rsq3=summary(lm(y~x3))$r.squared
  sse3=sum((y-fitted(lm(y~x3)))^2)
  #print(sse3)
  
  x4=dat$weight
  y=dat$mpg
  #plot(x4,y,main="weight Vs mpg",xlab="weight",ylab="mpg",col="blue")
  #abline(lm(y~x4),col="red")
  #r4=lm(y~x4)
  #rsq4=summary(lm(y~x4))$r.squared
  sse4=sum((y-fitted(lm(y~x4)))^2)
  #print(sse4)
  
  
  
  x5=dat$acceleration
  y=dat$mpg
  #plot(x5,y,main="acceleration Vs mpg",xlab="acceleration",ylab="mpg",col="blue")
  #abline(lm(y~x5),col="red")
  #r5=lm(y~x5)
  #rsq5=summary(lm(y~x5))$r.squared
  sse5=sum((y-fitted(lm(y~x5)))^2)
  #print(sse5)
  
  
  
  x6=dat$year
  y=dat$mpg
  #plot(x6,y,main="year Vs mpg",xlab="year",ylab="mpg",col="blue")
  #abline(lm(y~x6),col="red")
  #lm(y~x6)
  #r6=lm(y~x6)
  #rsq6=summary(lm(y~x6))$r.squared
  sse6=sum((y-fitted(lm(y~x6)))^2)
  #print(sse6)
  
  
  x7=dat$origin
  y=dat$mpg
  #plot(x7,y,main="origin Vs mpg",xlab="origin",ylab="mpg",col="blue")
  #abline(lm(y~x7),col="red")
  #r7=lm(y~x7)
  #rsq7=summary(lm(y~x7))$r.squared
  sse7=sum((y-fitted(lm(y~x7)))^2)
  #print(sse7)
  
  #To find the best covariate we take the SSE (sum of squared errors) for each covariate and output the one with minimum SSE value as the best covariate
  sseset=c(sse1,sse2,sse3,sse4,sse5,sse6,sse7)
  ssemin=min(sseset)
  #print(rsqmax)
  if(ssemin==sse1)
  {
    print("cylinder")
    
  }else if (ssemin==sse2)
  {
    print("displacement")
    
  }else if(ssemin==sse3)
  {
    print("horsepower")
    
  }else if(ssemin==sse4)
  {
    print("weight")
    
  }else if(ssemin==sse5)
  {
    print("acceleration")
    
  }else if(ssemin==sse6)
  {
    print("year")
    
  }else 
  {
    print("origin")
    
  }
}




#Q1b 
# return: list of following variables, Intercept, CylindersCoeff, DispCoeff, HPCoeff, WeightCoeff, AccCoeff, YearCoeff, OriginCoeff
MLR <- function(path='../data/hw23R-linear.csv'){
  datt=read.csv(path)
  mx1=datt$cylinders
  mx2=datt$displacement
  mx3=datt$horsepower
  mx4=datt$weight
  mx5=datt$acceleration
  mx6=datt$year
  mx7=datt$origin
  my=datt$mpg
  mr=lm(my~mx1+mx2+mx3+mx4+mx5+mx6+mx7)
  
  # fill in the list with the coeffes you compute
  result <- list("Intercept"=coef(mr)[1], "CylindersCoeff"=coef(mr)[2], "DispCoeff"=coef(mr)[3], "HPCoeff"=coef(mr)[4], "WeightCoeff"=coef(mr)[5], "AccCoeff"=coef(mr)[6], "YearCoeff"=coef(mr)[7], "OriginCoeff"=coef(mr)[8])
  return(result)
}
  
#Q2
# return: list of following variables, Intercept， Lag1oeff，Lag2Coeff，Lag3Coeff, Lag4Coeff, Lag5Coeff, VolumeCoeff
LogisticRegression <- function(path='../data/hw23R-logistic.txt'){

  lrdata=read.csv(path)
  #finging the logistic regression not taking Today as it algorithm converges and the Direction becomes completely dependent on Today and the probabilities will be very high >1 (which should not be possible)
  logmodel=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=lrdata,family=binomial)
  #coefficients(logmodel)
  
  #making the predictions on the regression model taking the entire data and for all values of probability greater than or equal to 0.5 direction is up and remaining down
  Direction_probabilty=predict(logmodel, type = "response")
  prediction = rep("Down", 1250)
  prediction[Direction_probabilty>=0.5] = "Up"
  
  #comparing the predicted value to the actual value and producing the accuracy 
  
  table(prediction,lrdata$Direction)
  #k=mean(prediction == lrdata$Direction)
  #print(k)
  
  
  
  # fill in the list with the coeffes you compute
  result <- list("Intercept" =coef(logmodel)[1] ,"Lag1Coeff" =coef(logmodel)[2], "Lag2Coeff" =coef(logmodel)[3], "Lag3Coeff" =coef(logmodel)[4],"Lag4Coeff" =coef(logmodel)[5], "Lag5Coeff" =coef(logmodel)[6],"VolumeCoeff"=coef(logmodel)[7]  )
  return(result)
  
}

#Q3
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/hw23R-logistic.txt'){
  
  
    
    ilrdata=read.csv(path)
    set.seed(123)
    
    # partitioning the ilrdata into two sets ilrdata_trainset and ilrdata_testset by the condition of year<2005 and year>=2005
    ilrdata_train=(ilrdata$Year<2005)
    ilrdata_test=(ilrdata$Year>=2005)
    ilrdata_trainset=data_logistic[ilrdata_train,]
    ilrdata_testset=data_logistic[ilrdata_test,]
    
    #obtaining a model using ilrdata_trainset (used only Lag2 ,Lag3 and Lag4 to improve the accuracy since Lag1 and Volume have the highest Standard error value so eliminate them for improved accuracy)
    ilogmodel=glm(Direction~Lag2+Lag3+Lag4,data=ilrdata_trainset,family=binomial)
    summary(ilogmodel)
    
    
    #making the predictions on the regression model taking the ilrdata_testset and for all values of probability greater than or equal to 0.5 direction is up and remaining down
    Direction_probabilty=predict(ilogmodel,ilrdata_testset,type="response")
    prediction=rep("Down",nrow(ilrdata_testset))
    prediction[Direction_probabilty>=0.5]="Up"
    
    #comparing the predicted value to the actual value and producing the accuracy
    table(prediction,ilrdata_testset$Direction)
    result=mean( prediction==ilrdata_testset$Direction)
    return(result)
    #improved accuracy is 0.5952381
  }
  



#Q4
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){
 
    
    library(data.table)
    library(biglm)
    library(ff)
    data_90m=fread(path)
    
    n=nrow(data_90m)
    model=biglm(formula=y~x,data=data_90m)
    #summary(model)
    
    set.seed(123)
    s1=sample(n,size=n/100,replace=TRUE)
    one_percent_sample=data_90m[s1,]
    one_percent_fit=lm(y~x,data=one_percent_sample)
    #coefficients(one_percent_fit)
    #summary(one_percent_fit)$adj.r.squared
    plot(one_percent_sample,xlab='X',ylab='Y',main="BIG-DATA PLOT")
    abline(one_percent_fit,col="red",lwd=3,lty=1 )
    
    
    
    
    set.seed(123)
    s2=sample(n,size=n/50,replace=TRUE)
    two_percent_sample=data_90m[s2,]
    two_percent_fit=lm(y~x,data=two_percent_sample)
    #coefficients(two_percent_fit)
    #summary(two_percent_fit)$adj.r.squared
    #plot(two_percent_sample,xlab='X',ylab='Y',main="BIG-DATA PLOT")
    abline(two_percent_fit,col="green",lwd=2,lty=2)
    
    set.seed(123)
    s3=sample(n,size=3*n/100,replace=TRUE)
    three_percent_sample=data_90m[s3,]
    three_percent_fit=lm(y~x,data=three_percent_sample)
    # coefficients(three_percent_fit)
    # summary(three_percent_fit)$adj.r.squared
    # plot(three_percent_sample,xlab='X',ylab='Y',main="BIG-DATA PLOT")
    abline(three_percent_fit,col="blue",lwd=2.5,lty=3)
    
    set.seed(123)
    s4=sample(n,size=n/25,replace=TRUE)
    four_percent_sample=data_90m[s4,]
    four_percent_fit=lm(y~x,data=four_percent_sample)
    # coefficients(four_percent_fit)
    # summary(four_percent_fit)$adj.r.squared
    # plot(four_percent_sample,xlab='X',ylab='Y',main="BIG-DATA PLOT")
    abline(four_percent_fit,col="orange",lwd=2,lty=4)
    
    
    set.seed(123)
    s5=sample(n,size=n/20,replace=TRUE)
    five_percent_sample=data_90m[s5,]
    five_percent_fit=lm(y~x,data=five_percent_sample)
    # coefficients(five_percent_fit)
    # summary(five_percent_fit)$adj.r.squared
    # plot(five_percent_sample,xlab='X',ylab='Y',main="BIG-DATA PLOT")
    abline(five_percent_fit,col="cyan",lwd=2,lty=5)
    
    legend("bottomright",inset=0,legend=c("1%","2%","3%","4%","5%"),col=c("red","green","blue","orange","cyan"),lty=c(1,2,3,4,5),lwd=c(3,2,2.5,2,2),bty="n")
    
    # fill in the list with the coeffes you compute
    # from the next line, you should infer that model should be the variable name of your model
    
    result <- list("Intercept"=coef(model)[1], "xCoeff"=coef(model)[2])
    return(result)
  }
  
 
#Q5
# return: string ("reject" to reject null hypothesis, "not-reject" to not reject null hypothesis)
ZTest <- function(x, test_type, alpha, pop_mean, pop_sd){
  #calculating the z score
  zscore=((mean(x)-pop_mean)/((pop_sd)/sqrt(length(x))))
  
  # If left tailed
  # Calculate the critical value for left-tailed and compare with zscore
  
  if(test_type=="left-tailed")
  {
    ltail=qnorm(alpha)
    if(zscore<ltail)
    {
      print("Reject")
      
    }
    else
    {
      print("Not Reject")
    }
  }
  
  # If right tailed
  # Calculate the critical value for right-tailed and compare with zscore
  
  else if(test_type=="right-tailed")
  {
    rtail=qnorm(1-alpha)
    if(zscore>rtail)
    {
      print("Reject")
    }
    else
    {
      print("Not Reject")
    }
  }
  
  #If two tailed
  #Calculate the critical value for ltwotail and rtwotail and compare with zscore
  
  else if(test_type=="two-tailed")
  {
    ltwotail=qnorm(alpha/2)
    rtwotail=qnorm(1-(alpha/2))
    if(zscore>rtwotail|zscore<ltwotail)
    {
      print("Reject")
    }
    else
    {
      print("Not Reject")
    }
  }
  
}
#ZTest(x=c(50, 95, 120, 85, 45, 90, 70, 60, 70, 50, 40, 80, 70,
#90, 75, 60, 90, 90, 75, 85, 80, 60, 110, 65, 80, 85, 85, 45,
# 60, 95, 110, 70, 75, 55, 80, 55),
# test_type='left-tailed',alpha=0.1, pop_mean=80, pop_sd=19.2)

# Output:Reject


#Q6
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  set.seed(123)
  samplemean=NULL
  msm=NULL
  sderror=NULL
  if(sampleSize>=30 && numberOfSamples>100)
  {
    
    if(populationDistribution=='normal')
    {
      x=rnorm(100000)
    }
    else if(populationDistribution=='uniform')
    { 
      x=runif(100000)
    }
    for(k in 1:numberOfSamples)
    { 
      samplemean[k]=mean(sample(x,sampleSize))
    }
    
    msm=mean(samplemean)
    sderror=sd(samplemean)
    hist(samplemean)
    
  }else 
  {
    print('error:check the values of sampleSize and numberof Samples')
  }
  
  # fill in the list with the mean and std you compute
  result=list('mean'=msm,'se'=sderror)
  return(result)
}  

#CLT('normal',45,9000)
#CLT('uniform,45,9000)
  
  
  
  
  
  
 


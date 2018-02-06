#Rohan Gupta
#February 6, 2018
#Pol Sci 4626 Problem Set 2

#1.
#Function to calculate Leemis' m statistic and Cho-Gains' d
benford=function(x,m=TRUE,d=TRUE){ #Function takes as input (i) vector of election returns and (ii) options that control whether m or d statistic should be calculated (default both)
  for(i in 1:length(x)){ #For each election return, find first significant digit
    x[i]<-substr(x[i],1,1)
  }
  x<-as.numeric(x) #Change type from character to numeric
  y<-NULL #Create vector of length 9 with values 0
  for(i in 1:9){
    y<-c(y,0)
  }
  for(i in 1:length(x)){ #For each first significant digit, add 1 to observed frequency
    y[x[i]]<-y[x[i]]+1
  }
  z<-y/length(x) #Observed proportional frequency = observed frequency/observed vote total (vector with full digit distribution)
  for(i in 1:length(y)){ #Calculate same expression in m and d
    y[i]<-y[i]/length(x)-log10(1+1/i)
  }
  leemis<-max(y) #m is max of this value
  chogains<-sqrt(sum(y^2)) #d is square root of sum of each value squared
  if(m&d){
    return(list(leemis,chogains,z)) #Output list containing results, including full digit distribution
  }
  else if(m){
    return(list(leemis,z))
  }
  else if(d){
    return(list(chogains,z))
  }
  else{
    return(list(z))
  }
}

#2.

#Rohan Gupta
#February 6, 2018
#Pol Sci 4626 Problem Set 2

# good documentation and good codes
# (-4) sink.benfords function does not work
sink.benfords=function(x){
  sink(file="Benford_Table.csv") 
  print.benfords(x)
  sink()
}
sink.benfords(1:100)
# this code will work. In order to use sink function, you need to specify the file name/path
# 96/100

export.benford <- function(vec){ # export.bendford function takes vec as an input,
  sink(file="Benford_StatTable.csv", append=TRUE, split=FALSE) # this will export outcome as Benford_StatTable.csv file
  print.benfords(vec) # this will rerun print.benfords function and print out the outcome
  sink()
}



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

benford(1:100)

#2.
#Function that will output table containing name of statistic, statistic as it was calculated, relevant number of asterisks, legend explaining asterisks
print.benfords=function(x){ #Function takes as input vector of election returns
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
  output<-table(c("Leemis'","Cho-Gains'","Legend"),c("Name","Statistic","Significance")) #Create output table
  output[1,1]<-"d" #Name of Cho-Gains' d statistic
  output[2,1]<-"m" #Name of Leemis' m statistic
  output[1,3]<-chogains #Cho-Gains' d statistic calculated
  output[2,3]<-leemis #Leemis' m statistic calculated
  legend<-"Signif. codes: 0.01 '***' 0.05 '**' 0.1 '*' 1" #Legend explaining asterisks
  output[3,1]<-substr(legend,1,15)
  output[3,2]<-substr(legend,16,30)
  output[3,3]<-substr(legend,31,45)
  a<-"" #Relevant number of asterisks
  b<-""
  if(chogains>=1.212){
    a<-"*" #One star for significance at a = .10 level
  }
  if(chogains>=1.330){
    a<-"**" #Two stars for significance at a = .05 level
  }
  if(chogains>=1.569){
    a<-"***" #Three stars for significance at a = .01 level
  }
  if(leemis>=.851){
    b<-"*" #One star for significance at a = .10 level
  }
  if(leemis>=.967){
    b<-"**" #Two stars for significance at a = .05 level
  }
  if(leemis>=1.212){
    b<-"***" #Three stars for significance at a = .01 level
  }
  output[1,2]<-a
  output[2,2]<-b
  return(output) #Output table
}
print.benfords(1:100)
#Function that uses print.benfords() to create csv containing table in directory provided as argument to function
sink.benfords=function(x,csv){
  print.benfords(x)
  sink(csv)
}

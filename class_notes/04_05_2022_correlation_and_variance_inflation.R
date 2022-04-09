require("wooldridge")
require("car")

#let's say that we want this linear model
linear_model = lm(colGPA ~ hsGPA + ACT + skipped, data = gpa1)

#create a dataframe that has the regressors that we are intersted in 
data = data.frame(gpa1$hsGPA, gpa1$ACT, gpa1$skipped)

#then we want to find the correlation matrix
corr_matrix = data.frame(cor(data))

#this checks that all of the values in the correlation matrix are less than some level
correlation_check = function(corr_matrix, threshold_amount){
  
  #the threshold amount is what we want to check at
  
  #let's loop through all values of the dataframe
  
  #loop through columns
  for (i in 1:length(colnames(corr_matrix))){
    
    #loop through rows
    for(j in 1:length(corr_matrix)){
      
      #save as variable and in this case we want the absolute value
      compare = abs(corr_matrix[i,j])
      
      #then want to see if the value is less than 0.9 and keep in mind that we have these values that are 1
      if(compare > threshold_amount && compare < 1){
        
        #output the information
        return(cat("correlation of", threshold_amount, "found"))
      }
    }
  }
  
  #we don't find a correlation of that size
  return(cat("no correlation at", threshold_amount, "found"))
}

#run the code
correlation_check(corr_matrix, 0.9)

#the other method we could use is the variance inflation factor

#variance inflation factor without function
variance_inflation = vif(linear_model)

#this how its usually done in class
variance_inflation < 10

#I would loop through just be certain
for (i in variance_inflation){
  
  #check if the variance inflation is bigger than 10
  if (i > 10){
    print("variance inflation greater than 10 found")
  }else{
    print("variance inflation less than 10")
  }
}

#here is all of it put into a function

variance_inflation_check = function(linear_model, threshold){
  
  #get the variance inflation
  variance_inflation = vif(linear_model)
  
  #loop through
  for (i in variance_inflation){
    
    #then check if its smaller
    if (i > threshold){
      
      return(cat("variance inflation exceeds", threshold, "amount"))
    }
  }
  
  return(cat("variance inflation is less than", threshold))
}

#variance inflation check
variance_inflation_check(linear_model, 10)

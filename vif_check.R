require("car")

#load the data
genderdata = read.csv("gender_data_final.csv")

#I used this for loop to fix the data

#extract the columns
columns = colnames(genderdata)

#extract the columns that we are interested in (the numeric ones)
columns = columns[2:length(columns)]

#now loop through the columns
for (i in columns){
  
  #we want to use the as.numeric method on the column
  genderdata[i] = as.numeric(unlist(genderdata[i]))
  
  #then we want to drop all the NaN
  genderdata[!is.na(genderdata[i])]
  
  print(i)
}

linear_model = lm(genderdata$LAST_CLOSE_ANN_TRR_5YR~genderdata$Bgei_Disclsre_Score+genderdata$Bgei_Data_Excllnce_Score+genderdata$Bgei_Femle_Leadrshp_Score+
                    genderdata$Bgei_Equal_Pay_Score+genderdata$Bgei_Sexl_Harssmnt_Plcy_Score+genderdata$Bgei_Inclsve_Cultre_Score+genderdata$Bgei_Prowomn_Brand_Score)


variance_inflation_check = function(linear_model, threshold){
  
  #get the variance inflation
  variance_inflation = vif(linear_model)
  
  #loop through
  for (i in variance_inflation){
    
    #then check if its smaller
    if (i > threshold){
      
      cat("regressor variance inflation factor is greater than", threshold, "\n")
    }else{
      cat("regressor variance inflation factor is less than", threshold, "\n")
    }
  }
}

variance_inflation_check(linear_model, 10)
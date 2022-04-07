require("wooldridge")

#define accept to reject functions
accept_reject = function(value, cutoff){
  if(value < cutoff){
    print("reject null hypothesis")
  }else{
    print("fail to reject null hypothesis")
  }
}

#for critical value
accept_reject_crit_val = function(value, conf_int){
  if(value > conf_int){
    print("reject null hypothesis")
  }else{
    print("fail to reject null hypothesis")
  }
}

#for confidence interval
accept_reject_conf_int = function(confidence_interval){
  
  if (0 > confidence_interval[1] && value < confidence_interval[2]){
    print("in interval: fail to reject null hypothesis")
  }else{
    print("not in interval: reject null hypothesis")
  }
}


#######################################################################################################################################
#(F-test)
#H0:ß3=ß4=0
#H1:H0 is false

#we have to define the unrestricted and the restricted model

unrestricted_model = lm(log(wage)~educ+exper+tenure+IQ,wage2)
restricted_model = lm(log(wage)~educ+exper,wage2)

#then we have to use the linearHypothesis method from the car package
require("car")

#to run the linear hypothesis we have to select what features we don't need
null_features = c("tenure", "IQ")

#######################################################################################################################################


#######################################################################################################################################
#p-value method
#######################################################################################################################################

#then run it through the lienar hypothesis to get the p-value
p_value = linearHypothesis(unrestricted_model, null_features)[2,6]

#then we want to check if the p-value is smaller than the significance level 
accept_reject(p_value, 0.01)

#######################################################################################################################################
#critical value method
#######################################################################################################################################

#find the degrees of freedom
deg_freed = unrestricted_model$df

#find the number of regressors that we are dropping
q = length(null_features)

#define a significance level of 5%
sig_level = 0.05

#then in this case we use the f-distribution to make our interval
f_stat_interval = qf(1 - sig_level, q, deg_freed)

#now we want to extract the f-value from the linearHypothesis test
f_value = linearHypothesis(unrestricted_model, null_features)[2,5]

#then run it through the accept reject
accept_reject_crit_val(f_value, f_stat_interval)

#######################################################################################################################################
#ANOVA and droppping all values
#######################################################################################################################################

#hypothesis
#H0:ß1=ß2=ß3=ß4=0
#H1:H0 is false

#define restricted and unrestricted model
unrestricted_model = lm(log(wage)~educ+exper+tenure+IQ,wage2)
restricted_model = lm(log(wage)~1,wage2)

#then use the linear hypothesis method with the null features and run it through
null_features = c("educ", "exper", "tenure", "IQ")
p_value = linearHypothesis(unrestricted_model, null_features)[2,6]
accept_reject(p_value, 0.05)

#alterntively we could use the ANOVA function get the values
p_value = anova(restricted_model, unrestricted_model)[2,6]
accept_reject(p_value, 0.05)

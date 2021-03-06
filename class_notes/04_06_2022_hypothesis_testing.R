require("wooldridge")

#define accept to reject functions
reject_fail = function(value, cutoff){
  if(value < cutoff){
    print("reject null hypothesis")
  }else{
    print("fail to reject null hypothesis")
  }
}

#for critical value
reject_fail_crit_value = function(value, crit_value, tail){
  
  if (tail == "left"){
    
    print("using left tail")
    if(value < crit_value){
      print("reject the null hypothesis")
    }else{
      print("fail to reject the null hypothesis")
    }
    
  }
  if(tail == "right"){
    if(value > crit_value){
      print("reject the null hypothesis")
    }else{
      print("fail to reject the null hypothesis")
    }
    
  }
  if(tail == "both"){
    print("both tails")
    if(value < abs(crit_value)){
      print("reject the null hypothesis")
    }else{
      print("fail to reject the null hypothesis")
    }
  }
}

#for confidence interval
reject_fail_conf_int = function(confidence_interval){
  
  if (0 > confidence_interval[1] && value < confidence_interval[2]){
    print("in interval: fail to reject null hypothesis")
  }else{
    print("not in interval: reject null hypothesis")
  }
}

#we have this hypothesis test
#H_0: B_2 = 0
#H_1: B_2 > 0

#let's start by defining the linear model
linear_model = lm(log(wage)~educ+exper+tenure, data = wage1)

#let's extract the p_value coefficient
p_value = summary(linear_model)$coef[3,4]

#account for the two tails
p_value = 0.5 * p_value

#run through accept reject
reject_fail(p_value, 0.01)

#we need to check the degrees of freedom to find if we should use qnorm or qstat
qnorm_or_qstat = function(deg_freedom, alpha, tail){
  
  if (deg_freedom > 120){
    
    print("using normal distribution")
    
    if(tail == "one"){
      print("using one tail")
      critical_value = qnorm(1 - alpha)
    }
    
    if (tail == "two"){
      print("using two tail")
      critical_value = qnorm(1 - (alpha / 2))
    }
  }
  
  else{
    
    print("using t-distribution")
    
    if(tail == "one"){
      print("using one tail")
      critical_value = qt(1 - alpha, deg_freedom)
    }
    
    else{
      print("using two tail")
      critical_value = qt(1 - (alpha / 2), deg_freedom)
    }
  }
}

#let's get the degrees of freedom
deg_freedom = linear_model$df

#now let's define a significance level
sig_level = 0.1

#let's get the confidence interval
critical_value = qnorm_or_qstat(deg_freedom, sig_level)

#we need to extract the t-stat
t_stat = summary(linear_model)$coef[3,3]

#run through the accept or reject
reject_fail_conf_int(t_stat, critical_value)

#let's try it at a 0.05 sig_level
sig_level = 0.05

#re-rerun critical value
critical_value = qnorm_or_qstat(deg_freedom, sig_level)

#then run it through again
reject_fail_crit_val(t_stat, critical_value)

#let's say that we have this linear model
linear_model = lm(log(rd)~log(sales),data=rdchem)

#Our hypotheses:
#H0:ß1=0
#H1:ß1 is non zero.

#we define the confidence interval at 90%
confidence_interval = confint(lm(log(rd)~log(sales),data=rdchem),parm="log(sales)",level=0.9)

#run it through accept reject
reject_fail_conf_int(confidence_interval)

require("wooldridge")

#define accept to reject functions
accept_reject = function(value, cutoff){
  if(value < cutoff){
    print("reject null hypothesis")
  }else{
    print("fail to reject null hypothesis")
  }
}

accept_reject_conf_int = function(value, conf_int){
  if(value > conf_int){
    print("reject null hypothesis")
  }else{
    print("fail to reject null hypothesis")
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
accept_reject(p_value, 0.01)

#we need to check the degrees of freedom to find if we should use qnorm or qstat
qnorm_or_qstat = function(deg_freedom, signficance_level){
  
  if (deg_freedom > 120){
    
    print("using qnorm")
    critical_value = qnorm(1- signficance_level)
    
  }else{
    
    print("using qstat")
    critical_value = qstat(1 - signficance_level)
  }
  return(critical_value)
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
accept_reject_conf_int(t_stat, critical_value)

#let's try it at a 0.05 sig_level
sig_level = 0.05

#re-rerun critical value
critical_value = qnorm_or_qstat(deg_freedom, sig_level)

#then run it through again
accept_reject_conf_int(t_stat, critical_value)

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
accept_reject_crit_val(t_stat, critical_value)

#let's say that we have this linear model
linear_model = lm(log(rd)~log(sales),data=rdchem)

#Our hypotheses:
#H0:ß1=0
#H1:ß1 is non zero.

#we define the confidence interval at 90%
confidence_interval = confint(lm(log(rd)~log(sales),data=rdchem),parm="log(sales)",level=0.9)

#run it through accept reject
accept_reject_conf_int(confidence_interval)

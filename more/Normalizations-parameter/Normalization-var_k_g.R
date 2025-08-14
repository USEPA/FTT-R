#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#

#### Header Info ####
## Normalization Functions for Parameter,  var_k_g

## Information from parameters_master.RData
# t(FishToxTranslator::parameters_master[ 4 ,]) #Uncomment to pull data from package into R

#### Derivation Notes: ####

#### Algorithm for fitting var_k_g ####
## This algorithm was developed by M.Etterson (etterson.matthew@epa.gov) and N.Pollesch (pollesch.nathan@epa.gov)

## Derive variance in k bounds based on max size at given time
# Can use multiple 'max size' at 'given time' types of statements
# The example given below uses the following statement to arrive at bounds in k_variance
# "The fastest growing fish can reach 99% of its maximum length in the time that the mean growing fish
#  can reach 95% of its maximum size"

# 1) In this case, first use mean growth rate (k) to find how long it takes (t_to_rat_mean) the average
#  fish to reach 95% of its size. Thus, solve for t_to_rat_mean when z_rat_mean=0.95
# 2) You then use that time (t=t_to_rat_mean) to solve for the value of k (k_for_z_rat_max) that corresponds
#  to a fast growing fish reaching 99% of its size in that time (z_rat_max=0.99)
# 3) Then use value k_for_z_rat_max and k to solve for a value of k_variance that has k_for_z_rat_max as
#  the 99th percentile of a normal distribution with mean as 'k' and variance as 'k_variance'


# Set the parameters
k=0.009 # Mean Kappa (vB growth rate)
z0=5.6 # Size at hatch (mm)
zinf=74 # Max Length (mm)


z_rat_mean=0.95 # Percent of growth to reach at mean growth
z_rat_max=0.99 # Percent of growth that can be reached by fastest fish in time for mean fish to reach z_rat_mean


VarianceK <- function( sigma_k , k = 0.009 , z0 = 5.6 , zinf = 74 , zrat=0.95 , zratmax=0.99 ){

  # sigma_k = variance of k
  # k = growth rate (daily)
  #
  TimeToZrat = ( log(1-z0/zinf)-log(1-zrat) )/ k
  krat_max = ( log(1-z0/zinf)-log(1-zratmax) )/ TimeToZrat


  k99 = qnorm( 0.99 , k , sigma_k )

  dif_k <- ( k99 - krat_max ) ^ 2

  return(dif_k)

}

## Run optimizer at default values VarianceK() function

# Set initial guess
sigma_iter <- 0.1
# Run optimizer at initial guess, lower bound of 0, upper bound of 100 (VERY high upper bound)
Otpt <- optim( sigma_iter , fn = VarianceK ,
               lower = 0 , upper=100, method = "Brent" ,
               hessian = TRUE)

Otpt$par
# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$par^2
print(k_variance)

# ### RUN SAME OPTIMIZER WITH DIFFERENT VALUES ### NOTE THIS GIVES ERRORS
#
# sigma_iter <- 0.1
#   # Run optimizer at initial guess, lower bound of 0, upper bound of 100 (VERY high upper bound)
#   Otpt <- optim( sigma_iter , fn = VarianceK , k=0.009, z0=5.6,zinf=74,zrat=0.98,zratmax=0.99,
#                  lower = 0 , upper=100, method = "Brent" ,
#                  hessian = TRUE)
#
#   Otpt$par
#   # Convert from standard deviation to variance by squaring the output
#   k_variance<-Otpt$par^2
#   print(k_variance)


# Run different optimizer lower bound of 0, upper bound of 100 (VERY high upper bound)
Otpt <- optimize(VarianceK , interval=c(0,100) , k=0.009, z0=5.6, zinf=74, zrat=0.00, zratmax=0.1)
Otpt
# Convert from standard deviation to variance by squaring the output
k_variance<-Otpt$minimum^2
print(k_variance)

#Explore difference in assumptions of mean and max rats
rats<-list(zrat<-c(0.00,0.05,0.10,0.25,0.50,0.75,0.90,0.95),zratmax<-c(0.04,0.09,0.14,0.29,0.54,0.79,0.94,0.99))
rats[[1]][1]
rats[[2]][1]
k_var_out<-c()
length(rats[[1]])
for(i in 1:length(rats[[1]])){
  Otpt <- optimize(VarianceK , interval=c(0,100) , k=0.009, z0=5.6, zinf=76, zrat=rats[[1]][i], zratmax=rats[[2]][i])
  # Convert from standard deviation to variance by squaring the output
  k_var_out[i]<-Otpt$minimum^2
}
k_var_out
plot(log(k_var_out))
abline(h=mean(log(k_var_out)))

#Bluegill
mean(k_var_out) #2.272E-07

#Fathead Minnow #5.516E-06
mean(k_var_out)



#### Example Normalizations: ####

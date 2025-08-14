# Conversions for Medaka Parameterization

#### z_hatch ####
#all hatch size values are similar and for both sexes, so we will average them all
z_hatches<-c(3.8, 4.2, 3.6, 5.8) # Study ID 472, 487
mean(z_hatches)
#-> 4.35
# z_hatch = 4.35 mm

#### z_inf ####
# Maximum reported total length for broodstock lab populations; 
# SID 489 https://doi.org/10.1643/CI-09-190

#### k_g ####
#mean for SID 486, 490;
mean(c(0.00947, 0.010))

#### allo_int, allo_slope ####
#plotting allometry
allo_int <- 2.16E-05
allo_slope <- 2.79
length <- c(0:55)

mass <- allo_int * length^allo_slope
plot(mass, xlab="length_mm_TL")
title(main="Medaka Allometry")

#### survival Medaka ####
#survival max (s_max) is calculated as 99% chance of mortality at the average lifespan
# lifespan is years is multiplied by days and then the 1% survival rate for that many days is used by taking .001^(1/(lifespan_in_years*365))
.001^(1/(1.25*365.25)) #average wild lifespan of 16 months assumed
# s_max = 0.984984

#survival min
e<-c(0.9980024, 0.9993989,  0.9995362, 0.9995439) #juvenile values only, too high

mean(e)
#s_min = 


#plotting the survival function to fit s_a and s_b
surv<-function(z,s_b,s_a,s_min=0.9495851,s_max=0.9976387){
  out<-s_min+((s_max-s_min)/(1+(exp(s_a*(s_b-z)))))
  return(out)
}

z_med<-seq(from=4,to=52,length.out=175)

plot(x=z_med,y=surv(z=z_med,s_b=100,s_a=.3,s_min=0.9495851,s_max=0.9976387),type="l",xlab="Size (mm)",ylab="Daily Survival")

title (main = "Medaka Surival")

#### z_repro ####
#->29
# Smallest converted/reported value for SID 495; https://doi.org/10.1643/CI-09-190

#### hatch rate ####
#Mean hatch rate for SIDs 482, 497, 519, 871.
#SID 497 reported high hatch-ability in optimized lab conditions
mean (c(.758, .73, .9, .744, .587 )) 
# 0.7438

#### Fecundity ####
#reproductive length range for medaka
L<-29:52
W <- 2.16E-05 * L^2.79

#Plotting linear relationship 
plot(L,(204.6*W-9.9)/4, xlim=c(29,52), ylim=c(0,80)) #Teather et al., 2000, function of egg/g for 4 days/4 spawning intervals 
points(L,-.91+.62*(1.177*L), col = "gray") #Fujimoto et al., 2024

## Create data sets for fitting
x <- L
y1 <- (204.6*W-9.9)/4
fecumed1 <- data.frame(x = x, y = y1)

x2 <- L
y2 <- -.91+.62*(1.177*L)
fecumed2 <- data.frame(x = x, y = y2)

data.all.med<-data.frame(x=append(x,x),y=append(y1,y2))

#plot to confirm proper data.frame
plot(fecumed1, col="green")
points(fecumed2, col="blue")
points(data.all.med)

# Fitting code using nonlinear least square:
nlc <- nls.control(maxiter = 10000)
log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all.med, start = list(a = 1, b = 1))
log_fit #a~0.01383 b~2.07711

fitted.data <-data.frame(x=x, y=predict(log_fit, list(x=x))) #creates dataframe from best fit parameters at the values of x supplied

library(tidyverse)
ggplot(data.all.med, aes(x, y)) + geom_point(color="red", alpha=.5) + geom_line(alpha=.5) + geom_line(data=fitted.data) #plot


fec_int <- 0.01383
fec_slope <- 2.07711

fecundityTestmed<-function(x){
  return(fec_int*(x^fec_slope))}

plot(29:52,fecundityTestmed(29:52))
title(main="Medaka Fecundity")
#points(L,(204.6*W-9.9)/4, col="green")
#points(L,-.91+.62*(1.177*L), col = "blue")

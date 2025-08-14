#~             ,''''''''''''''.
#~~           +     USEPA      +
#~   >~',*> <   FISH TOXICITY   }
#~~           +   TRANSLATOR   +
#~             `..............'
#~~
#~  N. Pollesch - pollesch.nathan@epa.gov
#
#
# #### Header Info ####
# ## Normalization Functions for Parameter,  fecu_slope
#
# ## Information from parameters_master.RData
# # t(FishToxTranslator::parameters_master[ 18 ,]) #Uncomment to pull data from package into R
#
# #### Derivation Notes: ####
#
# #### Example Normalizations: ####
# library(FishToxTranslator)
#
# #reproductive length for FHM
# L<-44:75
# #Plotting linear relationship of size to eggs/spawn from Andrews and Flickinger (1973) (See Life History DB)
# plot(L,-451+20*L) #warm water
# points(L,-372+14*L) #cold water
#
# fecundityTest<-function(x,a,b){
#   return((x^b)*a)}
#
# #plotting
# plot(L,fecundityTest(L,a=3.08*10^-3,b=2.8085),ylim=c(100,1000)) #plot current FHM parameters
# points(L,-451+20*L,col="Red") #plot warm linear params
# points(L,-372+14*L,col="Blue") #plot cold linear params
#
# ## Create data sets for fitting
# x1 <- L
# y1 <- -372+(14*x)
# data1 <- data.frame(x = x1, y = y1)
#
# x2 <- L
# y2 <- -451+(20*x)
# data2 <- data.frame(x = x2, y = y2)
#
# data.all<-data.frame(x=append(x1,x2),y=append(y1,y2))
#
# #plot to confirm proper data.frame
# plot(data1)
# points(data2)
# points(data.all,col="blue")
#
# # Fitting code using nonlinear least square:
# nlc <- nls.control(maxiter = 10000)
# log_fit <- nls(y ~ a*x^b, control=nlc, data = data.all, start = list(a = 1, b = 1))
# log_fit #a~0.5847 b~1.6978
#
# # Repeat for warm and cold water individually
# # cold
# nlc <- nls.control(maxiter = 10000)
# log_fit <- nls(y ~ a*x^b, control=nlc, data = data1, start = list(a = 1, b = 1))
# log_fit #a~0.2662 b~1.8208
# # warm
# nlc <- nls.control(maxiter = 10000)
# log_fit <- nls(y ~ a*x^b, control=nlc, data = data2, start = list(a = 1, b = 1))
# log_fit #a~0.9709 b~1.6209
#
# # Plotting
# plot(L,fecundityTest(L,a=3.08*10^-3,b=2.8085),ylim=c(100,1000)) #plot current FHM parameters
# points(L,fecundityTest(L,a=.5747,1.6968),col="purple") #plot fitted (power law) parameters from equal weight of warm and cold
# points(L,fecundityTest(L,a=.2662,1.8208),col="lightblue") #plot fitted (power law) parameters from cold
# points(L,fecundityTest(L,a=.9709,1.6209),col="pink") #plot fitted (power law) parameters from warm
# points(L,-451+20*L,col="Red") #plot warm linear params
# points(L,-372+14*L,col="Blue") #plot cold linear params
#
#
# #### FHM eggs/gram conversion ####
# fhm_mmSL_g <- (4.283333e-05 * (x^2.74))* 1700 #absolute fecundity, allometric relationship with eggs per gram body weight
# data3<- data.frame(x = x, y = fhm_mmSL_g/12) #add to dataframe divide by 12 for max spawns
#
# plot(log(data3)) #view data on log scale
# diff(log(data3$y))/diff(log(data3$x)) #get an estimate of slope in log linear form for fitting
# nlc <- nls.control(maxiter = 100000)
# #couldn't fit in power law form, converted to log linear form and fit
# log_fit2 <- nls(y ~ a+b*x, control=nlc, data = log(data3), start = list(a = .072, b = 2.74),algorithm="port")
# log_fit2 #a~ -5.105 b~2.74
# # convert back to linear form
# exp(-5.105) #a~ 0.006066339
#
# #plot with other fits and add this data in green/lightgreen for data and fit, respectively
# plot(L,fecundityTest(L,a=3.08*10^-3,b=2.8085),ylim=c(100,1000)) #plot current FHM parameters
# points(L,fecundityTest(L,a=.5747,1.6968),col="purple") #plot fitted (power law) parameters from equal weight of warm and cold
# points(L,fecundityTest(L,a=.2662,1.8208),col="lightblue") #plot fitted (power law) parameters from cold
# points(L,fecundityTest(L,a=.9709,1.6209),col="pink") #plot fitted (power law) parameters from warm
# points(L,fecundityTest(L,a=.0061,2.74),col="lightgreen") #plot fitted (power law) parameters from warm
# points(L,-451+20*L,col="Red") #plot warm linear params
# points(L,-372+14*L,col="Blue") #plot cold linear params
# points(L,fhm_mmSL_g/12,col="Green")
#

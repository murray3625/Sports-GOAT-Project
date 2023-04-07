library(wesanderson)

#initialize results for 
#N= 10, 100, 1000
#C = 0, .01, .05, .1, .5

results_n_10_c_0 = c(1, rep(0, 99))
results_n_100_c_0 = c(1, rep(0, 99))
results_n_1000_c_0 = c(1, rep(0, 99))


results_n_10_c_01 = c(1, rep(0, 99))
results_n_100_c_01 = c(1, rep(0, 99))
results_n_1000_c_01 = c(1, rep(0, 99))

results_n_10_c_05 = c(1, rep(0, 99))
results_n_100_c_05 = c(1, rep(0, 99))
results_n_1000_c_05 = c(1, rep(0, 99))

results_n_10_c_10 = c(1, rep(0, 99))
results_n_100_c_10 = c(1, rep(0, 99))
results_n_1000_c_10 = c(1, rep(0, 99))

results_n_10_c_50 = c(1, rep(0, 99))
results_n_100_c_50 = c(1, rep(0, 99))
results_n_1000_c_50 = c(1, rep(0, 99))

#This is the main function of interest. We are looking at the product of the CDFs of previous generations (with lower mean values) multiplied n times, where n is  the number of players per generation
#integrated over the pdf of the skill of a player in the current generation
integrand <- function(x, i, n, inc_const) {
  (prod(pnorm(x, mean = seq(from =0, to = inc_const*(i-2), length.out=i-1)))^n)*dnorm(x, mean= inc_const*(i-1))  
  
}

#Run this integration for 100 years/generations, each year incrementing the skill level and number of previous generations
#Note that since sigma=1, we're just integrating from  mean - 5 to mean +5, where the mean is of the current generation
#First we calculate the probability 1 player will be the best, then from there, we calculate the probability that at least 1 player in the 
#current generation will be better than all players of all previous generations
for(i in 2:100){
print(i)
  
result_10_0 = integrate(Vectorize(integrand), lower = -5, upper = 5, subdivisions = 100000L, i=i, inc_const=0, n=10, rel.tol = .5e-11,  abs.tol = 0)$value
result_100_0 = integrate(Vectorize(integrand), lower = -5, upper = 5, subdivisions = 100000L, i=i, inc_const=0, n=100, rel.tol = .5e-11,  abs.tol = 0)$value
result_1000_0 = integrate(Vectorize(integrand), lower = -5, upper = 5, subdivisions = 100000L, i=i, inc_const=0, n=1000, rel.tol = .5e-11,  abs.tol = 0)$value
  
  
result_10_01 = integrate(Vectorize(integrand), lower = (i*.01)-5, upper = (i*.01)+5, subdivisions = 100000L, i=i, inc_const=.01, n=10, rel.tol = .5e-11, abs.tol = 0)$value
result_100_01 = integrate(Vectorize(integrand), lower = (i*.01)-5, upper = (i*.01)+5, subdivisions = 100000L, i=i, inc_const=.01, n=100, rel.tol = .5e-11,  abs.tol = 0)$value
result_1000_01 = integrate(Vectorize(integrand), lower =(i*.01)-5, upper = (i*.01)+5, subdivisions = 100000L, i=i, inc_const=.01, n=1000, rel.tol = .5e-11,  abs.tol = 0)$value

result_10_05 = integrate(Vectorize(integrand), lower = (i*.05)-5, upper = (i*.05)+5, subdivisions = 100000L, i=i, inc_const=.05, n=10, rel.tol = .5e-11, abs.tol = 0)$value
result_100_05 = integrate(Vectorize(integrand), lower = (i*.05)-5, upper = (i*.05)+5, subdivisions = 100000L, i=i, inc_const=.05, n=100, rel.tol = .5e-11,  abs.tol = 0)$value
result_1000_05 = integrate(Vectorize(integrand), lower = (i*.05)-5, upper = (i*.05)+5, subdivisions = 100000L, i=i, inc_const=.05, n=1000, rel.tol = .5e-11,  abs.tol = 0)$value

result_10_10 = integrate(Vectorize(integrand), lower = (i*.1)-5, upper = (i*.1)+5, subdivisions = 100000L, i=i, inc_const=.1, n=10, rel.tol = .5e-11,  abs.tol = 0)$value
result_100_10 = integrate(Vectorize(integrand), lower = (i*.1)-5, upper = (i*.1)+5, subdivisions = 100000L, i=i, inc_const=.1, n=100, rel.tol = .5e-11,  abs.tol = 0)$value
result_1000_10 = integrate(Vectorize(integrand), lower = (i*.1)-5, upper = (i*.1)+5, subdivisions = 100000L, i=i, inc_const=.1, n=1000, rel.tol = .5e-11,  abs.tol = 0)$value

result_10_50 = integrate(Vectorize(integrand), lower = (i*.5)-5, upper = (i*.5)+5, subdivisions = 100000L, i=i, inc_const=.5, n=10, rel.tol = .5e-11,  abs.tol = 0)$value
result_100_50 = integrate(Vectorize(integrand), lower = (i*.5)-5, upper = (i*.5)+5, subdivisions = 100000L, i=i, inc_const=.5, n=100, rel.tol = .5e-11,  abs.tol = 0)$value
result_1000_50 = integrate(Vectorize(integrand), lower = (i*.5)-5, upper = (i*.5)+5, subdivisions = 100000L, i=i, inc_const=.5, n=1000, rel.tol = .5e-11,  abs.tol = 0)$value


#######
results_n_10_c_0[i]= 1-((1-result_10_0)^10)
results_n_100_c_0[i] = 1-((1-result_100_0)^100)
results_n_1000_c_0[i] = 1-((1-result_1000_0)^1000)

results_n_10_c_01[i]= 1-((1-result_10_01)^10)
results_n_100_c_01[i] = 1-((1-result_100_01)^100)
results_n_1000_c_01[i] = 1-((1-result_1000_01)^1000)

results_n_10_c_05[i]= 1-((1-result_10_05)^10)
results_n_100_c_05[i] = 1-((1-result_100_05)^100)
results_n_1000_c_05[i] = 1-((1-result_1000_05)^1000)

results_n_10_c_10[i]= 1-((1-result_10_10)^10)
results_n_100_c_10[i] = 1-((1-result_100_10)^100)
results_n_1000_c_10[i] = 1-((1-result_1000_10)^1000)

results_n_10_c_50[i]= 1-((1-result_10_50)^10)
results_n_100_c_50[i] = 1-((1-result_100_50)^100)
results_n_1000_c_50[i] = 1-((1-result_1000_50)^1000)

}

#Plot the results above. There isn't much action after the curves stabilize, so we're only displaying over 30 generations, not 100
#Why get stability? At some point, the probability that a player from more than G generations ago is still the highest value effectively goes to 0
# at that point, it's only really a (weighted by recency) competition of the past G generations. Think about it, a N(-5, 1) will almost never exceed a N(0, 1), but a N(-.1, 1) may occasionally

wp = wes_palette("Darjeeling1", 5)
plot(results_n_1000_c_50[1:30], ylim=0:1, type='l', col=wp[1], xlab ="generation", ylab ="GOAT probability", main = "GOAT Probability with different N and C" )
lines(1:30, results_n_1000_c_10[1:30], col=wp[2] )
lines(1:30, results_n_1000_c_05[1:30], col=wp[3] )
lines(1:30, results_n_1000_c_01[1:30], col=wp[5] )
lines(1:30, results_n_100_c_50[1:30], lty=2, col=wp[1])
lines(1:30, results_n_100_c_10[1:30], lty=2, col=wp[2])
lines(1:30, results_n_100_c_05[1:30], col=wp[3] , lty=2)
lines(1:30, results_n_100_c_01[1:30], col=wp[5] , lty=2)
lines(1:30, results_n_10_c_50[1:30], lty=3, col=wp[1])
lines(1:30, results_n_10_c_10[1:30], col=wp[2], lty=3 )
lines(1:30, results_n_10_c_05[1:30], col=wp[3], lty=3 )
lines(1:30, results_n_10_c_01[1:30], col=wp[5], lty=3 )
lines(1:30, results_n_1000_c_0[1:30], type='l', lty=1)
#lines(1:100, results_n_100_c_0, col='blue', lwd=4 )
#lines(1:100, results_n_10_c_0, col='blue' )
legend(x =3.2, y = .77, legend=c("c=.5, N=1000", "c=.5, N=100","c=.5, N=10"), col=wp[1], lty=1:3, lwd=2, horiz=T, box.lty=0, cex=.67, bg="transparent")
legend(x =3.2, y = .7, legend=c("c=.1, N=1000", "c=.1, N=100","c=.1, N=10"), col=wp[2], lty=1:3, lwd=2, horiz=T, box.lty=0, cex=.67, bg="transparent")
legend(x =3.2, y = .63, legend=c("c=.05, N=1000", "c=.05, N=100","c=.05, N=10"), col=wp[3], lty=1:3, lwd=2, horiz=T, box.lty=0, cex=.65, bg ="transparent")
legend(x =3.2, y = .56, legend=c("c=.01, N=1000", "c=.01, N=100","c=.01, N=10"), col=wp[5], lty=1:3, lwd=2, horiz=T, box.lty=0, cex=.65, bg ="transparent")
legend(x=12.5, y=.49, legend="c=0, N=1000", col=1, lty=1, lwd=2, horiz=T, box.lty=0, cex=.67, bg ="transparent")



#Let's turn the previous plot on it's head. Instead of keeping c fixed and looking at probabilities over the generations and different values of N
#We can instead fix the generation and plot the probabilities for different values of C
#Below we plot the "stable probability" for different c values from 0 to 1 at generation=100, when N=1000
results_n_1000_c_i = rep(0,101)
for(i in 0:100){
  print(i)
  result_1000_i = integrate(Vectorize(integrand), lower = (i)-5, upper = (i)+5, subdivisions = 100000L, i=100, inc_const=i/100, n=1000, rel.tol = .5e-11,  abs.tol = 0)$value
  
  results_n_1000_c_i[i+1] = 1-((1-result_1000_i)^1000)
}  

#rate of improvement vs Goat probability - note that this is probably dependent on a ratio of c/sigma, but since sigma=1, we can use the rate itself here 
plot((0:100)/100, results_n_1000_c_i, xlab = "Average improvement per generation",  ylab ="probability of GOAT in 100th generation", main = "Long Term GOAT probability (N=1000)", cex=.8)



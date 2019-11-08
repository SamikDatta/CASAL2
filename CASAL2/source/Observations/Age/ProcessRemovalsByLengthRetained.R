
# unit test: Process Removals By Length Retained + Retained Total

rm(list = ls())

age<-1:20
length_bins = 30:46
Sel.a50 <- 4  # total catch
Sel.ato95 <- 1
Re.a50    <- 6 # retained catch within catch
Re.ato95  <- 1
disc_mort = 0.5 # discard mortality

# vB parameters
Gr.Loo <- 55.7
Gr.k   <- 0.14
Gr.to  <- -0.82
Gr.cv  <- 0.10

N<-c(100000, 90000, 80000, 70000, 60000, 50000, 40000, 30000, 20000, 10000, rep(0, 10))

sell<-function(x, a50, ato95, a = 1){ #logistic
  y<-a/(1+19^((a50 - x)/ato95))
  return(data.frame(x = x, y = y))
}

l_from_a = function(age, length_bins, catch) {
  mu = numeric(length(age))
  sigma = mu
  age_length_matrix = matrix(0, nrow = length(age), ncol = length(length_bins)-1)
  for (j in 1:length(age)) { # go through each age group
    mu[j] = Gr.Loo*(1-exp(-Gr.k*(j - Gr.to))) # mean length at age
    sigma[j] = mu[j]*Gr.cv
    age_length_matrix[j, ] =  pnorm(length_bins[2:length(length_bins)], mu[j], sigma[j]) - pnorm(length_bins[1:(length(length_bins)-1)], mu[j], sigma[j])
  } # use cumulative normal (pnorm) to work out proportions BETWEEN length bins
  length_vec = numeric(length(length_bins)-1)
  for (j in 1:length(age)) { # go over each age
    length_vec = length_vec + catch[j]*age_length_matrix[j, ] # scale lengths by number caught at age
  }
  return(length_vec)
}

sFish <- sell(age, Sel.a50, Sel.ato95)
sRe   <- sell(age, Re.a50, Re.ato95) # selectivities
exploitation = 0.0702179 # number taken directly out of Casal2 output 
Ncaught <- sFish$y * N * exploitation # all catch
Nretained <- sRe$y * Ncaught # retained catch
Ndiscards = Ncaught - Nretained # discards
Ndiscardsdead = Ndiscards*disc_mort # discards alive
identical(Nretained + Ndiscards, Ncaught) # check that numbers match

# Getting length distributions from ages for removals_by_length_retained
length_caught = l_from_a(age, length_bins, Ncaught)
length_ret = l_from_a(age, length_bins, Nretained)

# Observations

# Now create observation for length retained
sRet <- sell(age, 7, 0.9) # selection for a50 = 7, ato95 = 0.9
obs_catch <- l_from_a(age, length_bins, sRet$y * N)  # all catch as length
obs_ret = obs_catch/sum(obs_catch) # normalised
exp_ret = l_from_a(age, length_bins, Nretained) # numbers from guess parameters in input file
exp_ret = exp_ret/sum(exp_ret) # scale to 1
length_retained = cbind(length_bins[1:(length(length_bins)-1)], obs_ret, exp_ret) # use these values in unit test
colnames(length_retained) = c('length', 'observed', 'expected')


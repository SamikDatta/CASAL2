
# unit test: Process Removals By Age Retained + Retained Total

rm(list = ls())

age<-1:20
Sel.a50 <- 4  # total catch
Sel.ato95 <- 1
Re.a50    <- 6 # retained catch within catch
Re.ato95  <- 1
disc_mort = 0.5 # discard mortality

N<-c(100000, 90000, 80000, 70000, 60000, 50000, 40000, 30000, 20000, 10000, rep(0, 10))

sell<-function(x, a50, ato95, a = 1){ #logistic
  y<-a/(1+19^((a50 - x)/ato95))
  return(data.frame(x = x, y = y))
}

sFish <- sell(age, Sel.a50, Sel.ato95)
sRe   <- sell(age, Re.a50, Re.ato95) # selectivities
exploitation = 0.0702179 # number taken directly out of Casal2 output 
Ncaught <- sFish$y * N * exploitation # all catch
Nretained <- sRe$y * Ncaught # retained catch
Ndiscards = Ncaught - Nretained # discards
Ndiscardsdead = Ndiscards*disc_mort # discards alive
identical(Nretained + Ndiscards, Ncaught) # check that numbers match


# Observations

# Now create observation for age total
sTot <- sell(age, 5, 0.8) # selection for a50 = 5, ato95 = 0.8
obs_catch <- sTot$y * N  # all catch
obs_total = obs_catch[3:15]/sum(obs_catch[3:15]) # normalised
exp_total = Ncaught[3:15]/sum(Ncaught[3:15]) # numbers from guess parameters in input file
age_total = cbind(age[3:15], obs_total, exp_total) # use these values in unit test
colnames(age_total) = c('age', 'observed', 'expected')


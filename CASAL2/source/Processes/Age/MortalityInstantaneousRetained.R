
# unit test Instantaneous Mortality Retained

#
# get partition from input files
#  and set up bio parameters M, retained catch, selectivities (catch & retained)
#

age<-1:20
max_age <- max(age)
M<-0.0
Gr.Loo <- 55.7
Gr.k   <- 0.14
Gr.to  <- -0.82
Gr.cv  <- 0.10

Sel.a50 <- 4  # total catch
Sel.ato95 <- 1

Re.a50    <- 6 # retained catch within catch
Re.ato95  <- 1

disc_mort = 0.5 # discard mortality

# From out.csl after running casal2
N<-c(100000, 90000, 80000, 70000, 60000, 50000, 40000, 30000, 20000, 10000, rep(0, 10))

sell<-function(x, a50, ato95, a = 1){ #logistic
  y<-a/(1+19^((a50 - x)/ato95))
  return(data.frame(x = x, y = y))
}

sFish <- sell(age, Sel.a50, Sel.ato95)
sRe   <- sell(age, Re.a50, Re.ato95) # selectivities
exploitation = 0.0702179 # number taken directly out of Casal2 output

Ncaught <- sFish$y * N * exploitation  # all catch
Nretained <- Ncaught * sRe$y # retained catch
Ndiscards = Ncaught - Nretained # discards
Ndiscardsdead = Ndiscards*disc_mort # discards alive
identical(Nretained + Ndiscards, Ncaught) # check that numbers match


## CHECK PROCESS IN CASAL2

# Final partition
final_partition = N - Nretained - Ndiscardsdead # final partition

# weights of catches in table
mean_weight_by_age = c(0.0249609, 0.082166, 0.176724, 0.304307, 0.457405, 0.627729, 0.807533,
                       0.990237, 1.17062, 1.34479, 1.51004, 1.66461, 1.80755, 1.93849,
                       2.05751, 2.16498, 2.2615, 2.34778, 2.42461, 2.49279)
tot_weight = sum(Ncaught * mean_weight_by_age)
ret_weight = sum(Nretained * mean_weight_by_age)
disc_weight = sum(Ndiscards * mean_weight_by_age)
disc_dead_weight = sum(Ndiscardsdead * mean_weight_by_age) # four numbers should be the same as the table of catches



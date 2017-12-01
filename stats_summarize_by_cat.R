# counting number of individuals within a group (e.g. unemployed)
# who are in different deciles of transit access

library(ineq)
library(splitstackshape)
library(wCorr)

rm(list = ls())

c <- read.table("access.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

inc_data <- read.table("income.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
inc_data[ inc_data == "x" ] <- NA
inc_data[ inc_data == "F" ] <- NA
inc_data[ inc_data == ".." ] <- NA
inc_data[ inc_data == "..." ] <- NA

unemdata <- read.csv("unemployment.csv", header = TRUE, sep = ",", check.names = FALSE)

q <- merge(unemdata,inc_data,by.x = "dauid", by.y = 0)

q <- merge(c,q,by.x = "dauid", by.y = "dauid")

q$pop_density_km2 <- as.numeric(as.character(q$pop_density_km2))
q <- subset(q, q$pop_density_km2 > 200)


q$cduid <- substr(q$dauid,1,4)
q <- subset(q, as.character(q$cduid) != '2456') # st jean

qs <- quantile(q$i2,  probs = seq(0, 1, 0.1), na.rm = TRUE)
b10 <- qs[2]
b20 <- qs[3]
b30 <- qs[4]
# #
q$but10 <- 0
q$but10[q$i2 < b10] <- 1
# #
q$but20 <- 0
q$but20[q$i2 < b20] <- 1
# #
q$but30 <- 0
q$but30[q$i2 < b30] <- 1
#
q$u_unem <- as.numeric(as.character(q$u_unem))

sum(q$u_unem, na.rm = TRUE)

aggregate(q$u_unem, by=list(q$but30), FUN=sum, na.rm = TRUE)[2,2]

aggregate(q$u_unem, by=list(q$but20), FUN=sum, na.rm = TRUE)[2,2]

aggregate(q$u_unem, by=list(q$but10), FUN=sum, na.rm = TRUE)[2,2]

# plots the relataionship between access to jobs and household income

library(ggplot2)
library(ggthemes)

setwd("~/Dropbox/work/canada_access")

DA_pop <- read.table("da_2016_data/da_labour_low_inc.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
DA_pop[ DA_pop == "x" ] <- NA
DA_pop[ DA_pop == "F" ] <- NA
DA_pop[ DA_pop == ".." ] <- NA
DA_pop[ DA_pop == "..." ] <- NA

c <- read.table("out_access_data/iter_comp_access_val/a_all.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
#c <- read.table("out_access_data/comp_access/a_tor.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

#q <- merge(DA_pop,c, by.x = 0, by.y = "dauid")
q <- merge(DA_pop,c, by.x = 0, by.y = 0) # for all


q <- data.frame(q)

q$pop_density_km2 <- as.numeric(as.character(q$pop_density_km2))
q <- subset(q, q$pop_density_km2 > 200)

median(as.numeric(as.character(q$median_household_income_total)), na.rm = TRUE)
qs <- quantile(log(as.numeric(as.character(q$median_household_income_total))/1000),  probs = seq(0, 1, 0.1), na.rm = TRUE)


m <- lm(data = q,formula = q$i2/ max(q$i2) ~ log(as.numeric(as.character(q$median_household_income_total))/1000), weights = as.numeric(as.character(q$private_dwellings_occ)))


ggplot(data = q, aes(x = log(as.numeric(as.character(q$median_household_income_total))/1000),y = q$i2/ max(q$i2))) +
  geom_hline(yintercept = 0, color = 'lightgrey') +
  geom_hline(yintercept = 1, color = 'lightgrey') +
  geom_hline(yintercept = 0.25, color = 'lightgrey') +
  geom_hline(yintercept = 0.5, color = 'lightgrey') +
  geom_hline(yintercept = 0.75, color = 'lightgrey') +
  geom_vline(xintercept = qs[2], color = 'red', size = 2) +
  geom_vline(xintercept = qs[3], color = 'orange', size = 2) +
  geom_vline(xintercept = log(25), color = 'lightgrey') +
  geom_vline(xintercept = log(50), color = 'lightgrey') +
  geom_vline(xintercept = log(100), color = 'lightgrey') +
  #geom_vline(xintercept = log(), color = 'lightgrey') +
  geom_vline(xintercept = log(200), color = 'lightgrey') +
  geom_point(size = 0.2) +
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "white", size = 3, alpha = 0.8) +
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "blue", size = 1.5) +
  scale_y_continuous(name="", limits=c(0, 1),labels = NULL) +
  scale_x_continuous(name="", limits=c(log(20), log(300)),labels = NULL) +
  theme_tufte() + theme(axis.ticks.x=element_blank(),plot.margin=unit(c(0.2,0.2,1.5,2),"cm"))

# # # # # # # # # #

summary(m)

# plots
# Y - percent of jobs accessibile in the region
# X - for a travel time thershold T (or theta!)

library(ggplot2)
library(ggthemes)

setwd("~/Dropbox/work/canada_access")


# read in data for 2 cubes and n jobs
c <- read.table("out_access_data/window_access/a_mtl.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
njobs <- 743615

 #


# read in low inc numbers
DA_pop <- read.table("da_2016_data/da_labour_low_inc.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
DA_pop[ DA_pop == "x" ] <- NA
DA_pop[ DA_pop == "F" ] <- NA
DA_pop[ DA_pop == ".." ] <- NA
DA_pop[ DA_pop == "..." ] <- NA


# join the data !
q <- merge(DA_pop,c, by.x = 0, by.y = 0)

q$pop_density_km2 <- as.numeric(as.character(q$pop_density_km2))
q <- subset(q, q$pop_density_km2 > 100)


q$pop <- NULL
q$pop_density_km2 <- NULL
q$median_household_income_total <- NULL

# line for total workers
q_all <- q
q_all$low_inc_labour_force <- NULL
rownames(q_all) <- q_all$Row.names
q_all$Row.names <- NULL
q_all[is.na(q_all)] <- 0
q_all$labour_force <- as.numeric(as.character(q_all$labour_force))
pop_total_all <- sum(q_all$labour_force)
q_all <- q_all * q_all$labour_force
q_all$labour_force <- NULL
q_all <- (colSums(q_all) / pop_total_all) / njobs # normailze also by total number of jobs

# line for low income workers
q_lii <- q
q_lii$labour_force <- NULL
rownames(q_lii) <- q_lii$Row.names
q_lii$Row.names <- NULL
q_lii[is.na(q_lii)] <- 0
q_lii$low_inc_labour_force <- as.numeric(as.character(q_lii$low_inc_labour_force))
pop_total_all <- sum(q_lii$low_inc_labour_force)
q_lii <- q_lii * q_lii$low_inc_labour_force
q_lii$low_inc_labour_force <- NULL
q_lii <- (colSums(q_lii) / pop_total_all) / njobs # normailze also by total number of jobs

# combine into a data frame
df <- merge(data.frame(q_lii),data.frame(q_all), by.x = 0, by.y = 0)
df$Row.names <- as.numeric(as.character(df$Row.names))
df$mins <- df$Row.names / 60

df$dif <- (df$q_lii - df$q_all) / df$q_all


# plot using ggplot dif curve
p <- ggplot(data = df, aes(x = mins)) +
    #geom_line(aes(y = q_all, color = 'grey')) +
    #geom_line(aes(y = q_all)) +
    #geom_line(aes(y = q_lii, color = 'red')) +
    geom_line(aes(y = dif), size = 2, color = 'blue') +
  theme_tufte() + theme(legend.position="none",plot.margin=unit(c(0.2,0.2,0.55,2),"cm")) +
  theme(panel.grid.major = element_line(size = 0.4, linetype = 'solid',
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                        colour = "lightgrey")) +
  scale_x_continuous(name="", limits=c(10, 90), breaks=seq(10,90,10),labels = NULL) +
  scale_y_continuous(name="", limits=c(-0.5, 1), breaks=seq(-0.5,1,0.5),labels = NULL)
p


# plot 2 curves
p <- ggplot(data = df, aes(x = mins)) +
  #geom_line(aes(y = q_all, color = 'grey')) +
  geom_line(aes(y = q_all),size=2) +
  geom_line(aes(y = q_lii, color = 'red'), size = 2) +
  #geom_line(aes(y = dif)) +
  theme_tufte() + theme(legend.position="none",plot.margin=unit(c(0.2,0.2,0.55,2),"cm")) +
  theme(panel.grid.major = element_line(size = 0.4, linetype = 'solid',
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                        colour = "lightgrey")) +
  scale_x_continuous(name="", limits=c(10, 90), breaks=seq(10,90,10),labels = NULL) +
  scale_y_continuous(name="", breaks=seq(0,1,0.2),labels = NULL)
p

interg <- subset(df,as.numeric(as.character(df$mins)) > 9)
sum(interg$dif) / 81

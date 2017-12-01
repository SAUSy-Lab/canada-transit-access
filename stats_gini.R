# computes the Gini, mean, and plots lorenz curves for accessibility scores

library(ineq)
library(splitstackshape)
library(ggplot2)
library(ggthemes)


c <- read.table("out_access_data/iter_comp_access_val/a_cgy.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)

DA_pop <- read.table("da_2016_data/da_labour_low_inc.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE)
DA_pop[ DA_pop == "x" ] <- NA
DA_pop[ DA_pop == "F" ] <- NA
DA_pop[ DA_pop == ".." ] <- NA
DA_pop[ DA_pop == "..." ] <- NA

da_by_id <- read.csv("da_2016_data/da_uid_data_just.csv", fill = TRUE, header = TRUE )

DAdata <- merge(da_by_id,DA_pop,by.x = "dauid",by.y = 0)

#q <- merge(DAdata,c, by.x = "dauid", by.y = 0) # for all
q <- merge(DAdata,c, by.x = "dauid", by.y = "dauid")

#q$cmauid <- as.character(q$cmauid)
# q <- subset(q, as.character(q$cmauid) == '535' | as.character(q$cmauid) == '537' | as.character(q$cmauid) == '532' )
#q$cduid <- as.character(q$cduid)
#q <- subset(q, as.character(q$cduid) == '3520')


q <- data.frame(q)
q$labour_force <- as.numeric(as.character(q$labour_force))


q$pop_density_km2 <- as.numeric(as.character(q$pop_density_km2))
q <- subset(q, q$pop_density_km2 > 200)

qb <- q[,c("labour_force","i2")]
#qb <- qb[rep(row.names(qb), qb$labour_force), 1:2]
qb$labour_force[is.na(qb$labour_force)] <- 0
qb <- expandRows(qb, "labour_force")

qc <- q[,c("low_inc_labour_force","i2")]
qc$low_inc_labour_force[is.na(qc$low_inc_labour_force)] <- 0
qc <- expandRows(qc, "low_inc_labour_force")


Gini(qb$i2,na.rm = TRUE)
Gini(qb$i2 / 27.34618, na.rm = TRUE)

mean(qb$i2/ 27.34618, na.rm = TRUE)

mean(qc$i2 / 27.34618, na.rm = TRUE)


ggplot() +
  geom_line(aes(Lc(qb$i2)$p,Lc(qb$i2)$L)) +
  geom_line(aes(x = c(0,1), y = c(0,1)), colour = "darkgrey") +
  #geom_ribbon(aes(ymax=x.upper, ymin=x.lower), fill="pink", alpha=.5) +
  theme_tufte() +
  theme(legend.position="none",plot.margin=unit(c(0.2,0.2,2,2),"cm")) +
  theme(panel.grid.major = element_line(size = 0.4, linetype = 'solid',
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                        colour = "lightgrey")) +
  scale_x_continuous(name="", labels = NULL) +
  scale_y_continuous(name="", labels = NULL)

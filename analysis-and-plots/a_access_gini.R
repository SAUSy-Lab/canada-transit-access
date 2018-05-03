
# computes the Gini, CV, mean, and plots lorenz curves for accessibility scores

library(ineq)
library(splitstackshape)
library(ggplot2)
library(ggthemes)



# or read the a_all file
dfa <- read.csv(file = "out_a_all/a_all/a_comp_all.csv")


# labour market data
lab <- read.csv('da_2016_data/da_data_chass.csv')
lab[ lab == "x" ] <- NA
lab[ lab == "F" ] <- NA
lab[ lab == ".." ] <- NA
lab[ lab == "..." ] <- NA
lab_header <- read.csv('da_2016_data/da_data_chass_header.csv')
# compute car mode share
lab$mode_car <- (lab$COL92 + lab$COL93) / lab$COL91
lab$mode_car[is.na(lab$mode_car)] <- 0.7
lab$mode_car[is.infinite(lab$mode_car)] <- 0.7
lab$mode_car[lab$mode_car > 1] <- 1
# compute transit/walk mode share
lab$mode_transit <- 1 - lab$mode_car

# area and cma data
dfca <- read.csv("da_2016_data/da_area_cma_2.csv")

# merge into one data frame
df <- merge(dfa,lab,by.x = "dauid", by.y = "COL0")
df <- merge(df,dfca,by.x = "dauid", by.y = "dauid")


df <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568" | cmauid=="933" | cmauid == "932" | cmauid == "930" | cmauid == "825" | cmauid == "835" | cmauid == "602" | cmauid == "505" | cmauid == "462" | cmauid == "421")


# update rows
df$popdens <- df$COL1 / (df$l_area_km2)
df$unemrate <- df$COL37/df$COL38
max(df$Ai_car)
df$Ai_transit <- df$Ai_transit / max(df$Ai_car)
df$Ai_car <- df$Ai_car / max(df$Ai_car)
df$Ai_car[df$Ai_car < 0.05] <- 0.05
df$Ai_ratio <- (df$Ai_transit / df$Ai_car)
df$Ai_transit[df$Ai_transit < 0.001] <- 0.001



# # subset just by the cma we're curious about
# cmauid=="933" | cmauid == "932" | cmauid == "930"
# cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568"
# dfs <- subset(df, cduid == "3520")
#dfs <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568")
# and remove rural DAs
dfs <- subset(df,df$popdens > 200)
# dfs <- subset(dfs, cduid == "3520")
# dfs <- subset(dfs, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568")
#dfs <- subset(dfs, cduid!='2456')

dfs$COL38[is.na(dfs$COL38)] <- 0
sum(dfs$COL38, na.rm = TRUE)

# for transit
qb <- dfs[,c("COL38","Ai_transit")]
#qb <- qb[rep(row.names(qb), qb$labour_force), 1:2]
qb <- expandRows(qb, "COL38")

Gini(qb$Ai_transit,na.rm = TRUE)
sd(qb$Ai_transit,na.rm = TRUE) / mean(qb$Ai_transit,na.rm = TRUE)
mean(qb$Ai_transit,na.rm = TRUE)



# grab just the fields we want
qb <- dfs[,c("COL38","Ai_car")]
#qb <- qb[rep(row.names(qb), qb$labour_force), 1:2]
qb <- expandRows(qb, "COL38")

Gini(qb$Ai_car,na.rm = TRUE)
sd(qb$Ai_car,na.rm = TRUE) / mean(qb$Ai_car,na.rm = TRUE)
mean(qb$Ai_car,na.rm = TRUE)



p1 <- Lc(dfs$Ai_transit)
p2 <- Lc(dfs$Ai_car)



ggplot() +
  geom_area(aes(x=p2$L,y=p2$L),alpha = 0.05) +
  geom_area(aes(x=p2$p,y=p2$L),alpha = 0.05) +
  geom_area(aes(x=p1$p,y=p1$L),alpha = 0.05) +
  geom_line(aes(x=p1$p,y=p1$L, colour = "transit")) +
  geom_line(aes(x=p2$p,y=p2$L, colour = "auto")) +
  geom_line(aes(x=p2$L,y=p2$L, colour = "line of equality")) +
  scale_colour_manual("",breaks=c("transit","auto","line of equality"),values = c("blue","black","red")) +
  xlab("cumulative share of people") +
  ylab("cumulative share of total access") +
  scale_x_continuous(breaks = seq(0,1,by=0.1), limits = c(0,1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0,1,by=0.1), limits = c(0,1), minor_breaks = NULL) +
  theme_minimal() +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=9))

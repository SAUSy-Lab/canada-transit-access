# tabulates summary statistics (max, mean, min) of access to jobs by urban region


library(dplyr)

# read in access data - as a batch:
filenames=list.files(path="out_a_all/comp_2step/", full.names=TRUE, include.dirs = FALSE)
datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
dfa <- bind_rows(datalist)
write.csv(x = dfa,file = "out_a_all/a_all/comp_2step.csv")

# or read the a_all file
# dfa <- read.csv(file = "out_a_all/fexp/all/a_all.csv")

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
lab$inc_under20 <- lab$COL16 + lab$COL21 + lab$COL22


# area and cma data
dfca <- read.csv("da_2016_data/da_area_cma_2.csv")

# merge into one data frame
df <- merge(dfa,lab,by.x = "dauid", by.y = "COL0")
df <- merge(df,dfca,by.x = "dauid", by.y = "dauid")

df <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568" | cmauid=="933" | cmauid == "932" | cmauid == "930" | cmauid == "825" | cmauid == "835" | cmauid == "602" | cmauid == "505" | cmauid == "462" | cmauid == "421")


df$popdens <- df$COL1 / (df$l_area_km2)
df$unemrate <- df$COL37/df$COL38


max(df$Ai_car)
df$Ai_transit <- df$Ai_transit / max(df$Ai_car)
df$Ai_car <- df$Ai_car / max(df$Ai_car)
df$Ai_car[df$Ai_car < 0.05] <- 0.05
df$Ai_ratio <- (df$Ai_transit / df$Ai_car)
df$Ai_transit[df$Ai_transit < 0.001] <- 0.001


# subset just by the cma we're curious about
# cmauid=="933" | cmauid == "932" | cmauid == "930"
# cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568"
# dfs <- subset(df,df$popdens > 200) # remove pop density < 200/km2
# dfs <- subset(dfs, cduid == "3520")
dfs <- subset(df,df$popdens > 200)

dfs <- subset(dfs, cmauid=="835")

dfs$COL38[is.na(dfs$COL38)] <- 0



weighted.mean(dfs$Ai_transit,dfs$COL38,na.rm = TRUE)
min(dfs$Ai_transit)
max(dfs$Ai_transit)
hist(dfs$Ai_transit)

weighted.mean(dfs$Ai_car,dfs$COL38, rm.na = TRUE)
min(dfs$Ai_car)
max(dfs$Ai_car)
hist(dfs$Ai_car)

weighted.mean(dfs$Ai_ratio,dfs$COL38, rm.na = TRUE)
min(dfs$Ai_ratio)
max(dfs$Ai_ratio)


# hist(dfs$Ai_ratio)
sum(dfs$COL1, na.rm = TRUE)
sum(dfs$COL38, na.rm = TRUE)
sum(dfs$geom_area, na.rm = TRUE)
aggregate(dfs$COL38 /2, list(cmauid=dfs$cmauid), FUN = sum)

# sum(dfs$COL77, na.rm = TRUE)

# access v unem
summary(lm(dfs$Ai_ratio~(dfs$unemrate)))
summary(lm(dfs$mode_transit~(dfs$Ai_transit)))
summary(lm(dfs$mode_transit~(dfs$Ai_ratio)))
summary(lm(dfs$Ai_ratio~(dfs$Ai_transit)))


dfs$Ai_transit_n <- dfs$Ai_transit / (max(dfs$Ai_car))
median(dfs$Ai_transit_n)
hist(dfs$Ai_transit_n)
hist(dfs$Ai_ratio,xlim = c(0,1000),breaks = 200)


mean(dfs$unemrate,na.rm = TRUE)
sum(dfs$COL38, na.rm = TRUE)


#
wtd.quantile(dfs$Ai_transit, weights=dfs$COL38, probs=c(0, .25, .5, .75, 1),
             type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'),
             normwt=FALSE, na.rm=TRUE)

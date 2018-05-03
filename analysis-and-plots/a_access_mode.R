# measures correleation between different types of accessibility measures and transit mode share



library(dplyr)


# read in access data - as a batch:
#filenames=list.files(path="out_a_all/G_e", full.names=TRUE, include.dirs = FALSE)
#datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
#dfa <- bind_rows(datalist)
#write.csv(x = dfa,file = "out_a_all/a_all/a_G_e_all.csv")
dfa <- read.csv("out_a_all/a_all/comp_2step.csv")

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

a_ca <- read.csv("out_a_all/a_all/a_ca_all.csv")
df <- merge(df,a_ca,by.x="dauid",by.y="X")

a_g <- read.csv("out_a_all/a_all/a_G_i_all.csv")
a_g[,1] <- NULL
a_g[,1] <- NULL
colnames(a_g) <- c("dauid","Ai_G_transit","Ai_G_car")
df <- merge(df,a_g,by.x = "dauid",by.y="dauid")

a_g_e <- read.csv("out_a_all/a_all/a_G_e_all.csv")
a_g_e[,1] <- NULL
a_g_e[,1] <- NULL
colnames(a_g_e) <- c("dauid","Ai_G_e_transit","Ai_G_e_car")
df <- merge(df,a_g_e,by.x = "dauid",by.y="dauid")


df$l_area_km2[df$l_area_km2 < 0.01] <- 0.01
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

dfs <- subset(dfs, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568")
cor(dfs$mode_transit,(dfs$Ai_transit))


print("$$$$$$$44")
cor(dfs$mode_transit,(dfs$Ai_transit)^0.8)
cor(dfs$mode_transit,(dfs$Ai_transit))
cor(dfs$mode_transit,(dfs$Ai_G_transit))
cor(dfs$mode_transit,(dfs$Ai_G_e_transit))
cor(dfs$mode_transit,(dfs$X1800))
cor(dfs$mode_transit,(dfs$X2700))
cor(dfs$mode_transit,(dfs$X3600))
cor(dfs$mode_transit,(dfs$X4500))


summary(lm(dfs$mode_transit~(dfs$Ai_transit)+dfs$popdens+dfs$unemrate))
summary(lm(dfs$mode_transit~(dfs$Ai_G_transit)+dfs$popdens+dfs$unemrate))
summary(lm(dfs$mode_transit~(dfs$X1800)))
summary(lm(dfs$mode_transit~(dfs$X2700)))
summary(lm(dfs$mode_transit~(dfs$X3600)))

dfus <- subset(dfs,dfs$unemrate >= 0)
cor(dfus$unemrate,(dfus$Ai_transit))
cor(dfus$unemrate,(dfus$Ai_G_transit))
cor(dfus$unemrate,(dfus$X1800))
cor(dfus$unemrate,(dfus$X2700))
cor(dfus$unemrate,(dfus$X3600))

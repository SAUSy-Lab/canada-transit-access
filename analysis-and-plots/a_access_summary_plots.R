# plots summary stats and distriubtions of access to jobs by urban region


library(dplyr)
library(ggplot2)
library(ggthemes)


# read in access data - as a batch:
#filenames=list.files(path="out_a_all/comp_i_all", full.names=TRUE, include.dirs = FALSE)
#datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
#dfa <- bind_rows(datalist)
dfa <- read.csv("out_a_all/a_all/a_comp_all")

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

dfcma <- read.csv("a_cma_city.csv")

df <- merge(df,dfcma,by.x = "cmauid", by.y = "cmauid")

df <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568" | cmauid=="933" | cmauid == "932" | cmauid == "930" | cmauid == "825" | cmauid == "835" | cmauid == "602" | cmauid == "505" | cmauid == "462" | cmauid == "421")


df$popdens <- df$COL1 / (df$l_area_km2)
df$unemrate <- df$COL37/df$COL38


max(df$Ai_car)
df$Ai_transit <- df$Ai_transit / max(df$Ai_car)
df$Ai_car <- df$Ai_car / max(df$Ai_car)
df$Ai_car[df$Ai_car < 0.05] <- 0.05
df$Ai_ratio <- (df$Ai_transit / df$Ai_car)
df$Ai_transit[df$Ai_transit < 0.01] <- 0.01


# okay, plotting time

df$city_num_random <- 0.75 * (200 - sample(400, size = nrow(df), replace = TRUE))/1000
df$city_num_random <- df$city_num + df$city_num_random

ggplot() +
  geom_point(data=df, aes(x=Ai_transit,y=city_num_random),alpha = as.numeric(df$l_pop) / 10000 ,size = 0.29,colour = "black",stroke = 0) +

  geom_segment(aes(x = 0.094, y = 7.75, xend = 0.094, yend = 8.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.097, y = 6.75, xend = 0.097, yend = 7.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.135, y = 5.75, xend = 0.135, yend = 6.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.081, y = 4.75, xend = 0.081, yend = 5.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.119, y = 3.75, xend = 0.119, yend = 4.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.070, y = 2.75, xend = 0.070, yend = 3.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.104, y = 1.75, xend = 0.104, yend = 2.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.133, y = 0.75, xend = 0.133, yend = 1.25),color = "red", size = 1) +

  scale_x_continuous(breaks = seq(0.1,1,by=0.1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(1,8,by=1), minor_breaks = NULL,labels = c("Winnipeg","Quebec City","Edmonton","Ottawa","Calgary","Vancouver","Montreal","Toronto")) +
  ylab("") +
  xlab("Access by Transit") +
  theme_minimal() +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=9))



ggplot() +
  geom_point(data=df, aes(x=Ai_car,y=city_num_random),alpha = as.numeric(df$l_pop) / 10000 ,size = 0.29,colour = "black",stroke = 0) +

  geom_segment(aes(x = 0.378, y = 7.75, xend = 0.378, yend = 8.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.422, y = 6.75, xend = 0.422, yend = 7.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.384, y = 5.75, xend = 0.384, yend = 6.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.404, y = 4.75, xend = 0.404, yend = 5.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.518, y = 3.75, xend = 0.518, yend = 4.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.402, y = 2.75, xend = 0.402, yend = 3.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.537, y = 1.75, xend = 0.537, yend = 2.25),color = "red", size = 1) +
  geom_segment(aes(x = 0.540, y = 0.75, xend = 0.540, yend = 1.25),color = "red", size = 1) +

  scale_x_continuous(breaks = seq(0,1,by=0.1), limits = c(0.06,1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(1,8,by=1), minor_breaks = NULL,labels = c("Winnipeg","Quebec City","Edmonton","Ottawa","Calgary","Vancouver","Montreal","Toronto")) +
  ylab("") +
  xlab("Access by Car") +
  theme_minimal() +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=9))






#

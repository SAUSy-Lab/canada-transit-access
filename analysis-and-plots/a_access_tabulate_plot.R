
# cross-tabulates populations living in quantiles of transit access

library(ineq)
library(splitstackshape)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(scales)


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


# or read the a_all file
dfa <- read.csv(file = "out_a_all/a_all/a_comp_all.csv")

dfi <- read.csv("da_2016_data/da_income_data.csv")

df <- merge(dfa,dfi,by.x = "dauid",by.y = "dauid")

dfca <- read.csv("da_2016_data/da_area_cma_2.csv")

# merge into one data frame
df <- merge(df,lab,by.x = "dauid", by.y = "COL0")
df <- merge(df,dfca,by.x = "dauid", by.y = "dauid")

dfcma <- read.csv("a_cma_city.csv")

df <- merge(df,dfcma,by.x = "cmauid", by.y = "cmauid")

df <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568" | cmauid=="933" | cmauid == "932" | cmauid == "930" | cmauid == "825" | cmauid == "835" | cmauid == "602" | cmauid == "505" | cmauid == "462" | cmauid == "421")

df$popdens <- df$COL1 / (df$l_area_km2)

max(df$Ai_car)
df$Ai_transit <- df$Ai_transit / max(df$Ai_car)
df$Ai_car <- df$Ai_car / max(df$Ai_car)
df$Ai_car[df$Ai_car < 0.05] <- 0.05
df$Ai_ratio <- (df$Ai_transit / df$Ai_car)
df$Ai_transit[df$Ai_transit < 0.005] <- 0.005


# subset by pop density
dfs <- subset(df,df$popdens > 200)


# unemployment

#dfs <- subset(dfs, cduid!='2456')
dfs <- subset(dfs, city_num == 7)


# deciles

q <- wtd.quantile(dfs$Ai_transit, weights = dfs$COL38, probs=seq(0,1,by=0.1), na.rm = TRUE)
q[1] <- 0
dfs$Ai_transit_deciles <- cut(dfs$Ai_transit, breaks = q,labels = seq(1,10,by=1))

dfq <- aggregate(dfs$COL38, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
dfq

ggplot() +
  geom_bar(aes(dfq$Group.1,as.numeric(dfq$x),fill = dfq$Group.1),stat = "identity",colour = "black") +
  geom_hline(yintercept = sum(dfq$x) / 10, colour = "black") +
  scale_y_continuous(labels = comma) +
  ylab("Unemployed") +
  xlab("Decile of Transit Access") +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_y_continuous(limits = c(0,100000),breaks = c(0,20000,40000,60000,80000,100000)) +
  theme_minimal() + theme(legend.position="none")






## By Transit Decile

# LIM
II_LIM <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- wtd.quantile(dfs$Ai_transit, weights = dfs$COL38, probs=seq(0,1,by=0.1), na.rm = TRUE)
  q[2] <- q[2] + 0.0001
  q[1] <- 0
  dfs$Ai_transit_deciles <- cut(dfs$Ai_transit, breaks = q,labels = seq(1,10,by=1))
  dfq <- aggregate(dfs$II_in_LIM, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_LIM <- c(II_LIM,c(dfq$x[1],dfq$x[1] + dfq$x[2],sum(dfq$x)))
}

# LICO
II_LICO <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- wtd.quantile(dfs$Ai_transit, weights = dfs$COL38, probs=seq(0,1,by=0.1), na.rm = TRUE)
  q[2] <- q[2] + 0.0001
  q[1] <- 0
  dfs$Ai_transit_deciles <- cut(dfs$Ai_transit, breaks = q,labels = seq(1,10,by=1))
  dfq <- aggregate(dfs$II_in_LICO, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_LICO <- c(II_LICO,c(dfq$x[1],dfq$x[1] + dfq$x[2],sum(dfq$x)))
}

# unem
II_unem <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- wtd.quantile(dfs$Ai_transit, weights = dfs$COL38, probs=seq(0,1,by=0.1), na.rm = TRUE)
  q[2] <- q[2] + 0.0001
  q[1] <- 0
  dfs$Ai_transit_deciles <- cut(dfs$Ai_transit, breaks = q,labels = seq(1,10,by=1))
  dfq <- aggregate(dfs$II_c_unem, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_unem <- c(II_unem,c(dfq$x[1],dfq$x[1] + dfq$x[2],sum(dfq$x)))
}

# imig
II_imig <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- wtd.quantile(dfs$Ai_transit, weights = dfs$COL38, probs=seq(0,1,by=0.1), na.rm = TRUE)
  q[2] <- q[2] + 0.0001
  q[1] <- 0
  dfs$Ai_transit_deciles <- cut(dfs$Ai_transit, breaks = q,labels = seq(1,10,by=1))
  dfq <- aggregate(dfs$II_c_imig, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_imig <- c(II_imig,c(dfq$x[1],dfq$x[1] + dfq$x[2], sum(dfq$x)))
}


write.csv(x = cbind(II_LIM,II_LICO,II_unem,II_imig),file = "temp.csv")




## By Transit Auto Ratio

# LIM
II_LIM <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- c(0,0.10,0.20,1)
  dfs$Ai_transit_deciles <- cut(dfs$Ai_ratio, breaks = q,labels = c(1,2,3))
  dfq <- aggregate(dfs$II_in_LIM, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_LIM <- c(II_LIM,c(dfq$x[1],dfq$x[1] + dfq$x[2], sum(dfq$x)))
}

# LICO
II_LICO <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- c(0,0.10,0.20,1)
  dfs$Ai_transit_deciles <- cut(dfs$Ai_ratio, breaks = q,labels = c(1,2,3))
  dfq <- aggregate(dfs$II_in_LICO, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_LICO <- c(II_LICO,c(dfq$x[1],dfq$x[1] + dfq$x[2], sum(dfq$x)))
}

# unem
II_unem <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- c(0,0.10,0.20,1)
  dfs$Ai_transit_deciles <- cut(dfs$Ai_ratio, breaks = q,labels = c(1,2,3))
  dfq <- aggregate(dfs$II_c_unem, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_unem <- c(II_unem,c(dfq$x[1],dfq$x[1] + dfq$x[2], sum(dfq$x)))
}

# rec im
II_imig <- c(1)
dfs <- subset(df,df$popdens > 200)
for (c in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs,dfs$city_num == c)
  q <- c(0,0.10,0.20,1)
  dfs$Ai_transit_deciles <- cut(dfs$Ai_ratio, breaks = q,labels = c(1,2,3))
  dfq <- aggregate(dfs$II_c_imig, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
  II_imig <- c(II_imig,c(dfq$x[1],dfq$x[1] + dfq$x[2], sum(dfq$x)))
}

write.csv(x = cbind(II_LIM,II_LICO,II_unem,II_imig),file = "temp.csv")








# ratio plot

dfs <- subset(df,df$popdens > 200)
q <- c(0,0.05,0.10,0.15,0.20,0.25,0.3,0.35,0.40,0.45,0.5,1)
q[1] <- 0

dfs$Ai_transit_deciles <- cut(dfs$Ai_ratio, breaks = q,labels = c("0.00-0.05","0.05-0.10","0.10-0.15","0.15-0.20","0.20-0.25","0.25-0.30","0.30-0.35","0.35-0.40","0.40-0.45","0.45-0.50","0.50-0.75"))

dfq <- aggregate(dfs$II_in_LIM, by=list(dfs$Ai_transit_deciles), FUN=sum, na.rm = TRUE)
dfq
sum(dfq$x)

ggplot() +
  geom_bar(aes(dfq$Group.1,as.numeric(dfq$x), fill = dfq$Group.1),stat = "identity",colour = "darkgrey", width = 0.7, size = 0.5) +
  #geom_hline(yintercept = sum(dfq$x) / 10, colour = "black") +
  scale_y_continuous(labels = comma) +
  ylab("Individuals in Low-Income Households (LIM)") +
  xlab("= (Transit Access) / (Auto Access)") +
  scale_fill_brewer(palette = "RdYlGn") +
  # scale_y_continuous(limits = c(0,500000),breaks = c(0,100000,200000,300000,400000,500000)) +
  theme_minimal()  + theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))

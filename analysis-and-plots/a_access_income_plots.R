# plots correlations between transit access and income


library(dplyr)
library(ggplot2)
library(ggthemes)


# read in access data - as a batch:
#filenames=list.files(path="out_a_all/finv01con171513", full.names=TRUE, include.dirs = FALSE)
#datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
#dfa <- bind_rows(datalist)
#write.csv(x = dfa,file = "a_finv01con171513_all.csv")
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

pov <- read.csv('da_2016_data/da_data_chass_poverty.csv')
pov[ pov == "x" ] <- NA
pov[ pov == "F" ] <- NA
pov[ pov == ".." ] <- NA
pov[ pov == "..." ] <- NA


# area and cma data
dfca <- read.csv("da_2016_data/da_area_cma_2.csv")

# merge into one data frame
df <- merge(dfa,lab,by.x = "dauid", by.y = "COL0")
df <- merge(df,dfca,by.x = "dauid", by.y = "dauid")
df <- merge(df,pov,by.x = "dauid", by.y = "dauid")


# update rows
df$popdens <- df$COL1 / (df$l_area_km2)
df$unemrate <- df$COL37/df$COL38
max(df$Ai_car)
df$Ai_transit <- df$Ai_transit / max(df$Ai_car)
df$Ai_car <- df$Ai_car / max(df$Ai_car)
df$Ai_ratio <- (df$Ai_transit / df$Ai_car)
df$Ai_transit[df$Ai_transit < 0.001] <- 0.001
df$P_perc_imm_11_16 <- df$P_Imm_2011_2016 / df$P_Total_pop_I
df$P_perc_housing_30more <- df$P_spend_more_30per_housing / df$P_housing_w_inc
df$P_perc_vis_min <- df$P_vis_min / df$P_total_pop_V

# # subset just by the cma we're curious about
# cmauid=="933" | cmauid == "932" | cmauid == "930"
# cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568"
# dfs <- subset(df, cduid == "3520")
#dfs <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568")
# and remove rural DAs
dfs <- subset(df,df$popdens > 200)
dfs <- subset(dfs, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568")
dfs <- subset(dfs, cduid!='2456')
dfs <- subset(dfs, cmauid!='543')


cor.test(dfs$Ai_transit,dfs$P_pre_LIMAT,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_pre_LICOAT,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_med_AT_h_income,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_unem_rate,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_perc_housing_30more,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_perc_imm_11_16,use="complete.obs")

cor.test(dfs$Ai_transit,dfs$mode_transit,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_average_num_weeks_worked,use="complete.obs")
cor.test(dfs$Ai_transit,dfs$P_perc_vis_min,use="complete.obs")

summary(lm(dfs$Ai_transit~dfs$P_med_AT_h_income))

cor.test(dfs$Ai_ratio,dfs$P_pre_LIMAT,use="complete.obs")
cor.test(dfs$Ai_ratio,dfs$P_pre_LICOAT,use="complete.obs")
cor.test(dfs$Ai_ratio,dfs$P_med_AT_h_income,use="complete.obs")
cor.test(dfs$Ai_ratio,dfs$P_unem_rate,use="complete.obs")
cor.test(dfs$Ai_ratio,dfs$P_perc_housing_30more,use="complete.obs")
cor.test(dfs$Ai_ratio,dfs$P_perc_imm_11_16,use="complete.obs")





m <- lm(data = dfs,formula = dfs$Ai_transit ~ log(as.numeric(as.character(dfs$P_med_AT_h_income))/1000), weights = as.numeric(as.character(dfs$COL38)))





png(filename="~/Dropbox/work/MA_Thesis/analysis_2/plots/aiT_hinc_all.png",)

ggplot(data = dfs, aes(x = log(as.numeric(as.character(dfs$P_med_AT_h_income))/1000),y = dfs$Ai_transit)) +
  geom_point(size = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 0, color = 'grey') +
  geom_hline(yintercept = 0.2, color = 'red') +
  geom_hline(yintercept = 0.4, color = 'grey') +
  geom_hline(yintercept = 0.6, color = 'grey') +
  geom_hline(yintercept = 0.8, color = 'grey') +
  #geom_vline(xintercept = qs[2], color = 'red', size = 2) +
  #geom_vline(xintercept = qs[3], color = 'orange', size = 2) +
  geom_vline(xintercept = log(25), color = 'grey') +
  geom_vline(xintercept = log(50), color = 'grey') +
  geom_vline(xintercept = log(100), color = 'grey') +
  #geom_vline(xintercept = log(), color = 'lightgrey') +
  geom_vline(xintercept = log(200), color = 'grey') +

  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "white", size = 2, alpha = 0.8) +
  geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "blue", size = 1) +
  scale_y_continuous(name="", limits=c(0, 0.7),labels = NULL) +
  scale_x_continuous(name="", limits=c(log(20), log(300)),labels = NULL) +
  theme_tufte() + theme(axis.ticks.x=element_blank(),plot.margin=unit(c(0.2,0.2,1.5,2),"cm"))

dev.off()
#
# #
#

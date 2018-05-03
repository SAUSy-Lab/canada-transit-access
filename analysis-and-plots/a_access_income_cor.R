
# measures correlation between transit access and various categories of income levels

library(ineq)
library(splitstackshape)
library(ggplot2)
library(ggthemes)

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
dfa <- read.csv(file = "out_a_all/a_all/c_20_40.csv")
dfa[mapply(is.infinite, dfa)] <- 0.001


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
df$Ai_car[df$Ai_car < 0.02] <- 0.02
df$Ai_ratio <- (df$Ai_transit / df$Ai_car)
df$Ai_transit[df$Ai_transit < 0.002] <- 0.002





df$II_mu <- (0.25 * scale(df$II_MHI) - 0.25 * scale(df$II_P_LICO) - 0.25 * scale(df$II_P_LIM) - 0.25 * scale(df$II_p_unem))



# dfs <- subset(df, cmauid=="532" | cmauid == "535" | cmauid == "537" | cmauid == "539" | cmauid == "541" | cmauid == "550" | cmauid == "568")
# dfs <- subset(dfs, cmauid!='543')

for (cn in seq(8,1,by=-1)) {
  dfs <- subset(df,df$popdens > 200)
  dfs <- subset(dfs, cduid!='2456')
  dfs <- subset(dfs, city_num == cn)

  corrow <- c(cn,
              cor(dfs$Ai_transit,dfs$II_P_LICO, use="pairwise.complete.obs"),
              cor(dfs$Ai_transit,dfs$II_P_LIM, use="pairwise.complete.obs"),
              cor(dfs$Ai_transit,dfs$II_p_unem, use="pairwise.complete.obs"),
              cor(dfs$Ai_transit,log(as.numeric(as.character(dfs$II_MHI))/1000), use="pairwise.complete.obs"),
              cor(dfs$Ai_transit,dfs$II_mu, use="pairwise.complete.obs")
  )
  print(corrow)
}
dfs <- subset(df,df$popdens > 200)
dfs <- subset(dfs, cduid!='2456')
#dfs <- subset(dfs, city_num == cn)

corrow <- c(cn,
            cor(dfs$Ai_transit,dfs$II_P_LICO, use="pairwise.complete.obs"),
            cor(dfs$Ai_transit,dfs$II_P_LIM, use="pairwise.complete.obs"),
            cor(dfs$Ai_transit,dfs$II_p_unem, use="pairwise.complete.obs"),
            cor(dfs$Ai_transit,log(as.numeric(as.character(dfs$II_MHI))/1000), use="pairwise.complete.obs"),
            cor(dfs$Ai_transit,dfs$II_mu, use="pairwise.complete.obs")
)
print(corrow)

# cor(dfs$II_p_unem,log(as.numeric(as.character(dfs$II_MHI))/1000), use="pairwise.complete.obs")

# # # # # #





# # income
#
# m <- lm(data = df,formula = df$Ai_transit ~ log(as.numeric(as.character(df$II_MHI))/1000))
# summary(m)
# r <- cor(df$Ai_transit,log(as.numeric(as.character(df$II_MHI))/1000), use="pairwise.complete.obs")
#
# ggplot(data = df, aes(x = (as.numeric(as.character(df$II_MHI))/1000),y = df$Ai_transit)) +
#
#   # #geom_vline(xintercept = qs[2], color = 'red', size = 2) +
#   # #geom_vline(xintercept = qs[3], color = 'orange', size = 2) +
#
#   geom_point(size = 0.1, alpha = 0.3) +
#   geom_smooth(method='lm', colour = "red") +
#   geom_smooth(method='loess') +
#   #geom_text(aes(x = 200, y = 0.6, label = paste("r =",round(r,digits = 2))),size = 4) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "white", size = 2, alpha = 0.8) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "blue", size = 1) +
#   scale_y_continuous(limits=c(0.01, 0.65), breaks = c(0.1,0.2,0.3,0.4,0.5,0.6)) +
#   scale_x_log10(breaks = c(25,50,100,200,400),limits = c(15,500)) +
#   xlab("Median Household Income ($1000s)") +
#   ylab("Transit Access to Jobs") +
#   theme_minimal() +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=9))
#
# # # # # # # # # # #
#
# # unem
#
# m <- lm(data = df,formula = df$Ai_transit ~ (as.numeric(as.character(df$II_p_unem))))
# summary(m)
#
# ggplot(data = df, aes(x = (as.numeric(as.character(df$II_p_unem))),y = df$Ai_transit)) +
#
#   # #geom_vline(xintercept = qs[2], color = 'red', size = 2) +
#   # #geom_vline(xintercept = qs[3], color = 'orange', size = 2) +
#   geom_hline(yintercept = mean(df$Ai_transit), colour = "red") +
#   geom_vline(xintercept = mean(df$II_p_unem,na.rm = TRUE), colour = "red") +
#
#   geom_point(size = 0.2, alpha = 0.5) +
#   geom_smooth(method='loess',span = 0.2) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "white", size = 2, alpha = 0.8) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "blue", size = 1) +
#   scale_y_continuous(limits=c(0.01, 0.65), breaks = c(0.1,0.2,0.3,0.4,0.5,0.6)) +
#   scale_x_continuous(breaks = c(5,10,15,20,25,30),limits = c(0,33))+
#   #(name="", limits=c(log(20), log(300)),labels = NULL) +
#   xlab("Unemployment Rate (%)") +
#   ylab("Transit Access to Jobs") +
#   theme_minimal()
#
# # LICO
#
# m <- lm(data = df,formula = df$Ai_transit ~ (as.numeric(as.character(df$II_P_LICO))))
# summary(m)
# r <- cor(df$Ai_transit,(as.numeric(as.character(df$II_P_LICO))), use="pairwise.complete.obs")
# r
#
# ggplot(data = df, aes(x = (as.numeric(as.character(df$II_P_LICO))),y = df$Ai_transit)) +
#
#   # #geom_vline(xintercept = qs[2], color = 'red', size = 2) +
#   # #geom_vline(xintercept = qs[3], color = 'orange', size = 2) +
#
#   geom_point(size = 0.2, alpha = 0.5) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "white", size = 2, alpha = 0.8) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "blue", size = 1) +
#   scale_y_continuous(limits=c(0.01, 0.65), breaks = c(0.1,0.2,0.3,0.4,0.5,0.6)) +
#   scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,55,60),limits = c(0,60))+
#   #(name="", limits=c(log(20), log(300)),labels = NULL) +
#   xlab("LICO (%)") +
#   ylab("Transit Access to Jobs") +
#   theme_minimal()
#
#
#
# # LIM
#
# m <- lm(data = df,formula = df$Ai_transit ~ (as.numeric(as.character(df$II_P_LIM))))
# summary(m)
#
# ggplot(data = df, aes(x = (as.numeric(as.character(df$II_P_LIM))),y = df$Ai_transit)) +
#
#   # #geom_vline(xintercept = qs[2], color = 'red', size = 2) +
#   # #geom_vline(xintercept = qs[3], color = 'orange', size = 2) +
#
#   geom_point(size = 0.2, alpha = 0.5) +
#   #geom_smooth(method='loess',span=0.2) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "white", size = 2, alpha = 0.8) +
#   #geom_abline(slope = m$coefficients[2], intercept = m$coefficients[1], color = "blue", size = 1) +
#   scale_y_continuous(limits=c(0.01, 0.65), breaks = c(0.1,0.2,0.3,0.4,0.5,0.6)) +
#   scale_x_continuous(breaks = c(10,20,30,40,50,60,70),limits = c(0,70))+
#   #(name="", limits=c(log(20), log(300)),labels = NULL) +
#   xlab("% in Low Income Households") +
#   ylab("Transit Access to Jobs") +
#   theme_minimal()

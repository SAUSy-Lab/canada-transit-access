# cluster analysis of results for Transport Policy paper

library(ggplot2)
library(ggthemes)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(questionr)


setwd("~")

# access data
df <- read.csv("out_a_all/all/can_acc_all.csv")

# income / poverty data
dfi <- read.csv("da_2016_data/da_income_data.csv")

# built env data
dfb <- read.csv("da_2016_data/da_built_env.csv")

# mobility data
dfmob <- read.csv("da_2016_data/da_movers.csv")

# merge
df <- merge(df,dfi,by.x = "dauid",by.y="dauid")
df <- merge(df,dfb,by.x = "dauid",by.y="COL0")
df <- merge(df,dfmob,by.x = "dauid", by.y = "dauid")
dfcma <- read.csv("a_cma_city.csv")
df <- merge(df,dfcma,by.x = "cmauid", by.y = "cmauid")

df$h_detached <- df$COL12 / df$COL3
df$h_app5up <- df$COL13 / df$COL3
df$h_all5dwn <- df$COL17 / df$COL3
df$h_attched <- (df$COL14 + df$COL15 + df$COL16 + df$COL18) / df$COL3
df$h_app <- df$h_app5up + df$h_all5dwn

df$b_pre1960 <- df$COL5 / df$COL4
df$b_19601980 <- df$COL6 / df$COL4
df$b_19802000 <- (df$COL7 + df$COL8) / df$COL4
df$b_since2000 <- (df$COL9 + df$COL10 + df$COL11) / df$COL4

df$pd_group[df$pop_density < 2000] <- 'a) less than 2,000'
df$pd_group[df$pop_density >= 2000 & df$pop_density < 4000] <- 'b) 2,000 to 4,000'
df$pd_group[df$pop_density >= 4000 & df$pop_density < 6000] <- 'c) 4,000 to 6,000'
df$pd_group[df$pop_density >= 6000] <- 'd) over 6,000'
table(df$pd_group)

df$tc_group[df$comp_car < 0.2] <- 'a) less than 0.2'
df$tc_group[df$comp_car >= 0.2 & df$comp_car < 0.4] <- 'b) 0.2 to 0.4'
df$tc_group[df$comp_car >= 0.4 & df$comp_car < 0.6] <- 'c) 0.4 to 0.6'
df$tc_group[df$comp_car >= 0.6] <- 'd) over 0.6'
table(df$tc_group)

df$mov_p_nonmovers <-  df$m_nonmovers / df$m_all
df$mov_p_incsd <-  df$m_nonmigrants / df$m_all
df$mov_p_incan <-  df$m_internal / df$m_all
df$mov_p_inextern <-  df$m_external / df$m_all


df <- subset(df, df$pop_density > 200)



# subset to just include those DAs at risk of TP
df$poverty <- df$II_in_LICO + df$II_c_imig + df$II_c_unem
dfst <- subset(df, df$comp_transit < 0.1) # less than 0.1 transit access to emp
dfsi <- subset(dfst, dfst$II_in_LICO > 100) # more than 100 at risk people and shitty transit access



all <- "All Dissemination Areas (DA)\n\n"
tlow <- "DAs with low transit access\n\n"
tpov <- "DAs with low transit access \nand high amounts of poverty\n"





df$random15 <- sample(1:5, size = nrow(df), replace = TRUE)
df$II_in_LICO_j <- df$II_in_LICO - 3 + df$random15 
df$II_in_LICO_j[df$II_in_LICO_j < 0] <- 1

df$random02 <- runif(n = nrow(df),min = 0.001,max = 0.01)
df$comp_transit_2 <- df$comp_transit
df$comp_transit_2[df$comp_transit_2 < 0.01] <- df$random02


df$tpov1 <- -0.5*df$comp_transit_2 + 0.0005*df$II_in_LICO_j + 0.5 

quantile(x = df$tpov1,probs = c(0.25,0.5,0.75),na.rm = T)


tpoverty = -0.5*access + 0.0005*LICO + 0.5 
-0.5*(0.5) + 0.0005*(500) + 0.5 
-0.5*ya  = (0.525 - 0.5 - 0.0005*(500)) / -0.5
-0.5*ya + 0.0005*yl + 0.5 = 0.475


df$topv1cat <- 'd) Low'
df$topv1cat[df$tpov1 >= 0.475] <- 'c) Moderate'
df$topv1cat[df$tpov1 >= 0.5] <- 'b) High'
df$topv1cat[df$tpov1 >= 0.525] <- 'a) Very High'

#df$topv1cat <- 'd) Low'
#df$topv1cat[df$tpov1 >= 0.4632674] <- 'c) Moderate'
#df$topv1cat[df$tpov1 >= 0.4905298] <- 'b) High'
#df$topv1cat[df$tpov1 >= 0.5079535] <- 'a) Very High'


# plot with dots and colour

ggplot() +
  geom_polygon(aes(x = c(0,0.15,0),y=c(50,200,200)),fill = '#d7191c', alpha = 0.3)+
  geom_polygon(aes(x = c(0,0.15,0.2,0),y=c(50,200,200,0)),fill = '#fdae61', alpha = 0.3)+
  geom_polygon(aes(x = c(0,0.2,0.2,0.05),y=c(0,200,150,0)),fill = '#abdda4', alpha = 0.3)+
  geom_polygon(aes(x = c(0.05,0.2,0.2),y=c(0,150,0)),fill = '#2b83ba', alpha = 0.3)+
  geom_point(aes(df$comp_transit_2,df$II_in_LICO_j), alpha = 0.5, size = 0.05) +
  scale_x_continuous(limits = c(0,0.2)) + 
  scale_y_continuous(limits = c(0,200)) + 
  xlab("Transit Access") + ylab("People under the LICO") + 
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=8))


ggplot() +
  geom_polygon(aes(x = c(0,0.45,0),y=c(50,500,500)),fill = '#d7191c', alpha = 0.3)+
  geom_polygon(aes(x = c(0,0.45,0.5,0),y=c(50,500,500,0)),fill = '#fdae61', alpha = 0.3)+
  geom_polygon(aes(x = c(0,0.5,0.5,0.05),y=c(0,500,450,0)),fill = '#abdda4', alpha = 0.3)+
  geom_polygon(aes(x = c(0.05,0.5,0.5),y=c(0,450,0)),fill = '#2b83ba', alpha = 0.3)+
  geom_point(aes(df$comp_transit_2,df$II_in_LICO_j), alpha = 0.5, size = 0.2) +
  scale_x_continuous(limits = c(0,0.5)) + 
  scale_y_continuous(limits = c(0,500)) + 
  xlab("Transit Access") + ylab("People under the LICO") + 
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=10)) 


ggplot() +
  #geom_polygon(aes(x = c(0,0.15,0),y=c(50,200,200)),fill = '#d7191c', alpha = 0.3)+
  #geom_polygon(aes(x = c(0,0.15,0.2,0),y=c(50,200,200,0)),fill = '#fdae61', alpha = 0.3)+
  #geom_polygon(aes(x = c(0,0.2,0.2,0.05),y=c(0,200,150,0)),fill = '#abdda4', alpha = 0.3)+
  #geom_polygon(aes(x = c(0.05,0.2,0.2),y=c(0,150,0)),fill = '#2b83ba', alpha = 0.3)+
  #geom_point(aes(df$comp_transit_2,df$II_in_LICO_j), alpha = 0.5, size = 0.2) +
  geom_point(aes(x = df$comp_transit_2,y = df$II_in_LICO_j, colour = as.factor(df$topv1cat)), size = 0.05, alpha = 0.5) +
  #geom_vline(xintercept = 0.103, linetype = 2) +
  #geom_hline(yintercept = 81.7, linetype = 2) +
  #geom_vline(xintercept = 0.077, linetype = 2) +
  #geom_hline(yintercept = 52, linetype = 2) +
  #scale_x_continuous(limits = c(0,0.3)) + 
  #scale_y_continuous(limits = c(0,300)) + 
  scale_color_manual(values = c('#d7191c','#fdae61','#abdda4','#2b83ba'),guide=FALSE) + 
  scale_x_log10(breaks = c(0,0.01,0.02,0.05,0.1,0.2,0.5),limits = c(0.003,0.7),minor_breaks=F) + 
  scale_y_log10(breaks = c(0,10,50,100,200,500,1000),minor_breaks=F) + 
  xlab("Transit Access") + ylab("People under the LICO") + 
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=8))



ggplot() +
  geom_point(aes(x = df$comp_transit,y = df$II_in_LICO_j, colour = as.factor(df$topv1cat)), size = 0.5) +
  scale_x_continuous(limits = c(0,0.3)) + scale_y_continuous(limits = c(0,300))


# plane surface plots


set.seed(1)
x <- seq(0, 1, length= 50)
y <- seq(0, 1000, length= 50)
f <- function(x,y){ z <- -0.5*x + 0.0005*y + 0.5 }
z <- outer(x,y,f)
persp(x, y, z, theta = -28, phi = 20, expand = 0.66, col = "white",zlim = c(0,1), xlab = "", ylab = "", zlab = "", border = NA, axes = TRUE, shade = 0.15, r = 8, d = 1)
par(new=T)
persp(x, y, z, theta = -28, phi = 20, expand = 0.66, col = NA,zlim = c(0,1), xlab = "Transit access", ylab = "Count under the LICO", zlab = "Risk of transport poverty", border = "#ff6666", axes = TRUE, shade = z, r = 8, d = 1)


x <- seq(0, 10, length= 50)
y <- seq(0, 10, length= 50)
f <- function(x,y){ z <- -x^2 - y^2}
z <- outer(x,y,f)
persp(x, y, z, theta = -228, phi = 25, expand = 0.75, col = "white", xlab = "Transit Access", ylab = "SES", zlab = "Risk of Transport Poverty", border = 'green', axes = TRUE, shade = z, r = 8, d = 1)



#

# cluster anlaysis

# pop dens, const period, dwelling type, auto access

dfcluster <- subset(df, df$tpov1 > 0.5)
dfcluster <- dfcluster[,c("comp_car","comp_transit","pop_density","II_in_LICO","h_app","b_since2000","mov_p_nonmovers")]
dfcluster <- subset(dfcluster, dfcluster$pop_density < 20000)

dfcluster <- na.omit(dfcluster) # listwise deletion of missing
dfcluster <- scale(dfcluster) # standardize variables 

# Determine number of clusters
wss <- (nrow(dfcluster)-1)*sum(apply(dfcluster,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dfcluster,
                                     centers=i)$withinss)

ggplot() + geom_point(aes(1:15, wss)) + geom_line(aes(1:15, wss)) + xlab("number of clusters") + ylab("within groups sum of squares") + 
  scale_x_continuous(breaks = 1:15) +
  theme_minimal()  +
  theme(text = element_text(size=10))



# K-Means Cluster Analysis
fit <- kmeans(dfcluster, 2) # n clusters


# join to data
dfclustero <- subset(df, df$tpov1 > 0.5)
dfclustero <- dfclustero[,c("dauid","comp_car","comp_transit","pop_density","II_in_LICO","h_app","b_since2000","mov_p_nonmovers","population","topv1cat")]
dfclustero <- subset(dfclustero, dfclustero$pop_density < 20000)

dfclustero <- na.omit(dfclustero) # listwise deletion of missing

dfclustero <- data.frame(dfclustero, fit$cluster) 


aggregate(dfclustero,by=list(fit$cluster),FUN=mean)
table(fit$cluster)


ggplot() + geom_point(aes(x = dfcluster$comp_ratio,y = dfcluster$pop_density,color = as.character(dfcluster$fit.cluster)))


aggregate(dfcluster$population,by=list(dfcluster$fit.cluster),FUN=sum)


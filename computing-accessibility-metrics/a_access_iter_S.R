# computing a floating catchment measure of access

setwd("~/Dropbox/work/MA_Thesis/analysis_2")

# input folders
in_city <- 'wpg'
schedule_dir <- paste('matrix_da16_ct11/t_',in_city,'_79',sep = '')
car_matrix_name <- paste('matrix_da16_ct11/drive/c_',in_city,'.csv',sep = '')
out_file_name <- paste('out_a_all/a_',in_city,'.csv',sep = '')

# setup the car matrix - adding 10 min for parking / traffic
in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
in_car_matrix <- 1.4 * (in_car_matrix) + 120
in_car_matrix$id <- ((in_car_matrix$id) - 120) / 1.4


# functions
#ftij <- function(matrix_in) {
#  1 / (1 + exp((matrix_in / 15)-3))
#}
ftij <- function(matrix_in) {
  (180*(90 + matrix_in)^-1)-1
}


# employment data
emp <- read.csv('ct_2016_jobs_data/ct_16_jobs_in_11_ct.csv',colClasses=c("tct"="character"))
emp[ emp == "x" ] <- NA
emp$cmauid <- substr(emp$tct,1,3)
emp <- subset(emp, cmauid != "543")

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


# overall labour force col
# "COL38","Labour - Total Sex / Total - Population aged 15 years and over by Labour force status - 25% sample data / In the labour force" (employed and unemployed combined)
lab_col <- "COL38"

# initial job_type
emp_col <- "NOC_total"




# initial function for access to jobs - normalized by Lj
Ai_wLj <- function(transit_matrix, jobtype, Lj_in) {
  L_col_name <- colnames(Lj_in)
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix[access_matrix > 5400] <- NA
  access_matrix <-ftij(access_matrix / 60) * ftij(access_matrix / 60) # the gravity function
  access_matrix <- t(access_matrix)
  
  emp_t <- subset(emp, select=c("tct", jobtype)) # change this line for job types !!!!
  emp_t <- merge(Lj_in, emp_t, by.x = 0, by.y = "tct")
  emp_t$ratio <- emp_t[,jobtype] / emp_t[,L_col_name]
  emp_t[,L_col_name] <- NULL
  emp_t[,jobtype] <- NULL
  access_matrix <- merge(emp_t,access_matrix,by.x="Row.names",by.y=0)
  # print(sum(access_matrix[,jobtype]),na.rm = TRUE) # for checking job counts (should match table)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- access_matrix * access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- t(access_matrix)
  access_matrix <- as.data.frame(rowSums(access_matrix, na.rm = TRUE))
  colnames(access_matrix) <- c(jobtype)
  return(access_matrix)
}




# Lj_noAi_wMode
Lj_noAi_wMode <- function(in_matrix, poptype, lab_mode) {
  access_matrix <- in_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix[access_matrix > 5400] <- NA
  access_matrix <- ftij(access_matrix / 60)
  lab_in <- lab[,c("COL0",lab_col,lab_mode)]
  lab_in[is.na(lab_in)] <- 0
  lab_in$ratio <- lab_in[,lab_col] * lab_in[,lab_mode]
  lab_in[,lab_col] <- NULL
  lab_in[,lab_mode] <- NULL
  access_matrix <- merge(lab_in,access_matrix,by.x="COL0",by.y=0)
  # print(sum(access_matrix[,lab_col]),na.rm = TRUE) # for checking total sum of pop
  rownames(access_matrix) <- access_matrix$COL0
  access_matrix[1] <- NULL
  access_matrix <- access_matrix[,"ratio"] * access_matrix
  access_matrix[,"ratio"] <- NULL
  q <- as.data.frame(colSums(access_matrix, na.rm = TRUE))
  colnames(q) <- lab_col
  return(q)
}

# Lj_wAi_wMode
Lj_wAi_wMode <- function(in_matrix, poptype, lab_mode, Ai_in) {
  access_matrix <- in_car_matrix
  A_col_name <- colnames(Ai_in)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix[access_matrix > 5400] <- NA
  access_matrix <- ftij(access_matrix / 60)
  lab_in <- lab[,c("COL0",lab_col,lab_mode)]
  lab_in <- merge(lab_in,Ai_in,by.x = "COL0", by.y = 0)
  lab_in[is.na(lab_in)] <- 0
  lab_in$ratio <- lab_in[,lab_col] * lab_in[,lab_mode] / lab_in[,A_col_name]
  lab_in[,lab_col] <- NULL
  lab_in[,lab_mode] <- NULL
  lab_in[,A_col_name] <- NULL
  access_matrix <- merge(lab_in,access_matrix,by.x="COL0",by.y=0)
  # print(sum(access_matrix[,lab_col]),na.rm = TRUE) # for checking total sum of pop
  rownames(access_matrix) <- access_matrix$COL0
  access_matrix[1] <- NULL
  access_matrix <- access_matrix[,"ratio"] * access_matrix
  access_matrix[,lab_col] <- NULL
  access_matrix[,"ratio"] <- NULL
  q <- as.data.frame(colSums(access_matrix, na.rm = TRUE))
  colnames(q) <- lab_col
  return(q)
}





######




# initial driving Lj no Ai
Lj_initial_car_mode <- Lj_noAi_wMode(in_car_matrix, "COL38", "mode_car")

# initial transit Lj Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_noAi_wMode(in_transit_matrix, "COL38", "mode_transit")
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Lj_initial_transit_mode <- Reduce("+", access_G_list) / length(access_G_list)
# combine
Lj_initial_mode <- Lj_initial_transit_mode + Lj_initial_car_mode




######


# Ai transit initial with Lj
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Ai_wLj(in_transit_matrix, emp_col,Lj_initial_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_initial_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_initial_transit[Ai_initial_transit < 0.001] <- 0.001


# driving Ai with Lj
Ai_initial_car <- Ai_wLj(in_car_matrix, emp_col, Lj_initial_mode)
Ai_initial_car[Ai_initial_car < 0.001] <- 0.001



# compute mean Ai
lab_pop_mode <- lab[,c("COL0","COL38", "mode_transit", "mode_car")]
lab_pop_mode$COL38[is.na(lab_pop_mode$COL38)] <- 0
Abar_o <- merge(Ai_initial_car,lab_pop_mode,by.x=0,by.y="COL0")
rownames(Abar_o) <- Abar_o[,1]
Abar_o[,1] <- NULL
Abar_o$a_car <- Abar_o$NOC_total * Abar_o$mode_car * Abar_o$COL38
Abar_o$NOC_total <- NULL
Abar_o <- merge(Abar_o,Ai_initial_transit,by.x=0,by.y=0)
rownames(Abar_o) <- Abar_o[,1]
Abar_o[,1] <- NULL
Abar_o$a_transit <- Abar_o$NOC_total * Abar_o$mode_transit * Abar_o$COL38
Abar_o$NOC_total <- NULL
Abar_o <- (sum(Abar_o$a_car) + sum(Abar_o$mode_transit)) / sum(Abar_o$COL38)
Abar_o








######

# driving Lj 1 with Ai
Lj_1_car_mode <- Lj_wAi_wMode(in_car_matrix, "COL38", "mode_car", Ai_initial_car)
# Lj 1 transit with Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_wAi_wMode(in_transit_matrix, "COL38", "mode_transit", Ai_initial_transit)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Lj_1_transit_mode <- Reduce("+", access_G_list) / length(access_G_list)
# combine
Lj_1_mode <- Lj_1_transit_mode + Lj_1_car_mode


########


# Ai transit i 1
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Ai_wLj(in_transit_matrix, emp_col,Lj_1_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_1_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_1_transit[Ai_1_transit < 0.001] <- 0.001
# driving Ai with Lj
Ai_1_car <- Ai_wLj(in_car_matrix, emp_col, Lj_1_mode)
Ai_1_car[Ai_1_car < 0.001] <- 0.001



# compute mean Ai
lab_pop_mode <- lab[,c("COL0","COL38", "mode_transit", "mode_car")]
lab_pop_mode$COL38[is.na(lab_pop_mode$COL38)] <- 0
Abar_c <- merge(Ai_1_car,lab_pop_mode,by.x=0,by.y="COL0")
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_car <- Abar_c$NOC_total * Abar_c$mode_car * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- merge(Abar_c,Ai_1_transit,by.x=0,by.y=0)
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_transit <- Abar_c$NOC_total * Abar_c$mode_transit * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- (sum(Abar_c$a_car) + sum(Abar_c$mode_transit)) / sum(Abar_c$COL38)
Abar_c


# update Ai
Ai_1_transit_c1 <- (Abar_o / Abar_c) * Ai_1_transit 
Ai_1_car_c1 <- (Abar_o / Abar_c) * Ai_1_car






# driving Lj 2 with Ai
Lj_2_car_mode <- Lj_wAi_wMode(in_car_matrix, "COL38", "mode_car", Ai_1_car_c1)
# Lj 1 transit with Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_wAi_wMode(in_transit_matrix, "COL38", "mode_transit", Ai_1_transit_c1)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Lj_2_transit_mode <- Reduce("+", access_G_list) / length(access_G_list)
# combine
Lj_2_mode <- Lj_2_transit_mode + Lj_2_car_mode


########


# Ai transit i 2
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Ai_wLj(in_transit_matrix, emp_col,Lj_2_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_2_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_2_transit[Ai_2_transit < 0.001] <- 0.001

# driving Ai with Lj
Ai_2_car <- Ai_wLj(in_car_matrix, emp_col, Lj_2_mode)
Ai_2_car[Ai_2_car < 0.001] <- 0.001


# compute mean Ai
lab_pop_mode <- lab[,c("COL0","COL38", "mode_transit", "mode_car")]
lab_pop_mode$COL38[is.na(lab_pop_mode$COL38)] <- 0
Abar_c <- merge(Ai_2_car,lab_pop_mode,by.x=0,by.y="COL0")
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_car <- Abar_c$NOC_total * Abar_c$mode_car * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- merge(Abar_c,Ai_2_transit,by.x=0,by.y=0)
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_transit <- Abar_c$NOC_total * Abar_c$mode_transit * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- (sum(Abar_c$a_car) + sum(Abar_c$mode_transit)) / sum(Abar_c$COL38)
Abar_c


# update Ai
Ai_2_transit_c2 <- (Abar_o / Abar_c) * Ai_2_transit 
Ai_2_car_c2 <- (Abar_o / Abar_c) * Ai_2_car





# # #

# driving Lj 3 with Ai
Lj_3_car_mode <- Lj_wAi_wMode(in_car_matrix, "COL38", "mode_car", Ai_2_car_c2)
# Lj 3 transit with Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_wAi_wMode(in_transit_matrix, "COL38", "mode_transit", Ai_2_transit_c2)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Lj_3_transit_mode <- Reduce("+", access_G_list) / length(access_G_list)
# combine
Lj_3_mode <- Lj_3_transit_mode + Lj_3_car_mode



# Ai transit i 3
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Ai_wLj(in_transit_matrix, emp_col,Lj_3_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_3_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_3_transit[Ai_3_transit < 0.001] <- 0.001

# driving Ai with Lj
Ai_3_car <- Ai_wLj(in_car_matrix, emp_col, Lj_3_mode)
Ai_3_car[Ai_3_car < 0.001] <- 0.001



# compute mean Ai
lab_pop_mode <- lab[,c("COL0","COL38", "mode_transit", "mode_car")]
lab_pop_mode$COL38[is.na(lab_pop_mode$COL38)] <- 0
Abar_c <- merge(Ai_3_car,lab_pop_mode,by.x=0,by.y="COL0")
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_car <- Abar_c$NOC_total * Abar_c$mode_car * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- merge(Abar_c,Ai_3_transit,by.x=0,by.y=0)
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_transit <- Abar_c$NOC_total * Abar_c$mode_transit * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- (sum(Abar_c$a_car) + sum(Abar_c$mode_transit)) / sum(Abar_c$COL38)
Abar_c



# update Ai
Ai_3_transit_c3 <- (Abar_o / Abar_c) * Ai_3_transit 
Ai_3_car_c3 <- (Abar_o / Abar_c) * Ai_3_car



# driving Lj 4 with Ai
Lj_4_car_mode <- Lj_wAi_wMode(in_car_matrix, "COL38", "mode_car", Ai_3_car_c3)
# Lj 4 transit with Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_wAi_wMode(in_transit_matrix, "COL38", "mode_transit", Ai_3_transit_c3)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Lj_4_transit_mode <- Reduce("+", access_G_list) / length(access_G_list)
# combine
Lj_4_mode <- Lj_4_transit_mode + Lj_4_car_mode




# Ai transit i 4
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Ai_wLj(in_transit_matrix, emp_col,Lj_4_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_4_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_4_transit[Ai_4_transit < 0.001] <- 0.001

# driving Ai with Lj
Ai_4_car <- Ai_wLj(in_car_matrix, emp_col, Lj_4_mode)
Ai_4_car[Ai_4_car < 0.001] <- 0.001



# compute mean Ai
lab_pop_mode <- lab[,c("COL0","COL38", "mode_transit", "mode_car")]
lab_pop_mode$COL38[is.na(lab_pop_mode$COL38)] <- 0
Abar_c <- merge(Ai_4_car,lab_pop_mode,by.x=0,by.y="COL0")
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_car <- Abar_c$NOC_total * Abar_c$mode_car * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- merge(Abar_c,Ai_4_transit,by.x=0,by.y=0)
rownames(Abar_c) <- Abar_c[,1]
Abar_c[,1] <- NULL
Abar_c$a_transit <- Abar_c$NOC_total * Abar_c$mode_transit * Abar_c$COL38
Abar_c$NOC_total <- NULL
Abar_c <- (sum(Abar_c$a_car) + sum(Abar_c$mode_transit)) / sum(Abar_c$COL38)
Abar_c



# update Ai
Ai_4_transit_c4 <- (Abar_o / Abar_c) * Ai_4_transit 
Ai_4_car_c4 <- (Abar_o / Abar_c) * Ai_4_car



#
conv_mean <- c(mean(abs(Ai_4_transit_c4$NOC_total - Ai_3_transit_c3$NOC_total)),mean(abs(Ai_2_transit_c2$NOC_total - Ai_3_transit_c3$NOC_total)),mean(abs(Ai_2_transit_c2$NOC_total - Ai_1_transit_c1$NOC_total)),mean(abs(Ai_initial_transit$NOC_total - Ai_1_transit_c1$NOC_total)))

conv_max <- c(max(abs(Ai_4_transit_c4$NOC_total - Ai_3_transit_c3$NOC_total)),max(abs(Ai_2_transit_c2$NOC_total - Ai_3_transit_c3$NOC_total)),max(abs(Ai_2_transit_c2$NOC_total - Ai_1_transit_c1$NOC_total)),max(abs(Ai_initial_transit$NOC_total - Ai_1_transit_c1$NOC_total)))

conv_mean_c <- c(mean(abs(Ai_4_car_c4$NOC_total - Ai_3_car_c3$NOC_total)),mean(abs(Ai_2_car_c2$NOC_total - Ai_3_car_c3$NOC_total)),mean(abs(Ai_2_car_c2$NOC_total - Ai_1_car_c1$NOC_total)),mean(abs(Ai_initial_car$NOC_total - Ai_1_car_c1$NOC_total)))

conv_max_c <- c(max(abs(Ai_4_car_c4$NOC_total - Ai_3_car_c3$NOC_total)),max(abs(Ai_2_car_c2$NOC_total - Ai_3_car_c3$NOC_total)),max(abs(Ai_2_car_c2$NOC_total - Ai_1_car_c1$NOC_total)),max(abs(Ai_initial_car$NOC_total - Ai_1_car_c1$NOC_total)))


ggplot() +
  
  geom_line(aes(x = seq(4,1,by=-1),y=conv_mean)) +
  # geom_line(aes(x = seq(4,1,by=-1),y=conv_max)) +
  geom_line(aes(x = seq(4,1,by=-1),y=conv_mean_c)) +
  # geom_line(aes(x = seq(4,1,by=-1),y=conv_max_c)) +
  geom_point(aes(x = seq(4,1,by=-1),y=conv_mean),color = "red") +
  # geom_point(aes(x = seq(4,1,by=-1),y=conv_max),color = "blue") +
  geom_point(aes(x = seq(4,1,by=-1),y=conv_mean_c),color = "green") +
  # geom_point(aes(x = seq(4,1,by=-1),y=conv_max_c),color = "blue") +
  scale_y_log10(breaks = c(1,0.1,0.01,0.001,0.0001,0.00001,0.000001),limits = c(0.000001,0.1)) +
  theme_minimal()



########

# # output to csv
# out_a <- merge(Ai_1_transit,Ai_1_car,by.x = 0, by.y = 0)
# colnames(out_a) <- c("dauid","Ai_transit","Ai_car")
# write.csv(out_a,file = out_file_name)
# 
# 
# # # # checking how much change theres been in Z-score
# Ai_transit_merge <- merge(Ai_initial_transit,Ai_1_transit,by.x = 0,by.y = 0)
# colnames(Ai_transit_merge) <- c("dauid","Ai_t_1","Ai_t_2")
# rownames(Ai_transit_merge) <- Ai_transit_merge$dauid
# Ai_transit_merge$dauid <- NULL
# Ai_transit_Z <- as.data.frame(scale(Ai_transit_merge))
# print(mean(abs(Ai_transit_Z$Ai_t_1-Ai_transit_Z$Ai_t_2)))
# 
# 
# # # # checking how much change theres been in Z-score
# Ai_car_merge <- merge(Ai_initial_car,Ai_1_car,by.x = 0,by.y = 0)
# colnames(Ai_car_merge) <- c("dauid","Ai_c_1","Ai_c_2")
# rownames(Ai_car_merge) <- Ai_car_merge$dauid
# Ai_car_merge$dauid <- NULL
# Ai_car_Z <- as.data.frame(scale(Ai_car_merge))
# print(mean(abs(Ai_car_Z$Ai_c_1-Ai_car_Z$Ai_c_2)))




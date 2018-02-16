# computing a floating catchment measure of access

setwd("~/")

# input folders
in_city <- 'que'
schedule_dir <- paste('matrix_da16_ct11/t_',in_city,'_79',sep = '')
car_matrix_name <- paste('matrix_da16_ct11/drive/c_',in_city,'.csv',sep = '')
out_file_name <- paste('out_a_all/a_',in_city,'.csv',sep = '')

# setup the car matrix - adding 10 min for parking / traffic
in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
in_car_matrix <- in_car_matrix + 420
in_car_matrix$id <- in_car_matrix$id - 420

# employment data
emp <- read.csv('.csv',colClasses=c("tct"="character"))
emp[ emp == "x" ] <- NA


# labour market data
lab <- read.csv('.csv')
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


# initial function for access to jobs - not normalized by labour market
Ai_noLj <- function(transit_matrix, beta, jobtype) {
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- exp(beta * (access_matrix / 60)) # the gravity function
  access_matrix <- t(access_matrix)
  emp_t <- subset(emp, select=c("tct", jobtype)) # change this line for job types !!!!
  access_matrix <- merge(emp_t,access_matrix,by.x="tct",by.y=0)
  # print(sum(access_matrix[,jobtype]),na.rm = TRUE) # for checking job counts (should match table)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- access_matrix * access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- t(access_matrix)
  access_matrix <- as.data.frame(rowSums(access_matrix, na.rm = TRUE))
  colnames(access_matrix) <- c(emp_col)
  return(access_matrix)
}

# initial function for access to jobs - normalized by Lj
Ai_wLj <- function(transit_matrix, beta, jobtype, Lj_in) {
  L_col_name <- colnames(Lj_in)
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- exp(beta * (access_matrix / 60)) # the gravity function
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



# labour market function - no Ai = no mode
Lj_noAi_noMode <- function(in_matrix, beta, poptype) {
  access_matrix <- in_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- exp(beta * (access_matrix / 60))
  lab_in <- lab[,c("COL0",lab_col)]
  lab_in[is.na(lab_in)] <- 0
  access_matrix <- merge(lab_in,access_matrix,by.x="COL0",by.y=0)
  # print(sum(access_matrix[,lab_col]),na.rm = TRUE) # for checking total sum of pop
  rownames(access_matrix) <- access_matrix$COL0
  access_matrix[1] <- NULL
  access_matrix <- access_matrix[,lab_col] * access_matrix
  access_matrix[,lab_col] <- NULL
  q <- as.data.frame(colSums(access_matrix, na.rm = TRUE))
  colnames(q) <- lab_col
  return(q)
}

# Lj_noAi_wMode
Lj_noAi_wMode <- function(in_matrix, beta, poptype, lab_mode) {
  access_matrix <- in_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- exp( -0.02310491 * (access_matrix / 60))
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
Lj_wAi_wMode <- function(in_matrix, beta, poptype, lab_mode, Ai_in) {
  access_matrix <- in_car_matrix
  A_col_name <- colnames(Ai_in)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- exp( -0.02310491 * (access_matrix / 60))
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
Lj_initial_car_mode <- Lj_noAi_wMode(in_car_matrix, -0.02310491, "COL38", "mode_car")

# initial transit Lj Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_noAi_wMode(in_transit_matrix, -0.02310491, "COL38", "mode_transit")
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
  out_matrix <- Ai_wLj(in_transit_matrix, -0.02310491, emp_col,Lj_initial_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_initial_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_initial_transit[Ai_initial_transit < 0.0001] <- 0.0001
# driving Ai with Lj
Ai_initial_car <- Ai_wLj(in_car_matrix, -0.02310491, emp_col, Lj_initial_mode)
Ai_initial_car[Ai_initial_car < 0.0001] <- 0.0001



######

# driving Lj 1 with Ai
Lj_1_car_mode <- Lj_wAi_wMode(in_car_matrix, -0.02310491, "COL38", "mode_car", Ai_initial_car)
# Lj 1 transit with Ai
sched_matrices <- list.files(schedule_dir)
access_G_list <- list()
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Lj_wAi_wMode(in_transit_matrix, -0.02310491, "COL38", "mode_transit", Ai_initial_transit)
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
  out_matrix <- Ai_wLj(in_transit_matrix, -0.02310491, emp_col,Lj_1_mode)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_1_transit <- Reduce("+", access_G_list) / length(access_G_list)
Ai_1_transit[Ai_1_transit < 0.001] <- 0.001
# driving Ai with Lj
Ai_1_car <- Ai_wLj(in_car_matrix, -0.02310491, emp_col, Lj_1_mode)
Ai_1_car[Ai_1_car < 0.001] <- 0.001



########

# output to csv
out_a <- merge(Ai_1_transit,Ai_1_car,by.x = 0, by.y = 0)
colnames(out_a) <- c("dauid","Ai_transit","Ai_car")
write.csv(out_a,file = out_file_name)


# # # checking how much change theres been in Z-score
Ai_transit_merge <- merge(Ai_initial_transit,Ai_1_transit,by.x = 0,by.y = 0)
colnames(Ai_transit_merge) <- c("dauid","Ai_t_1","Ai_t_2")
rownames(Ai_transit_merge) <- Ai_transit_merge$dauid
Ai_transit_merge$dauid <- NULL
Ai_transit_Z <- as.data.frame(scale(Ai_transit_merge))
print(mean(abs(Ai_transit_Z$Ai_t_1-Ai_transit_Z$Ai_t_2)))


# # # checking how much change theres been in Z-score
Ai_car_merge <- merge(Ai_initial_car,Ai_1_car,by.x = 0,by.y = 0)
colnames(Ai_car_merge) <- c("dauid","Ai_c_1","Ai_c_2")
rownames(Ai_car_merge) <- Ai_car_merge$dauid
Ai_car_merge$dauid <- NULL
Ai_car_Z <- as.data.frame(scale(Ai_car_merge))
print(mean(abs(Ai_car_Z$Ai_c_1-Ai_car_Z$Ai_c_2)))


# # #


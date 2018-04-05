# computing a floating catchment measure of access

setwd("~/Dropbox/work/MA_Thesis/analysis_2")

# input folders
in_city <- 'wpg'
schedule_dir <- paste('matrix_da16_ct11/t_',in_city,'_79',sep = '')
car_matrix_name <- paste('matrix_da16_ct11/drive/c_',in_city,'.csv',sep = '')
out_file_name <- paste('out_a_all/a_',in_city,'.csv',sep = '')

# setup the car matrix - adding 10 min for parking / traffic
in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
in_car_matrix <- 1.5 * (in_car_matrix) + 120
in_car_matrix$id <- ((in_car_matrix$id) - 120) / 1.5


# functions
ftij <- function(matrix_in) {
  1 / (1 + exp((matrix_in / 15)-3))
}
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



# initial function for access to jobs - not normalized by labour market
Ai_noLj <- function(transit_matrix, beta, jobtype) {
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix[access_matrix > 5400] <- NA
  access_matrix <- ftij(access_matrix / 60) # the gravity function
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
  access_matrix[access_matrix > 5400] <- NA
  access_matrix <-ftij(access_matrix / 60) # the gravity function
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
  access_matrix[access_matrix > 5400] <- NA
  access_matrix <- ftij(access_matrix / 60)
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
Lj_wAi_wMode <- function(in_matrix, beta, poptype, lab_mode, Ai_in) {
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
# computing a floating catchment measure of access

setwd("~/Dropbox/work/MA_Thesis/analysis_2")

cities <- matrix(c("que","mtl","ott","tor","wpg","cgy","edm","van",1.4,1.65,1.5,1.7,1.4,1.4,1.4,1.6),ncol = 2,nrow = 8)
# cities <- matrix(c("van",1.6),ncol = 2,nrow = 1) # for just 1


# employment data
emp <- read.csv('ct_2016_jobs_data/ct_16_jobs_in_11_ct.csv',colClasses=c("tct"="character"))
emp[ emp == "x" ] <- NA
emp$cmauid <- substr(emp$tct,1,3)
emp <- subset(emp, cmauid != "543")

emp$I_20_39 <- emp$I_20_29 + emp$I_30_39
emp$I_40_59 <- emp$I_40_49 + emp$I_50_59
emp$I_60_79 <- emp$I_60_69 + emp$I_70_79
emp$I_80_up <- emp$I_80_89 + emp$I_90_99 + emp$I_100_over

emp_col <- "NOC_total"
#emp_col <- "I_under_20"
# emp_col <- "NOC_total"



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
lab[is.na(lab)] <- 0
lab$inc_under20 <- lab$COL16 + lab$COL21 + lab$COL22
lab$inc_20_40 <- lab$COL23 + lab$COL24
lab$inc_40_60 <- lab$COL25 + lab$COL26
lab$inc_60_80 <- lab$COL27 + lab$COL28
lab$inc_over80 <- lab$COL31 + lab$COL30 + lab$COL29
lab_col <- "COL38"
# lab_col <- "COL38"


ccc <- 1
while (ccc < 9) {
  in_city <- cities[ccc,1]
  print(in_city)
  conv_factor <- as.numeric(cities[ccc,2])
  
  
  # input folders
  # in_city <- 'wpg'
  schedule_dir <- paste('matrix_da16_ct11/t_',in_city,'_79',sep = '')
  car_matrix_name <- paste('matrix_da16_ct11/drive/c_',in_city,'.csv',sep = '')
  out_file_name <- paste('out_a_all/comp_2step/a_',in_city,'.csv',sep = '')
  
  # setup the car matrix - adding 10 min for parking / traffic
  in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
  in_car_matrix_u <- conv_factor * (in_car_matrix) + 120
  in_car_matrix_u$id <- as.integer(((in_car_matrix_u$id) - 120) / conv_factor)
  in_car_matrix <- in_car_matrix_u
  
  # functions
  #ftij <- function(matrix_in) {
  #  1 / (1 + exp((matrix_in / 15)-3))
  #}
  ftij <- function(matrix_in) {
    (180*(90 + matrix_in)^-1)-1
  }
  
  
  
  
  # overall labour force col
  # "COL38","Labour - Total Sex / Total - Population aged 15 years and over by Labour force status - 25% sample data / In the labour force" (employed and unemployed combined)
  
  
  
  
  
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
  
  
  
  # get counts for lab and inc data
  ctuid_in <- as.data.frame(colnames(in_car_matrix))
  O_emp <- emp[,c("tct",emp_col)]
  O_emp <- merge(ctuid_in,O_emp,by.x="colnames(in_car_matrix)",by.y="tct")
  sum_O_emp <- sum(O_emp[,emp_col])
  print(sum_O_emp)
  
  # get counts for lab and inc data
  dauid_in <- as.data.frame(in_car_matrix$id)
  P_lab <- lab[,c("COL0",lab_col)]
  P_lab <- merge(dauid_in,P_lab,by.x=1,by.y="COL0")
  sum_P_lab <- sum(P_lab[,lab_col])
  print(sum_P_lab)
  
  
  ######
  
  
  
  
  # initial driving Lj no Ai
  Lj_initial_car_mode <- Lj_noAi_wMode(in_car_matrix, lab_col, "mode_car")
  
  # initial transit Lj Ai
  sched_matrices <- list.files(schedule_dir)
  access_G_list <- list()
  f <- 1
  for (sm in sched_matrices) {
    print(sm)
    sm_name <- paste(schedule_dir, '/', sm, sep='')
    in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
    out_matrix <- Lj_noAi_wMode(in_transit_matrix, lab_col, "mode_transit")
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
  
  
  
  # output to csv
  out_a <- merge(Ai_initial_transit,Ai_initial_car,by.x = 0, by.y = 0)
  colnames(out_a) <- c("dauid","Ai_transit","Ai_car")
  write.csv(out_a,file = out_file_name)
  
  ccc <- ccc + 1
}
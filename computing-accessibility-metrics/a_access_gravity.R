# computing access to jobs with a gravity function



# input folders
in_city <- 'que'
schedule_dir <- paste('matrix_da16_ct11/t_',in_city,'_79',sep = '')
car_matrix_name <- paste('matrix_da16_ct11/drive/c_',in_city,'.csv',sep = '')
out_file_name <- paste('out_a_all/G_i/g_',in_city,'.csv',sep = '')

# setup the car matrix - adding 10 min for parking / traffic
in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
in_car_matrix <- 1.5 * (in_car_matrix) + 120
in_car_matrix$id <- ((in_car_matrix$id) - 120) / 1.5


# functions
#ftij <- function(matrix_in) {
#  exp(-0.0231 * matrix_in)
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
# area and cma data
dfca <- read.csv("da_2016_data/da_area_cma.csv")



# labour colomn - total labour force
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


# Ai transit initial with Lj
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {
  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')
  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)
  out_matrix <- Ai_noLj(in_transit_matrix, -0.02310491, emp_col)
  access_G_list[[f]] <- out_matrix
  f <- f + 1
}
# averaging the initial transit
Ai_initial_transit <- Reduce("+", access_G_list) / length(access_G_list)
#Ai_initial_transit[Ai_initial_transit < 0.01] <- 0.01
# driving Ai with Lj
Ai_initial_car <- Ai_noLj(in_car_matrix, -0.02310491, emp_col)
#Ai_initial_car[Ai_initial_car < 0.01] <- 0.01

out_a <- merge(Ai_initial_transit,Ai_initial_car,by.x = 0, by.y = 0)
colnames(out_a) <- c("dauid","Ai_transit","Ai_car")
write.csv(out_a,file = out_file_name)

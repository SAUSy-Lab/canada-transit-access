# computing a floating catchment measure of access

setwd("~/Dropbox/work/canada_access")

# input folders
schedule_dir <- ''
car_matrix_name <- '.csv'
out_file_name <- ".csv"

# employment data
emp <- read.csv('.csv')

# labour market data
lab <- read.csv('.csv')
lab[ lab == "x" ] <- NA
lab[ lab == "F" ] <- NA
lab[ lab == ".." ] <- NA
lab[ lab == "..." ] <- NA

# initial function for access to jobs - not normalized by labour market
access_score_transit_noL <- function(transit_matrix, beta) {
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- access_matrix ^ -beta

  access_matrix <- t(access_matrix)
  emp_t <- subset(emp, select=c("ctuid","total")) # change this line for job types !!!!
  access_matrix <- merge(emp_t,access_matrix,by.x="ctuid",by.y=0)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- access_matrix * access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- t(access_matrix)
  access_matrix <- as.data.frame(rowSums(access_matrix, na.rm = TRUE))
  colnames(access_matrix) <- c(beta)
  return(access_matrix)
}

# computing this for the first run
access_G_list <- list()
# looping the cube for the first time transit
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {

  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')

  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)

  nrout <- nrow(in_transit_matrix)
  out_matrix <- matrix(data = NA, nrow = nrout, ncol = 1)

  # access for gravity
  BS <- c(0.5,1) # decay parameters
  for (b in BS) {
    print(b)
    out_matrix <- cbind(out_matrix, access_score_transit_noL(in_transit_matrix, b))
  }
  out_matrix <- out_matrix[,-1]

  access_G_list[[f]] <- out_matrix
  f <- f + 1

}
# averaging the initial transit
Ai_initial_transit <- Reduce("+", access_G_list) / length(access_G_list)

#########################


########################

# computing the initial car access

in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
in_car_matrix <- in_car_matrix + 600
in_car_matrix$id <- in_car_matrix$id - 600

access_score_car_noL <- function(car_matrix, beta) {
  access_matrix <- car_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix < 0] <- NA
  access_matrix <- access_matrix ^ -beta
  access_matrix <- t(access_matrix)
  emp_t <- subset(emp, select=c("ctuid","total")) # change this line for job types !!!!
  access_matrix <- merge(emp_t,access_matrix,by.x="ctuid",by.y=0)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- access_matrix * access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- t(access_matrix)
  access_matrix <- as.data.frame(rowSums(access_matrix, na.rm = TRUE))
  colnames(access_matrix) <- c(beta)
  return(access_matrix)
}

Ai_initial_car <- access_score_car_noL(car_matrix = in_car_matrix, beta = 1)

##########################


# setup the out matrix

out_data <- Ai_initial_transit
out_data$`0.5` <- NULL
colnames(out_data) <- c("i0")
out_data$i0 <- (out_data$i0)



# function for access to the labour force

# first, by car
Lj_car <- function(car_matrix, beta, Ai_car) {

  access_matrix <- car_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[access_matrix < 0] <- NA

  lab <- as.data.frame(lab[,c("dauid","labour_force")])
  lab_Ai <- merge(lab,Ai_car,by.x = "dauid", by.y = 0)
  # add in real mode in here
  lab_Ai$ratio <- 0.7 * as.numeric(as.character(lab_Ai$labour_force)) / lab_Ai$`1`
  lab_Ai$labour_force <- NULL
  lab_Ai$`1` <- NULL
  lab_Ai$ratio[is.infinite(lab_Ai$ratio)] <- 0

  access_matrix <- merge(lab_Ai,access_matrix,by.x="dauid",by.y=0)
  access_matrix[,3] <- NULL
  rownames(access_matrix) <- access_matrix$dauid
  access_matrix[,1] <- NULL

  access_matrix <- access_matrix$ratio * access_matrix ^ -beta

  q <- as.data.frame(colSums(access_matrix, na.rm = TRUE))
  colnames(q) <- beta
  #q = q[-1,]
  return(q)
}

Lj_car_new <- Lj_car(in_car_matrix,1,Ai_initial_car)


# by transit
Lj_transit <- function(transit_matrix, beta, Ai_transit) {

  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[access_matrix < 0] <- NA

  lab <- as.data.frame(lab[,c("dauid","labour_force")])
  lab_Ai <- merge(lab,Ai_transit,by.x = "dauid", by.y = 0)
  # add in real mode in here
  lab_Ai$ratio <- 0.3 * as.numeric(as.character(lab_Ai$labour_force)) / lab_Ai$`1`
  lab_Ai$labour_force <- NULL
  lab_Ai$`1` <- NULL
  lab_Ai$`0.5`<- NULL
  lab_Ai$ratio[is.infinite(lab_Ai$ratio)] <- 0
  access_matrix <- merge(lab_Ai,access_matrix,by.x="dauid",by.y=0)
  access_matrix[,3] <- NULL
  rownames(access_matrix) <- access_matrix$dauid
  access_matrix[,1] <- NULL

  access_matrix <- access_matrix$ratio * access_matrix ^ -beta

  q <- as.data.frame(colSums(access_matrix, na.rm = TRUE))
  colnames(q) <- beta
  #q = q[-1,]
  return(q)

}

# looping the directories
access_Q_list <- list()
f <- 1
for (sm in sched_matrices) {

  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')

  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)

  nrout <- ncol(in_transit_matrix)
  out_matrix <- matrix(data = NA, nrow = nrout, ncol = 1)

  # access for gravity
  BS <- c(0.5,1) # decay parameters
  for (b in BS) {
    print(b)
    out_matrix <- cbind(out_matrix, Lj_transit(in_transit_matrix, b, Ai_initial_transit))
  }
  out_matrix <- out_matrix[,-1]



  access_Q_list[[f]] <- out_matrix
  f <- f + 1

}

# reducing

Lj_transit_new <- Reduce("+", access_Q_list) / length(access_Q_list)

# combine
Lj_both_new <- merge(Lj_car_new,Lj_transit_new,by.x = 0, by.y = 0)

Lj_both_new$c1 <- Lj_both_new$`1.x` + Lj_both_new$`1.y`
Lj_both_new$`1.x` <- NULL
Lj_both_new$`0.5` <- NULL
Lj_both_new$`1.y` <- NULL

# out result has both car and transit, can save to csv here, or continue iterating (see other script)

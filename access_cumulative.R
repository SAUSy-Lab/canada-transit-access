# computes a measure of cumulative access
# the number of jobs reachable within a time window, T

setwd("~/Dropbox/work/canada_access")

# in data
schedule_dir <- 'dir'
out_file_name <- ".csv"

# employment data
emp <- read.csv('jobs.csv')

# function for a transit matrix, and threshold T
access_score_T <- function(transit_matrix,T) {
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix > T] <- NA
  access_matrix[access_matrix < 0] <- NA
  access_matrix[access_matrix <= T] <- 1
  access_matrix <- t(access_matrix)
  emp_t <- subset(emp, select=c("ctuid","total")) # change this line for job types !!!!
  access_matrix <- merge(emp_t,access_matrix,by.x="ctuid",by.y=0)
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- access_matrix * access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix <- t(access_matrix)
  access_matrix <- as.data.frame(rowSums(access_matrix, na.rm = TRUE))
  colnames(access_matrix) <- c(T)
  return(access_matrix)
}

## running the function once if needed
# access_score_T(in_transit_matrix,3600)


# running for multiple transit travel time matrices -
# e.g. all minutes over a rush hour period and then averaging
access_T_list <- list()

sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {

  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')

  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)

  nrout <- nrow(in_transit_matrix)
  out_matrix <- matrix(data = NA, nrow = nrout, ncol = 1)

  x <- 60
  while (x <= 5400) {
    out_matrix <- cbind(out_matrix, access_score_T(in_transit_matrix, x))
    print(x)
    x <- x + 60
  }
  out_matrix <- out_matrix[,-1]

  # q <- access_score_T(in_transit_matrix,3600)

  access_T_list[[f]] <- out_matrix
  f <- f + 1

}

# averaging the thing
T_means <- Reduce("+", access_T_list) / length(access_T_list)

# and output to a csv file
write.csv(x = T_means,file = out_file_name)

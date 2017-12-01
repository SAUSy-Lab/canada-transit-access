# computes access to employment using a gravity formula
# specifically, using an inverse-power decay function with a paramater beta

setwd("~/Dropbox/work/canada_access")

# which city
schedule_dir <- '/dir'
out_file_name <- ".csv"

# employment data
emp <- read.csv('jobs.csv')

# function for the gravity function
access_score_G <- function(transit_matrix, beta) {
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


access_G_list <- list()


# looping and averaing for multiple matrices
sched_matrices <- list.files(schedule_dir)
f <- 1
for (sm in sched_matrices) {

  print(sm)
  sm_name <- paste(schedule_dir, '/', sm, sep='')

  in_transit_matrix <- read.csv(sm_name, check.names=FALSE)

  nrout <- nrow(in_transit_matrix)
  out_matrix <- matrix(data = NA, nrow = nrout, ncol = 1)

  # access for gravity
  BS <- c(0.25,0.5,0.75,1,1.25,1.5,1.75,2) # decay parameters
  for (b in BS) {
    print(b)
    out_matrix <- cbind(out_matrix, access_score_G(in_transit_matrix, b))
  }
  out_matrix <- out_matrix[,-1]

  access_G_list[[f]] <- out_matrix
  f <- f + 1

  #break

}


# averaging the thing
G_means <- Reduce("+", access_G_list) / length(access_G_list)
write.csv(x = G_means,file = out_file_name)



# # #

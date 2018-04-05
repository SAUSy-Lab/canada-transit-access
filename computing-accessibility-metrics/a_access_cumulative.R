# computing a floating catchment measure of access
# i.e. number of jobs that can be reached within a travel time threshold


library(ggplot2)
library(stringr)


# input folders
in_city <- 'tor'
schedule_dir <- paste('matrix_da16_ct11/t_',in_city,'_79',sep = '')
car_matrix_name <- paste('matrix_da16_ct11/drive/c_',in_city,'.csv',sep = '')
out_file_name <- paste('out_a_all/ca/a_',in_city,'.csv',sep = '')

# setup the car matrix - adding 10 min for parking / traffic
in_car_matrix <- read.csv(car_matrix_name, check.names=FALSE)
in_car_matrix <- 1.5 * (in_car_matrix) + 120
in_car_matrix$id <- ((in_car_matrix$id) - 120) / 1.5



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




# function for a transit matrix, and threshold T
access_score_T <- function(transit_matrix,T) {
  access_matrix <- transit_matrix
  rownames(access_matrix) <- access_matrix[,1]
  access_matrix[,1] <- NULL
  access_matrix[access_matrix > T] <- NA
  access_matrix[access_matrix < 0] <- NA
  access_matrix[access_matrix <= T] <- 1
  access_matrix <- t(access_matrix)
  emp_t <- subset(emp, select=c("tct", "NOC_total")) # change this line for job types !!!!
  access_matrix <- merge(emp_t,access_matrix,by.x="tct",by.y=0)
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

  x <- 900
  while (x <= 5400) {
    out_matrix <- cbind(out_matrix, access_score_T(in_transit_matrix, x))
    print(x)
    x <- x + 900
  }
  out_matrix <- out_matrix[,-1]

  # q <- access_score_T(in_transit_matrix,3600)

  access_T_list[[f]] <- out_matrix
  f <- f + 1

}

# averaging the thing
T_means <- Reduce("+", access_T_list) / length(access_T_list)
write.csv(x = T_means,file = out_file_name)




#### # for plotting cumulative access by dept time

# # 30 minutes
# l30 <- c()
# for (s in access_T_list) {
#   t1800 <- (s["46110663","1800"])
#   l30 <- c(l30,t1800)
# }
#
# mean(l30)
# l30 <- as.data.frame(l30)
# l30$i <- sched_matrices
#
# # 45 minutes
# l45 <- c()
# for (s in access_T_list) {
#   t1800 <- (s["46110663","2700"])
#   l45 <- c(l45,t1800)
# }
#
# mean(l45)
# l45 <- as.data.frame(l45)
# l45$i <- sched_matrices
#
# l <- merge(l30,l45,by.x="i",by.y="i")
#
# lspl <- as.data.frame(str_split_fixed(l$i, "_",5))
# l <- merge(l,lspl, by.x = 0, by.y = 0)
#
# l$i2 <- substr(l$i,8,8)
# l <- subset(l,i2 != 1)
# l$m <- as.numeric(substr(l$i,10,11))
#
#
# ggplot() +
#   geom_hline(yintercept = mean(l$l30),color = "red",linetype=5) +
#   geom_hline(yintercept = mean(l$l45),color = "red",linetype=5) +
#   geom_line(data = l, aes(x=m,y=l30)) +
#   geom_point(data = l, aes(x=m,y=l30)) +
#   geom_line(data = l, aes(x=m,y=l45)) +
#   geom_point(data = l, aes(x=m,y=l45)) +
#   theme_minimal() + xlab("") + ylab("") +
#   scale_x_continuous(minor_breaks = seq(0, 60, 1),breaks = seq(0,60,5)) +
#   scale_y_continuous(breaks = seq(0, 200000, 20000),limits = c(1,200000))

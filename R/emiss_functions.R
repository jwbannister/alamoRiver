# emiss_functions.R -- functions related to estimating dust emissions 
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 

#' Read in multiple data files
#' 
#' Read in multiple files of the same format and append all into one data frame.
#' Additional column is added with name of source data file for each row.
#' 
#' @param path String. Path for data files.
#' @param filenames String. File names. Can be a vector for multiple files.
#' @return Data frame. 
#' @examples
#' multi_read("./data-raw/", c("file1.csv", "file2.csv", "file3.csv"))
multi_read <- function(path, filenames){
  data_call <- sapply(filenames, function(x) paste0(path, x))    
  stopifnot(file.exists(data_call))
  for (i in 1:length(data_call)){
    if (i==1){ 
      df1 <- read.csv(data_call[i], skip=4, header=FALSE)
      df1$file <- data_files[i]
    } else{
      df2 <- read.csv(data_call[i], skip=4, header=FALSE)
      df2$file <- data_files[i]
      df1 <- rbind(df1, df2)
    }
  }
  df1
}


#' Build reference list for cardinal direction angles.
#' 
#' @return A list of vectors for the 16 cardinal directions. 
#' Vector[1] = minimum angle, vector[2] = maximum angle, 
#' vector [3] = central angle for direction.
direction_reference <- function(){
  angles <- seq(11.25, 348.75, 22.5)
  angles <- c(-11.25, angles)
  cardinal_dirs <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
                     "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  dir_ref <- vector(mode="list", length=length(cardinal_dirs))
  for (i in 1:length(cardinal_dirs)){
    dir_ref[i] <- list(c(angles[i], angles[i+1],
                       mean(c(angles[i], angles[i+1]))))
  }
  names(dir_ref) <- cardinal_dirs
  dir_ref
}


#' Assign cardinal direction to wind direction.
#' 
#' Go through a data file containing wind directions (in angles) and assign one 
#' of the 16 cardinal directions (in a new column).
#' 
#' @param df1 Data frame. Data frame containing wind direction (in angle).
#' @param dir_column. Integer. Number of wind direction column in df1.
#' @return Data frame. Original data frame df1 with additional column listing 
#' named wind direction.
#' @examples
#' assign_cardinal(mydata, 3)
assign_cardinal <- function(df1, dir_column){
  angles <- seq(0, 360, 11.25)
  cardinal_dirs <- c("N", rep("NNE", 2), rep("NE", 2), rep("ENE", 2),
                     rep("E", 2), rep("ESE", 2), rep("SE", 2), rep("SSE", 2), 
                     rep("S", 2), rep("SSW", 2), rep("SW", 2), rep("WSW", 2), 
                     rep("W", 2), rep("WNW", 2), rep("NW", 2), rep("NNW", 2), 
                     "N")
  f1 <- function(x) cardinal_dirs[findInterval(x, angles)]
  dir_name <- lapply(df1[ , dir_column], f1)
  dir_name <- unlist(dir_name)
  df1$wd.named <- dir_name
  df1
}

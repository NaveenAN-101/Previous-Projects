file <- readline(prompt="Enter the file name: ")
tab <- read.table(file, header = TRUE)

while(TRUE){
  print(names(tab))
  print("The above set is the menu of variables")
  
  tim_str <- readline(prompt = "Enter the time variable: ")
  
  if (!(tim_str %in% names(tab))) {
    print("Invalid time variable...Exiting the loop")
    break
  }
  
  tim <- tab[[tim_str]]
  
  print("Which two variables do you wish to correlate?")
  x <- readline(prompt = "Enter variable 1: ")
  y <- readline(prompt = "Enter variable 2: ")
  
  if (!(x %in% names(tab)) || !(y %in% names(tab))) {
    print("Invalid variable input...try again")
    next
  }
  
  var1 <- tab[[x]]
  var2 <- tab[[y]]
  
  cor_vec <- c()
  j <- 1
  int <- as.numeric(readline(prompt = "Enter the time difference: "))
  
  if (is.na(int) || int <= 0) {
    print("Invalid time difference...Exiting the loop")
    break
  }
  
  # Correct sequence and indexing for correlation
  time_points <- seq(min(tim), max(tim), int)
  
  for (i in seq_along(time_points)) {
    start <- time_points[i]
    end <- start + int
    indices <- which(tim >= start & tim < end)
    
    if (length(indices) > 1) {
      cor_vec[j] <- cor(var1[indices], var2[indices])
      j <- j + 1
    }
  }
  
  # Write the cor_vec to a .dat file with a header
  output_file <- paste0("correlation_", x, "_", y, ".dat")
  header <- paste("Correlation between", x, "and", y)
  write.table(cor_vec, file = output_file, col.names = header, row.names = FALSE)
  print(paste("Correlation vector written to", output_file))
  
  promp <- readline(prompt = "Do you wish to continue? (y/n): ")
  if (promp == "y") {
    next
  } else if (promp == "n") {
    break
  } else {
    print("Invalid character....exiting the loop!")
    break
  }
}

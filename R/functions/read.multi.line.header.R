read.multi.line.header <- function(file, header.lines = 2, sep = ","){
  
  header <- scan(file, nlines = 1, what = character(), sep = sep)
  
  data <- read.table(file, skip = header.lines, header = FALSE, sep = sep)
  
  base::names(data) <- header
  
  return(data)
}
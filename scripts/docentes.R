library(stringr)

DIRECTORY <- "/home/gvso/Data/MEC/data/docentes/"
CUR_YEAR <- 2018
STARTING_YEAR <- 1985

file <- paste0(DIRECTORY, "funcionarios_docentes_2018_9.csv")

## Gets only document number and seniority.
columns <- c(rep("NULL", 2), NA, rep("NULL", 2), NA, rep("NULL", 14))
data2018 <- read.csv(file, colClasses=columns)
data2018$documento <- as.numeric(data2018$documento)
data2018 <- data2018[!duplicated(data2018$documento), ]

#########################################################
######## Creates the seniority columns. #################
#########################################################
data2018$seniority_year <- sapply(data2018$antiguedad_docente, function(value) {
  seniority <- as.numeric(str_extract_all(value, "[0-9]+")[[1]])
  
  seniority[1]
})
data2018$seniority_month <- sapply(data2018$antiguedad_docente, function(value) {
  seniority <- as.numeric(str_extract_all(value, "[0-9]+")[[1]])
  
  seniority[2]
})
data2018$antiguedad_docente <- NULL

#########################################################
###### Estimates number of new teachers per year ########
#########################################################
plot_delta_teachers <- function() {
  year <- sapply(data2018$seniority_year, function(seniority_year) {
    CUR_YEAR - seniority_year
  })
  
  hirings <- as.data.frame(table(year))
  
  years <- as.numeric(levels(hirings$year))[hirings$year]
  freq <- hirings$Freq
  
  plot(years, freq, type='l', xaxt="n", xlab='Año', ylab='Nuevos docentes', 
       xlim=c(STARTING_YEAR, CUR_YEAR), col='purple', lty=3, lwd=2)
  axis(1, at = seq(STARTING_YEAR, CUR_YEAR, by = 2), las=2) 
}

#########################################################
######## Estimates number of teachers per year ##########
#########################################################
plot_number_of_teachers <- function() {
  seniority_years <- (CUR_YEAR - 1985) : min(data2018$seniority_year)
  
  number_teachers <- c()
  total <- 0
  
  for (i in seniority_years) {
    count <- nrow(data2018[data2018$seniority_year == i, ])
    
    total <- total + count
    
    number_teachers <- c(number_teachers, total)
  }
  
  plot(CUR_YEAR - seniority_years, number_teachers, xaxt="n", xlab='Año', ylab='Cantidad de docentes (aproximado)', 
       type='l', col='purple', lty=3, lwd=2)
  axis(1, at = seq(STARTING_YEAR, CUR_YEAR, by = 2), las=2)
}


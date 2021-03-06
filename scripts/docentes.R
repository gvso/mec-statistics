library(stringr)

DIRECTORY <- "/home/gvso/Data/MEC/data/docentes/"
CUR_YEAR <- 2018
STARTING_YEAR <- 1995
RETIRING_YEAR <- 25

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
######## Estimates number of teachers per year ##########
#########################################################
plot_number_of_teachers <- function() {
  seniority_years <- (CUR_YEAR - 1965) : min(data2018$seniority_year)
  
  number_teachers <- c()
  total <- 0
  
  for (i in seniority_years) {
    count <- nrow(data2018[data2018$seniority_year == i, ])
    
    total <- total + count
    
    number_teachers <- c(number_teachers, total)
  }
  
  print(total)
  
  #plot(CUR_YEAR - seniority_years, number_teachers, xaxt="n", xlab='Año', ylab='Cantidad de docentes (aproximado)', 
  #     type='l', col='purple', lty=3, lwd=2)
  #axis(1, at = seq(STARTING_YEAR, CUR_YEAR, by = 2), las=2)

  plot(CUR_YEAR - seniority_years[30:length(seniority_years)], number_teachers[30:length(seniority_years)], xaxt="n", xlab='Año', ylab='Cantidad de docentes (aproximado)', 
     type='l', col='purple', lty=3, lwd=2)
  axis(1, at = seq(STARTING_YEAR, CUR_YEAR, by = 2), las=2)
}

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
  
  hirings_table <- data.frame(year=years, teachers=freq)
  
  print(hirings_table)
  
  plot(years, freq, type='l', xaxt="n", xlab='Año', ylab='Nuevos docentes', 
       xlim=c(STARTING_YEAR, CUR_YEAR), col='purple', lty=3, lwd=2)
  axis(1, at = seq(STARTING_YEAR, CUR_YEAR, by = 2), las=2) 
}

#########################################################
#### Estimates number of retiring teachers per year #####
#########################################################
plot_retiring_teachers <- function() {
  years <- 2019:2030
  
  retiring <- sapply(years, function(year) {
    nrow(data2018[CUR_YEAR + (RETIRING_YEAR - data2018$seniority_year) == year, ])
  })
  
  retiring_table <- data.frame(year=years, teachers=retiring)
  
  print(retiring_table)
  
  plot(years, retiring, xaxt="n", xlab='Año', ylab='Cantidad de docentes (aproximado)', 
       type='l', col='purple', lty=3, lwd=2)
  axis(1, at = seq(CUR_YEAR, 2030, by = 2), las=2)
}


library(plyr)

# DIRECTORY <- "/home/gvso/Data/MEC/data/matricula/"

inicial_file <- paste0(DIRECTORY, "matriculaciones_inicial_2013.csv")
eeb_file <- paste0(DIRECTORY, "matriculaciones_eeb_2013.csv")
media_file <- paste0(DIRECTORY, "matriculaciones_media_2013.csv")

# Educación Inicial.
columns <- c("NULL", NA, NA, rep("NULL", 7), rep(NA, 3), rep("NULL", 9), NA, NA, rep("NULL", 2))
inicial <- read.csv(inicial_file, colClasses=columns)
inicial <- inicial[inicial$sector_o_tipo_gestion != "PRIVADA", ]
inicial$total <- inicial$total_matriculados_hombre + inicial$total_matriculados_mujer

# Educacion Escolar Básica (EEB).
#columns <- c("NULL", NA, NA, rep("NULL", 7), NA, rep("NULL", 3), rep(NA, 6), rep("NULL", 12), NA, NA)
columns <- c("NULL", NA, NA, rep("NULL", 7), rep(NA, 3), rep("NULL", 19), NA, NA)
eeb <- read.csv(eeb_file, colClasses=columns)
eeb <- eeb[eeb$sector_o_tipo_gestion != "PRIVADA", ]
eeb$total <- eeb$total_matriculados_hombre + eeb$total_matriculados_mujer

# Educación Media.
columns <- c("NULL", NA, NA, rep("NULL", 7), rep(NA, 3), "NULL", rep(NA, 8))
media <- read.csv(media_file, 1, colClasses=columns)
media <- media[media$sector_o_tipo_gestion != "PRIVADA", ]
media$total <- rowSums(media[, 6:12])

total_students <- sum(inicial$total) + sum(eeb$total) + sum(media$total)

inicial_total <- inicial[, c(1:3, 8)]
eeb_total <- eeb[, c(1:3, 8)]
media_total <- media[, c(1:3, 14)]
schools <- rbind.fill(inicial_total, eeb_total, media_total)
schools <- aggregate(schools["total"], by=schools[c("codigo_institucion", "codigo_establecimiento", "codigo_departamento")], FUN=sum)

#########################################################
##### Estimate number of schools with conditions. #######
#########################################################

# nrow(schools) 
# nrow(schools[schools$total < 200, ])

# Asunción
# nrow(schools[schools$total < 200 & schools$codigo_departamento == 0, ])

# Central
# nrow(schools[schools$total < 200 & schools$codigo_departamento == 11, ])

#codigo <- eeb_total$codigo_institucion
#n_occur <- data.frame(table(codigo))
#n_occur[n_occur$Freq > 1,]

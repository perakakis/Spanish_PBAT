library(ggplot2)
source("./code/auxFunctions.R")

df <- read.csv(file = "./data/data.csv")

# Sex
N_male <- length(which(df$Sexo=="Hombre"))
N_female <- length(which(df$Sexo=="Mujer"))

# Age group
summarize_and_plot(df, "Grupo.de.edad")

# Social class
summarize_and_plot(df, "Clase.Social")

# Income
summarize_and_plot(df, "Ingresos.Mensuales.en.el.Hogar")

# Occupation
summarize_and_plot(df, "Ocupación")

# Municipal size
summarize_and_plot(df, "Tamaño.del.Municipio")

# Studies
summarize_and_plot(df, "Nivel.de.estudios")

# Province
summarize_and_plot(df, "Provincia")

# CCAA
summarize_and_plot(df, "CCAA")

# Health
summarize_and_plot(df, "HEALTH")

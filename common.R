if (!require("pacman")) install.packages("pacman")
library("pacman")

pacman::p_load(dplyr)

pwt <- function() {
  pacman::p_load(readxl)
  return(read_xlsx("../Data/pwt1001.xlsx", sheet = "Data"))
}

FDI <- function(sheet_number){
  pacman::p_load(readxl)
  return(read_xlsx("../Data/FDI-figures.xlsx", sheet = sheet_number))
}
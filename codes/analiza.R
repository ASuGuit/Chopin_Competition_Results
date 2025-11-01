library(data.table)

source("funkcje.R")

wyniki_1 <- fread("data-raw/Chopin_wyniki_1.csv")
wyniki_2 <- fread("data-raw/Chopin_wyniki_2.csv")
wyniki_3 <- fread("data-raw/Chopin_wyniki_3.csv")
wyniki_4 <- fread("data-raw/Chopin_wyniki_4.csv")

wynik <- policz_konkurs(wyniki_1, wyniki_2,
                        wyniki_3, wyniki_4,
                        progi = c(3, 2, 2, 2),
                        wagi = c(0.1, 0.2, 0.35, 0.35))

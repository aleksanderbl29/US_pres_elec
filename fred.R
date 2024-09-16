if (!require(fredr)) install.packages("fredr")

link <- "https://fred.stlouisfed.org/series/DSPIC96"

real_disposable_pers_inc <- fredr(series_id = "DSPIC96")
plot(real_disposable_pers_inc$value)


print("Deriving financial year")
lt <- as.POSIXlt(deliveries$epistart)
deliveries[, epistart_fyear_starting := lt$year + (lt$mo >= 3) + 1900 - 1] # -1 to make it fyear starting
rm(lt)

print("Cleaning delivery episodes")

print("Individual maternity tail variables")
deliveries[neocare %in% c(8, 9), neocare := NA]
deliveries[numpreg == 99, numpreg := NA]
deliveries[numbaby %in% c("9", "X"), numbaby := NA]
deliveries[anagest > 50 | anagest < 0, anagest := NA]

deliveries[, anasdate := as.Date(anasdate)]
min_date <- as.Date("1901-01-01")
max_date <- as.Date(paste0(cohort_years[length(cohort_years)] + 1, "-03-31"))
deliveries[anasdate <= min_date | anasdate > max_date, anasdate := NA]
rm(min_date, max_date)

deliveries[delchang == 9, delchang := NA]
deliveries[delinten == 9, delinten := NA]
deliveries[delonset == 9, delonset := NA]
deliveries[delposan == 9, delposan := NA]
deliveries[delprean == 9, delprean := NA]
deliveries[matage == 0 | matage > 100, matage := NA]

for (i in 1:9) {
  
  print(paste0("Multiple vars (", i, " of 9)"))
        
  curr_var <- paste0("biresus_", i)
  deliveries[get(curr_var) %in% c(8, 9), (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("birordr_", i)
  deliveries[get(curr_var) %in% c("8", "9", "X"), (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("birstat_", i)
  deliveries[get(curr_var) %in% c("8", "9", "X"), (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("birweit_", i)
  deliveries[get(curr_var) >= 7001 | get(curr_var) == 0, (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("delmeth_", i)
  deliveries[get(curr_var) %in% c("X", " X"), (curr_var) := NA]
  deliveries[get(curr_var) == "10", (curr_var) := "1"]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("delstat_", i)
  deliveries[get(curr_var) == 9, (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("gestat_", i)
  deliveries[get(curr_var) == 99, (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
  curr_var <- paste0("sexbaby_", i)
  deliveries[get(curr_var) %in% c("M"), (curr_var) := "1"]
  deliveries[get(curr_var) %in% c("F"), (curr_var) := "2"]
  deliveries[epistart_fyear_starting >= 1996 & !(get(curr_var) %in% c("1", "2", "9")), (curr_var) := NA]
  deliveries[epistart_fyear_starting < 1996 & !(get(curr_var) %in% c("1", "2", "3")), (curr_var) := NA]
  deliveries[, (curr_var) := as.integer(get(curr_var))]
  
}

rm(curr_var, i)


print("Dates")
deliveries <- clean_hes_dates(deliveries)

for (i in 1:24) {
  curr_var <- paste0("opdate_", sprintf("%02d", i))
  print(curr_var)
  deliveries[, (curr_var) := as.Date(get(curr_var))]
  deliveries[get(curr_var) <= as.Date("1901-01-01"), (curr_var) := NA]
}

rm(i, curr_var)


print("postdur and antedur")
deliveries[!is.na(postdur) & as.Date(epiend - postdur) < epistart, postdur := NA]
deliveries[!is.na(antedur) & as.Date(epistart + antedur) > epiend, antedur := NA]


print("Saving")
save(deliveries, file = "processed/tmp_deliveries_02.rda")
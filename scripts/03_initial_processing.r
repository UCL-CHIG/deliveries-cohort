
print("Delivery status")
col_diag <- paste0("diag_", sprintf("%02d", 1:20))
col_birstat <- paste0("birstat_", 1:9)
diag_live_birth <- c("Z370", "Z372", "Z373", "Z375", "Z376")
diag_still_birth <- c("Z371", "Z374", "Z377")


print("Identifying live births")
deliveries[, livebirth := apply(deliveries[, col_birstat, with = F], 1, function(x) any(x == 1))]
deliveries[, livebirth := ifelse(apply(deliveries[, col_diag, with = F], 1, function(x) any(substr(x, 1, 4) %in% diag_live_birth)) == T, T, livebirth)]


print("Identifying stillbirths")
deliveries[, stillbirth := apply(deliveries[, col_birstat, with = F], 1, function(x) any(x %in% 2:4))]
deliveries[, stillbirth := ifelse(apply(deliveries[, col_diag, with = F], 1, function(x) any(substr(x, 1, 4) %in% diag_still_birth)) == T, T, stillbirth)]

rm(col_birstat, diag_live_birth, diag_still_birth)

deliveries[, livebirth := as.integer(livebirth)]
deliveries[, stillbirth := as.integer(stillbirth)]

deliveries[livebirth == 0, livebirth := NA]
deliveries[stillbirth == 0, stillbirth := NA]


print("Identifying multiples")

deliveries[, multiples := as.integer(NA)]
diag_singleton <- c("Z370", "Z371", "Z380", "Z381", "Z382")
diag_twin <- c("Z372", "Z373", "Z374", "Z383", "Z384", "Z385")
diag_multiple <- c("Z375", "Z376", "Z377", "Z386", "Z387", "Z388")


print("Singletons")
deliveries[numbaby == 1 |
             (is.na(sexbaby_2) & !is.na(sexbaby_1)) |
             (is.na(birordr_2) & !is.na(birordr_1)) |
             (is.na(birweit_2) & !is.na(birweit_1)) |
             (is.na(delmeth_2) & !is.na(delmeth_1)) |
             (is.na(gestat_2) & !is.na(gestat_1)) |
             (is.na(birstat_2) & !is.na(birstat_1)) |
             (is.na(biresus_2) & !is.na(biresus_1)),
           multiples := 1]


deliveries[, multiples := ifelse(apply(deliveries[, col_diag, with = F], 1, function(x) any(substr(x, 1, 4) %in% diag_singleton)) == T,
                                 1, multiples)]


print("Twins")
deliveries[numbaby == 2 |
             (is.na(sexbaby_3) & !is.na(sexbaby_2) & !is.na(sexbaby_1)) |
             (is.na(birordr_3) & !is.na(birordr_2) & !is.na(birordr_1)) |
             (is.na(birweit_3) & !is.na(birweit_2) & !is.na(birweit_1)) |
             (is.na(delmeth_3) & !is.na(delmeth_2) & !is.na(delmeth_1)) |
             (is.na(gestat_3) & !is.na(gestat_2) & !is.na(gestat_1)) |
             (is.na(birstat_3) & !is.na(birstat_2) & !is.na(birstat_1)) |
             (is.na(biresus_3) & !is.na(biresus_2) & !is.na(biresus_1)),
           multiples := 2]

deliveries[, multiples := ifelse(apply(deliveries[, col_diag, with = F], 1, function(x) any(x %in% diag_twin)) == T,
                                 2, multiples)]


print("Multiples")
cols_to_check <- c("sexbaby_",
                   "birordr_",
                   "birweit_",
                   "delmeth_",
                   "gestat_",
                   "birstat_",
                   "biresus_")

for (col_name in cols_to_check) {
  
  print(paste0("Now doing ", col_name))

  deliveries[numbaby >= 3 |
               
               (
                 (!is.na(get(paste0(col_name, 9))) | !is.na(get(paste0(col_name, 8))) |
                    !is.na(get(paste0(col_name, 7))) | !is.na(get(paste0(col_name, 6))) |
                    !is.na(get(paste0(col_name, 5))) | !is.na(get(paste0(col_name, 4))) |
                    !is.na(get(paste0(col_name, 3)))) &
                   
                   !is.na(get(paste0(col_name, 2))) & !is.na(get(paste0(col_name, 1)))
               ),
             
             multiples := 3]
  
}

print("Diagnosis codes")
deliveries[, multiples := ifelse(apply(deliveries[, col_diag, with = F], 1, function(x) any(x %in% diag_multiple)) == T,
                                 3, multiples)]

rm(col_diag, diag_singleton, diag_twin, diag_multiple, col_name, cols_to_check)


print("Scoring completeness of maternity tail")

deliveries[, mat_tail_complete :=
             as.integer(!is.na(delprean)) + as.integer(!is.na(delposan)) + as.integer(!is.na(antedur)) +
             as.integer(!is.na(birordr_1)) + as.integer(!is.na(birordr_2)) + as.integer(!is.na(birordr_3)) +
             as.integer(!is.na(birordr_4)) + as.integer(!is.na(birordr_5)) + as.integer(!is.na(birordr_6)) +
             as.integer(!is.na(birordr_7)) + as.integer(!is.na(birordr_8)) + as.integer(!is.na(birordr_9)) +
             as.integer(!is.na(birweit_1)) + as.integer(!is.na(birweit_2)) + as.integer(!is.na(birweit_3)) +
             as.integer(!is.na(birweit_4)) + as.integer(!is.na(birweit_5)) + as.integer(!is.na(birweit_6)) +
             as.integer(!is.na(birweit_7)) + as.integer(!is.na(birweit_8)) +  as.integer(!is.na(birweit_9)) +
             as.integer(!is.na(delchang)) +
             as.integer(!is.na(delmeth_1)) + as.integer(!is.na(delmeth_2)) + as.integer(!is.na(delmeth_3)) +
             as.integer(!is.na(delmeth_4)) + as.integer(!is.na(delmeth_5)) + as.integer(!is.na(delmeth_6)) +
             as.integer(!is.na(delmeth_7)) + as.integer(!is.na(delmeth_8)) + as.integer(!is.na(delmeth_9)) +
             as.integer(!is.na(gestat_1)) + as.integer(!is.na(gestat_2)) + as.integer(!is.na(gestat_3)) +
             as.integer(!is.na(gestat_4)) + as.integer(!is.na(gestat_5)) + as.integer(!is.na(gestat_6)) +
             as.integer(!is.na(gestat_7)) + as.integer(!is.na(gestat_8)) + as.integer(!is.na(gestat_9)) +
             as.integer(!is.na(birstat_1)) + as.integer(!is.na(birstat_2)) + as.integer(!is.na(birstat_3)) +
             as.integer(!is.na(birstat_4)) + as.integer(!is.na(birstat_5)) + as.integer(!is.na(birstat_6)) +
             as.integer(!is.na(birstat_7)) + as.integer(!is.na(birstat_8)) + as.integer(!is.na(birstat_9)) +
             as.integer(!is.na(biresus_1)) + as.integer(!is.na(biresus_2)) + as.integer(!is.na(biresus_3)) +
             as.integer(!is.na(biresus_4)) + as.integer(!is.na(biresus_5)) + as.integer(!is.na(biresus_6)) +
             as.integer(!is.na(biresus_7)) + as.integer(!is.na(biresus_8)) + as.integer(!is.na(biresus_9)) +
             as.integer(!is.na(sexbaby_1)) + as.integer(!is.na(sexbaby_2)) + as.integer(!is.na(sexbaby_3)) +
             as.integer(!is.na(sexbaby_4)) + as.integer(!is.na(sexbaby_5)) + as.integer(!is.na(sexbaby_6)) +
             as.integer(!is.na(sexbaby_7)) + as.integer(!is.na(sexbaby_8)) + as.integer(!is.na(sexbaby_9)) +
             as.integer(!is.na(delstat_1)) + as.integer(!is.na(delstat_2)) + as.integer(!is.na(delstat_3)) +
             as.integer(!is.na(delstat_4)) + as.integer(!is.na(delstat_5)) + as.integer(!is.na(delstat_6)) +
             as.integer(!is.na(delstat_7)) + as.integer(!is.na(delstat_8)) + as.integer(!is.na(delstat_9)) +
             as.integer(!is.na(delinten)) + as.integer(!is.na(anagest)) + as.integer(!is.na(delonset)) +
             as.integer(!is.na(matage)) + as.integer(!is.na(numbaby)) + as.integer(!is.na(numpreg)) +
             as.integer(!is.na(postdur)) + as.integer(!is.na(neocare)) + as.integer(!is.na(anasdate))]


print("Counting delivery procedures")

proc_codes <- paste0("R", c(17:25, 27))
proc_cols <- paste0("opertn_", sprintf("%02d", 1:24))

deliveries[, n_delivery_procedures := apply(deliveries[, proc_cols, with = F], 1,
                                            function(x) sum(substr(x, 1, 3) %in% proc_codes))]

rm(proc_codes, proc_cols)

print("Ordering")
deliveries <- deliveries[order(tokenid, epistart)]


print("Saving")
save(deliveries, file = "processed/tmp_deliveries_03.rda")

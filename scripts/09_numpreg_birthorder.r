
print("Cleaning numpreg")

deliveries_processed[, numpreg_clean := numpreg]
deliveries_processed[is.na(numpreg_clean), numpreg_clean := delivery_n - 1]

procodes_to_shift <- fread("numpreg_cleaning/procodes_to_change.csv", stringsAsFactors = F)
procodes_to_shift[, procode_year := paste0(procode, "-", year)]

deliveries_processed[, tmp_c_year := format(epistart, format = "%Y")]
deliveries_processed[, tmp_procode := substr(procode, 1, 3)]
deliveries_processed[, tmp_procode_year := paste0(tmp_procode, "-", tmp_c_year)]

deliveries_processed[tmp_procode_year %in% procodes_to_shift$procode_year & numpreg_clean > 0, numpreg_clean := numpreg_clean - 1]

deliveries_processed[, numpreg := NULL]
deliveries_processed[, tmp_c_year := NULL]
deliveries_processed[, tmp_procode := NULL]
deliveries_processed[, tmp_procode_year := NULL]
rm(procodes_to_shift)


print("Creating first live birth flag")
deliveries_processed[, first_live_birth := delivery_n == 1 & birth_status %in% c("all_liveborn", "some_liveborn") & numpreg_clean == 0]


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_09.rda")

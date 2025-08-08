
print("Tidying main dataset")
deliveries_processed <- deliveries_processed[order(tokenid, delivery_date)]
setnames(deliveries_processed, "preterm", "any_preterm")

tmp <- names(deliveries_processed)[grepl("_[1-9]$", names(deliveries_processed))]
tmp <- tmp[!(grepl("cause_of", tmp))]

new_order <-
  c("tokenid",
    "epikey",
    "dob_full",
    "startage",
    "ethnos",
    "rescty",
    "resgor",
    "resha",
    "resladst",
    "resro",
    "delivery_date",
    "delivery_n",
    "numbaby_clean",
    "numbaby_cleaning_rule",
    "numpreg_clean",
    "birth_status",
    "birth_status_conflict",
    "first_live_birth",
    names(deliveries_processed)[grepl("3yrs_prior", names(deliveries_processed))],
    "teenage_mother",
    "teenage_mother_ever",
    "any_preterm",
    "dod_ons",
    "dod_apc",
    "dod_combined",
    names(deliveries_processed)[grepl("cause_of_death", names(deliveries_processed))],
    tmp)

remaining_vars <-
  names(deliveries_processed)[!(names(deliveries_processed) %in% new_order)]

remaining_vars <-
  remaining_vars[!(remaining_vars %in% c("abortion",
                                         "hes_activity_after_dod",
                                         "not_england",
                                         "exclude",
                                         "min_epistart",
                                         "epistart_fyear_starting",
                                         "matage",
                                         "epistat",
                                         "sex"))]

new_order <- c(new_order, remaining_vars)
deliveries_processed_reordered <- deliveries_processed[, new_order, with = F]
rm(new_order, remaining_vars, tmp)

print("Saving")
fwrite(
  deliveries_processed_reordered,
  file = "processed/deliveries_cohort_full.csv",
  row.names = F
)



print("APC record")

diagnoses_long <- diagnoses_long[tokenid %in% deliveries_processed_reordered$tokenid]
operations_long <- operations_long[tokenid %in% deliveries_processed_reordered$tokenid]

fwrite(
  diagnoses_long,
  file = "processed/diagnoses_long.csv",
  row.names = F
)

fwrite(
  operations_long,
  file = "processed/operations_long.csv",
  row.names = F
)



print("Saving data dictionary template")
write.csv(
  names(deliveries_processed_reordered),
  file = "processed/data_dictionary.csv",
  row.names = F
)


print("Removing temporary files")
files_to_remove <- list.files("processed/")
files_to_remove <- files_to_remove[grepl(".rda", files_to_remove)]
for (file in files_to_remove) { file.remove(paste0("processed/", file)) }


print("Cleaning memory")
rm(list = ls())


print("Disconnecting from database")
dbDisconnect(sql_channel)

print(" ------ ENMMOI ------ ")

enmmoi_codes <- fread("codelists/enmmoi_nair_v1.csv", stringsAsFactors = F)
enmmoi_codes[, code := gsub("\\.", "", code)]

diagnoses_long[, enmmoi := F]
diagnoses_long[substr(code, 1, 3) %in% enmmoi_codes[nchar(code) == 3 & code_type == "icd10"]$code, enmmoi := T]
diagnoses_long[substr(code, 1, 4) %in% enmmoi_codes[nchar(code) == 4 & code_type == "icd10"]$code, enmmoi := T]
diagnoses_enmmoi <- diagnoses_long[enmmoi == T]

diagnoses_long[, enmmoi := NULL]
diagnoses_enmmoi[, enmmoi := NULL]

operations_long[, enmmoi := F]
operations_long[substr(code, 1, 3) %in% enmmoi_codes[nchar(code) == 3 & code_type == "opcs4"]$code, enmmoi := T]
operations_long[substr(code, 1, 4) %in% enmmoi_codes[nchar(code) == 4 & code_type == "opcs4"]$code, enmmoi := T]
operations_enmmoi <- operations_long[enmmoi == T]

operations_long[, enmmoi := NULL]
operations_enmmoi[, enmmoi := NULL]


print("Identifying diagnoses in relevant window (diagnoses)")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  new_col <- paste0("enmmoi_", years, "yr_prior_del_", i)
  del_col <- paste0("del_", i)
  diagnoses_enmmoi[!is.na(get(del_col)), (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col)]
  
}

rm(i, new_col, del_col)

diagnoses_enmmoi <- diagnoses_enmmoi[, c("tokenid", names(diagnoses_enmmoi)[grepl("_prior_del_", names(diagnoses_enmmoi))]), with = F]
diagnoses_enmmoi <- diagnoses_enmmoi[!duplicated(diagnoses_enmmoi)]

diagnoses_enmmoi <-
  melt(
    diagnoses_enmmoi,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("enmmoi_", years, "yrs_prior_del")
  )

diagnoses_enmmoi <- diagnoses_enmmoi[!is.na(get(paste0("enmmoi_", years, "yrs_prior_del")))]
diagnoses_enmmoi[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_enmmoi <- diagnoses_enmmoi[get(paste0("enmmoi_", years, "yrs_prior_del")) == T]
diagnoses_enmmoi <- diagnoses_enmmoi[!duplicated(diagnoses_enmmoi)]


print("Identifying operations in relevant window (operations)")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  new_col <- paste0("enmmoi_", years, "yr_prior_del_", i)
  del_col <- paste0("del_", i)
  operations_enmmoi[!is.na(get(del_col)), (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col)]
  
}

rm(i, new_col, del_col)

operations_enmmoi <- operations_enmmoi[, c("tokenid", names(operations_enmmoi)[grepl("_prior_del_", names(operations_enmmoi))]), with = F]
operations_enmmoi <- operations_enmmoi[!duplicated(operations_enmmoi)]

operations_enmmoi <-
  melt(
    operations_enmmoi,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("enmmoi_", years, "yrs_prior_del")
  )

operations_enmmoi <- operations_enmmoi[!is.na(get(paste0("enmmoi_", years, "yrs_prior_del")))]
operations_enmmoi[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
operations_enmmoi <- operations_enmmoi[get(paste0("enmmoi_", years, "yrs_prior_del")) == T]
operations_enmmoi <- operations_enmmoi[!duplicated(operations_enmmoi)]


print("Merging flags into deliveries data")
diag_op_comb <- rbind(diagnoses_enmmoi, operations_enmmoi)
diag_op_comb <- diag_op_comb[!duplicated(diag_op_comb)]

deliveries_processed <-
  merge(
    deliveries_processed,
    diag_op_comb,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed[is.na(get(paste0("enmmoi_", years, "yrs_prior_del"))), (paste0("enmmoi_", years, "yrs_prior_del")) := FALSE]


print("Removing temporary data")
rm(enmmoi_codes, diagnoses_enmmoi, operations_enmmoi, diag_op_comb)
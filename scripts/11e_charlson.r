print(" ------ Charlson ------")

charlson_codes <- fread("codelists/charlson_quan_v1.csv", stringsAsFactors = F)
charlson_codes[, code := gsub("\\.", "", code)]

diagnoses_long[, charlson := F]
diagnoses_long[substr(code, 1, 3) %in% charlson_codes[nchar(code) == 3]$code, charlson := T]
diagnoses_long[substr(code, 1, 4) %in% charlson_codes[nchar(code) == 4]$code, charlson := T]
diagnoses_charlson <- diagnoses_long[charlson == T]

diagnoses_long[, charlson := NULL]
diagnoses_charlson[, charlson := NULL]


print("Identifying diagnoses in relevant window")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  new_col <- paste0("charlson_", years, "yr_prior_del_", i)
  del_col <- paste0("del_", i)
  diagnoses_charlson[!is.na(get(del_col)), (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col)]
  
}

rm(i, new_col, del_col)

diagnoses_charlson <- diagnoses_charlson[, c("tokenid", names(diagnoses_charlson)[grepl("_prior_del_", names(diagnoses_charlson))]), with = F]
diagnoses_charlson <- diagnoses_charlson[!duplicated(diagnoses_charlson)]

diagnoses_charlson <-
  melt(
    diagnoses_charlson,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("charlson_", years, "yrs_prior_del")
  )

diagnoses_charlson <- diagnoses_charlson[!is.na(get(paste0("charlson_", years, "yrs_prior_del")))]
diagnoses_charlson[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_charlson <- diagnoses_charlson[get(paste0("charlson_", years, "yrs_prior_del")) == T]
diagnoses_charlson <- diagnoses_charlson[!duplicated(diagnoses_charlson)]


print("Merging flags into deliveries data")
deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_charlson,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed[is.na(get(paste0("charlson_", years, "yrs_prior_del"))), (paste0("charlson_", years, "yrs_prior_del")) := FALSE]


print("Removing temporary data")
rm(charlson_codes, diagnoses_charlson)
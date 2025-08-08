print("high_risk_preg")

high_risk_preg_codes <- fread("codelists/high_risk_preg_langham_v1.csv", stringsAsFactors = F)
high_risk_preg_codes[, code := gsub("\\.", "", code)]

diagnoses_long[, high_risk_preg := F]
diagnoses_long[substr(code, 1, 3) %in% high_risk_preg_codes[nchar(code) == 3]$code, high_risk_preg := T]
diagnoses_long[substr(code, 1, 4) %in% high_risk_preg_codes[nchar(code) == 4]$code, high_risk_preg := T]
diagnoses_high_risk_preg <- diagnoses_long[high_risk_preg == T]

diagnoses_long[, high_risk_preg := NULL]
diagnoses_high_risk_preg[, high_risk_preg := NULL]


print("Identifying diagnoses in relevant window")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  new_col <- paste0("high_risk_preg_", years, "yr_prior_del_", i)
  del_col <- paste0("del_", i)
  diagnoses_high_risk_preg[!is.na(get(del_col)), (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col)]
  
}

rm(i, new_col, del_col)

diagnoses_high_risk_preg <- diagnoses_high_risk_preg[, c("tokenid", names(diagnoses_high_risk_preg)[grepl("_prior_del_", names(diagnoses_high_risk_preg))]), with = F]
diagnoses_high_risk_preg <- diagnoses_high_risk_preg[!duplicated(diagnoses_high_risk_preg)]

diagnoses_high_risk_preg <-
  melt(
    diagnoses_high_risk_preg,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("high_risk_preg_", years, "yrs_prior_del")
  )

diagnoses_high_risk_preg <- diagnoses_high_risk_preg[!is.na(get(paste0("high_risk_preg_", years, "yrs_prior_del")))]
diagnoses_high_risk_preg[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_high_risk_preg <- diagnoses_high_risk_preg[get(paste0("high_risk_preg_", years, "yrs_prior_del")) == T]
diagnoses_high_risk_preg <- diagnoses_high_risk_preg[!duplicated(diagnoses_high_risk_preg)]


print("Merging flags into deliveries data")
deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_high_risk_preg,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed[is.na(get(paste0("high_risk_preg_", years, "yrs_prior_del"))), (paste0("high_risk_preg_", years, "yrs_prior_del")) := FALSE]

print("Removing temporary data")
rm(high_risk_preg_codes, diagnoses_high_risk_preg)
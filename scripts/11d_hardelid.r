print(" ------ Hardelid ------")

hardelid <- fread("codelists/chc_hardelid_v1.csv", stringsAsFactors = F)
hardelid[, code := gsub("\\.", "", code)]

diagnoses_long[, chc_hardelid := F]
diagnoses_long[, diag_for_link := code]

diagnoses_long[substr(code, 1, 3) %in% hardelid[nchar(code) == 3]$code, chc_hardelid := T]
diagnoses_long[substr(code, 1, 3) %in% hardelid[nchar(code) == 3]$code, diag_for_link := substr(code, 1, 3)]

diagnoses_long[substr(code, 1, 4) %in% hardelid[nchar(code) == 4]$code, chc_hardelid := T]
diagnoses_long[substr(code, 1, 4) %in% hardelid[nchar(code) == 4]$code, diag_for_link := substr(code, 1, 4)]

diagnoses_hardelid <- diagnoses_long[chc_hardelid == T]

diagnoses_long[, chc_hardelid := NULL]
diagnoses_long[, diag_for_link := NULL]

diagnoses_hardelid <-
  merge(
    diagnoses_hardelid,
    hardelid[, c("code", "flag", "group", "subgroup")],
    by.x = "diag_for_link",
    by.y = "code",
    all.x = T
  )

diagnoses_hardelid[, chc_hardelid := NULL]


print("Dealing with flags")
diagnoses_hardelid[, drop := F]
diagnoses_hardelid[flag == "age10" & (startage < 10 | startage >= 7001), drop := T]
diagnoses_hardelid[flag == "los3" & admi_los_nights < 3, drop := T]
diagnoses_hardelid <- diagnoses_hardelid[drop == F]
diagnoses_hardelid[, drop := NULL]


print("Identifying diagnoses in relevant window")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  new_col <- paste0("any_hardelid_", years, "yr_prior_del_", i)
  del_col <- paste0("del_", i)
  diagnoses_hardelid[!is.na(get(del_col)), (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col)]
  
}

rm(i, new_col, del_col)

diagnoses_hardelid <- diagnoses_hardelid[, c("tokenid", names(diagnoses_hardelid)[grepl("_prior_del_", names(diagnoses_hardelid))]), with = F]
diagnoses_hardelid <- diagnoses_hardelid[!duplicated(diagnoses_hardelid)]

diagnoses_hardelid <-
  melt(
    diagnoses_hardelid,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("chc_hardelid_any_", years, "yrs_prior_del")
  )

diagnoses_hardelid <- diagnoses_hardelid[!is.na(get(paste0("chc_hardelid_any_", years, "yrs_prior_del")))]
diagnoses_hardelid[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_hardelid <- diagnoses_hardelid[get(paste0("chc_hardelid_any_", years, "yrs_prior_del")) == T]
diagnoses_hardelid <- diagnoses_hardelid[!duplicated(diagnoses_hardelid)]


print("Merging flags into deliveries data")
deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_hardelid,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed[is.na(get(paste0("chc_hardelid_any_", years, "yrs_prior_del"))), (paste0("chc_hardelid_any_", years, "yrs_prior_del")) := FALSE]

print("Removing temporary data")
rm(hardelid, diagnoses_hardelid)

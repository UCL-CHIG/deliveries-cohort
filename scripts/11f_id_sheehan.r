print(" ------ Sheearn intellectual disability ------ ")

int_dis_codes <- fread("codelists/id_sheehan_v1.csv", stringsAsFactors = F)
int_dis_codes[, code := gsub("\\.", "", code)]

diagnoses_long[, int_dis := F]
diagnoses_long[substr(code, 1, 3) %in% int_dis_codes[nchar(code) == 3]$code, int_dis := T]
diagnoses_long[substr(code, 1, 4) %in% int_dis_codes[nchar(code) == 4]$code, int_dis := T]
diagnoses_int_dis <- diagnoses_long[int_dis == T]

diagnoses_long[, int_dis := NULL]
diagnoses_int_dis[, int_dis := NULL]


print("Identifying diagnoses in relevant window")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  new_col <- paste0("int_dis_", years, "yr_prior_del_", i)
  del_col <- paste0("del_", i)
  diagnoses_int_dis[!is.na(get(del_col)), (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col)]
  
}

rm(i, new_col, del_col)

diagnoses_int_dis <- diagnoses_int_dis[, c("tokenid", names(diagnoses_int_dis)[grepl("_prior_del_", names(diagnoses_int_dis))]), with = F]
diagnoses_int_dis <- diagnoses_int_dis[!duplicated(diagnoses_int_dis)]

diagnoses_int_dis <-
  melt(
    diagnoses_int_dis,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("int_dis_", years, "yrs_prior_del")
  )

diagnoses_int_dis <- diagnoses_int_dis[!is.na(get(paste0("int_dis_", years, "yrs_prior_del")))]
diagnoses_int_dis[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_int_dis <- diagnoses_int_dis[get(paste0("int_dis_", years, "yrs_prior_del")) == T]
diagnoses_int_dis <- diagnoses_int_dis[!duplicated(diagnoses_int_dis)]


print("Merging flags into deliveries data")
deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_int_dis,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed[is.na(get(paste0("int_dis_", years, "yrs_prior_del"))), (paste0("int_dis_", years, "yrs_prior_del")) := FALSE]

print("Removing temporary data")
rm(int_dis_codes, diagnoses_int_dis)

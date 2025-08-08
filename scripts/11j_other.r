
print(" ------ Other phenotypes ------")
diag_cols <- names(deliveries_processed)[grepl("diag_", names(deliveries_processed))]
gestat_cols <- names(deliveries_processed)[grepl("gestat_", names(deliveries_processed))]


print("-- Abortions --")
abortion_codes <- fread("codelists/abortions.csv", stringsAsFactors = F)
abortion_codes[, code := gsub("\\.", "", code)]

tmp <-
  melt(
    deliveries_processed[, c("epikey", diag_cols), with = F],
    id.vars = "epikey"
  )

tmp <- tmp[!is.na(value)]
tmp <- tmp[order(epikey, variable)]
tmp <- tmp[substr(value, 1, 3) %in% abortion_codes[nchar(code) == 3]$code |
             substr(value, 1, 4) %in% abortion_codes[nchar(code) == 4]$code]

tmp[, abortion := T]
tmp <- tmp[, c("epikey", "abortion")]
tmp <- tmp[!duplicated(tmp)]

deliveries_processed <-
  merge(
    deliveries_processed,
    tmp,
    by = "epikey",
    all.x = T
  )

rm(tmp, abortion_codes)

deliveries_processed[is.na(abortion), abortion := F]


print("-- Pre-term births --")
print("With ICD-10 codes")
preterm_codes <- fread("codelists/preterm.csv", stringsAsFactors = F)
preterm_codes[, code := gsub("\\.", "", code)]

tmp <-
  melt(
    deliveries_processed[, c("epikey", diag_cols), with = F],
    id.vars = "epikey"
  )

tmp <- tmp[!is.na(value)]
tmp <- tmp[order(epikey, variable)]
tmp <- tmp[substr(value, 1, 3) %in% preterm_codes[nchar(code) == 3]$code |
             substr(value, 1, 4) %in% preterm_codes[nchar(code) == 4]$code]

tmp[, preterm := T]
tmp <- tmp[, c("epikey", "preterm")]
tmp <- tmp[!duplicated(tmp)]

deliveries_processed <-
  merge(
    deliveries_processed,
    tmp,
    by = "epikey",
    all.x = T
  )

rm(tmp, preterm_codes)


print("With gestat")
tmp <-
  melt(
    deliveries_processed[, c("epikey", gestat_cols), with = F],
    id.vars = "epikey"
  )

tmp <- tmp[order(epikey, variable)]
tmp[, all_gestat_na := all(is.na(value)), by = "epikey"]
tmp <- tmp[all_gestat_na == F]
tmp[, all_gestat_na := NULL]

tmp[, any_gestat_preterm := any(value < 37, na.rm = T), by = epikey]

tmp <- tmp[any_gestat_preterm == T, c("epikey", "any_gestat_preterm")]
tmp <- tmp[!duplicated(tmp)]

deliveries_processed <-
  merge(
    deliveries_processed,
    tmp,
    by = "epikey",
    all.x = T
  )

deliveries_processed[any_gestat_preterm == T, preterm := T]
deliveries_processed[is.na(preterm), preterm := F]
deliveries_processed[, any_gestat_preterm := NULL]

rm(tmp)


print("-- Teenage mothers --")
deliveries_processed[startage < 20, teenage_mother := T]
deliveries_processed[is.na(teenage_mother), teenage_mother := F]
deliveries_processed[, teenage_mother_ever := max(teenage_mother) == 1, by = tokenid]

rm(gestat_cols, diag_cols)

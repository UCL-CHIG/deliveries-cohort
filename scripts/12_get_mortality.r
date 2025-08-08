
print("Extracting ONS mortality data")

print("Re-establishing SQL server connection")
sql_channel <-
  dbConnect(
    RMySQL::MySQL(),
    username = "[omitted]",
    password = .rs.askForPassword("log in CPRU SQL server"),
    host = "[omitted]",
    port = omitted,
    dbname = "[omitted]"
  )

ons_mort_data <-
  data.table(
    dbGetQuery(
      sql_channel,
      paste0(
        "select tokenid, record_id, ",
        "dod, dor, cause_of_death, subsequent_activity, ",
        paste0("cause_of_death_non_neonatal_", 1:15, collapse = ", "), " ",
        "from ons_full_2022"
      )
    )
  )

dbDisconnect(sql_channel)

ons_mort_data <- ons_mort_data[tokenid %in% deliveries_processed$tokenid]
setnames(ons_mort_data, "subsequent_activity", "hes_activity_after_dod")
ons_mort_data[hes_activity_after_dod == "Y", hes_activity_after_dod := 1]
ons_mort_data[, hes_activity_after_dod := as.integer(hes_activity_after_dod)]

ons_mort_data[, dod := as.Date(dod, format = "%Y-%m-%d")]
apc_mortality[, dod_apc := as.Date(dod_apc, format = "%Y-%m-%d")]
apc_latest[, latest_disdate := as.Date(latest_disdate, format = "%Y-%m-%d")]

print("Merging")
deliveries_processed <-
  merge(
    deliveries_processed,
    ons_mort_data[, -c("dor", "record_id")],
    by = "tokenid",
    all.x = T
  )

deliveries_processed <-
  merge(
    deliveries_processed,
    apc_mortality[, c("tokenid", "dod_apc")],
    by = "tokenid",
    all.x = T
  )

deliveries_processed <-
  merge(
    deliveries_processed,
    apc_latest,
    by = "tokenid",
    all.x = T
  )


print("Creating dod variables")
deliveries_processed[, dod_combined := dod]
deliveries_processed[is.na(dod_combined) & !is.na(dod_apc), dod_combined := dod_apc]
deliveries_processed[latest_disdate > dod_combined, hes_activity_after_dod := 1]

deliveries_processed[, latest_disdate := NULL]
setnames(deliveries_processed, "dod", "dod_ons")

rm(apc_latest, apc_mortality, ons_mort_data)


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_12.rda")

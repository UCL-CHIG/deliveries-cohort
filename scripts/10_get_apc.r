
get_hes_data <- function(financial_years_starting) {
  
  tables <- dbListTables(sql_channel)
  tables <- tables[grepl("APC_[0-9]{4}$", tables)]
  
  years <-
    paste0(
      substr(financial_years_starting, 3, 4),
      substr(financial_years_starting + 1, 3, 4),
      collapse = "|"
    )
  
  tables <- tables[grepl(years, tables)]
  
  years <-
    paste0(
      substr(financial_years_starting, 3, 4),
      substr(financial_years_starting + 1, 3, 4)
    )
  
  tables <- tables[match(years, substr(tables, 5, 9))]
  
  hes_source <- data.table()
  
  for (table_name in tables) {
    
    print(paste0("Now doing table: ", table_name))
    
    temp <-
      data.table(
        dbGetQuery(
          sql_channel,
          paste0(
            "SELECT tokenid, ",
            "epistart, epiend, startage, admidate, disdate, ",
            "admimeth, dismeth, disdest, ",
            paste0("diag_", sprintf("%02d", 1:20), collapse = ", "), ", ",
            paste0("opertn_", sprintf("%02d", 1:24), collapse = ", "), " ",
            "FROM ", table_name, " ",
            "WHERE sex = 2 and startage > 11 and startage < 80"
          )
        )
      )
    
    #temp <- temp[tokenid %in% deliveries_processed$tokenid]
    hes_source <- rbind(hes_source, temp)
    
  }
  
  return(hes_source)
  
}

print("Extracting APC record")

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

apc <- get_hes_data(1997:2022)
apc <- apc[tokenid %in% deliveries_processed$tokenid]


print("Cleaning dates and deduplicating")
apc <- clean_hes_dates(apc)
apc <- apc[!duplicated(apc)]


print("Calculating admission length of stay")
apc[, admi_los_nights := as.integer(difftime(disdate, admidate, units = "days"))]


print("Separating out diagnoses")
print("Reshaping")
diagnoses_long <-
  melt(
    apc[, c("tokenid",
            "admidate",
            "epistart",
            "startage",
            "admimeth",
            "admi_los_nights",
            names(apc)[grepl("diag_", names(apc))]),
        with = F],
    id.vars = c("tokenid",
                "admidate",
                "epistart",
                "startage",
                "admimeth",
                "admi_los_nights"),
    variable.name = "field",
    value.name = "code"
  )

print("Cleaning diagnoses")
diagnoses_long <- diagnoses_long[code != "" & !is.na(code)]
diagnoses_long[, code_no := as.integer(gsub("[a-z]*._", "", field))]
diagnoses_long[, field := NULL]
diagnoses_long[, code := substr(code, 1, 4)]


print("Separating out operations")
print("Reshaping")
operations_long <-
  melt(
    apc[, c("tokenid",
            "admidate",
            "epistart",
            "startage",
            "admimeth",
            "admi_los_nights",
            names(apc)[grepl("opertn_", names(apc))]),
        with = F],
    id.vars = c("tokenid",
                "admidate",
                "epistart",
                "startage",
                "admimeth",
                "admi_los_nights"),
    variable.name = "field",
    value.name = "code"
  )


print("Cleaning operations")
operations_long <- operations_long[code != "" & !is.na(code)]
operations_long[, code_no := as.integer(gsub("[a-z]*._", "", field))]
operations_long[, field := NULL]
operations_long[, code := substr(code, 1, 4)]


print("Deduplicating diagnoses and operations")
diagnoses_long <- diagnoses_long[!duplicated(diagnoses_long)]
operations_long <- operations_long[!duplicated(operations_long)]


print("Separating out mortality discharges")
apc_mortality <- apc[dismeth == 4 | disdest == 79, c("tokenid", "disdate"), with = F]
apc_mortality <- apc_mortality[, c("tokenid", "disdate")]
setnames(apc_mortality, "disdate", "dod_apc")
apc_mortality[, dod_apc := max(dod_apc), by = tokenid]
apc_mortality <- apc_mortality[!duplicated(apc_mortality)]


print("Seperating out latest APC activity")
apc_latest <- apc[, c("tokenid", "disdate")]
apc_latest[, latest_disdate := max(disdate), by = tokenid]
apc_latest <- apc_latest[, c("tokenid", "latest_disdate")]
apc_latest <- apc_latest[!duplicated(apc_latest)]


print("Saving")

print("Diagnoses")
save(diagnoses_long, file = "processed/diagnoses_long.rda")

print("Operations")
save(operations_long, file = "processed/operations_long.rda")

print("APC mortality")
save(apc_mortality, file = "processed/apc_mortality.rda")

print("APC latest")
save(apc_latest, file = "processed/apc_latest.rda")


rm(apc)
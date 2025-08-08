
print("Geography")

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
            "SELECT tokenid, epikey, ",
            "rescty, resgor, resha, resladst, resro ",
            "FROM ", table_name, " ",
            "WHERE sex = 2 and startage > 11 and startage < 51"
          )
        )
      )
    
    #temp <- temp[epikey %in% deliveries_processed$epikey]
    hes_source <- rbind(hes_source, temp)
    
  }
  
  return(hes_source)
  
}


print("Extracting geography data")

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

geo_data <- get_hes_data(1997:2022)
geo_data <- geo_data[epikey %in% deliveries_processed$epikey]
dbDisconnect(sql_channel)


print("Processing")

geo_data <- geo_data[!(is.na(rescty) & is.na(resgor) & is.na(resha) & is.na(resladst) & is.na(resro))]

geo_data[, rescty := toupper(rescty)]
geo_data[, resgor := toupper(resgor)]
geo_data[, resha := toupper(resha)]
geo_data[, resladst := toupper(resladst)]
geo_data[, resro := toupper(resro)]

non_england_codes <- c("S", "W", "X", "Z",
                       "S00", "W00", "X00", "Z00")

geo_data[, not_england :=
           rescty %in% non_england_codes |
           resgor %in% non_england_codes |
           resha %in% non_england_codes |
           resladst %in% non_england_codes |
           resro %in% non_england_codes]

geo_data <- geo_data[, -c("tokenid")]


print("Merging")
deliveries_processed <-
  merge(
    deliveries_processed,
    geo_data,
    by = "epikey",
    all.x = T
  )

rm(geo_data, non_england_codes)

deliveries_processed[is.na(not_england), not_england := F]


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_13.rda")
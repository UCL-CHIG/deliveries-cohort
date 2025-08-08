
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
            "SELECT tokenid, ethnos ",
            "FROM ", table_name, " ",
            "WHERE sex = 2 and startage > 11 and startage < 80 ",
            "AND ethnos NOT IN ('X', 'Z', '9', '99')"
          )
        )
      )
    
    temp <- temp[tokenid %in% deliveries_processed$tokenid]
    hes_source <- rbind(hes_source, temp)
    
  }
  
  return(hes_source)
  
}


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


print("Extracting ethnicity data")

eth_data <- get_hes_data(cohort_years)
dbDisconnect(sql_channel)

eth_data <- eth_data[order(tokenid, ethnos)]


print("Separating old and new codings")

eth_data_old <- eth_data[ethnos %in% 0:8]
eth_data_new <- eth_data[ethnos %in% LETTERS]
eth_data_old <- eth_data_old[!(tokenid %in% eth_data_new$tokenid)]


print("Identifying modal value")

set.seed(753)
eth_data_old[, ethnos := mode_fun(ethnos), by = tokenid]
eth_data_new[, ethnos := mode_fun(ethnos), by = tokenid]

eth_data_old <- eth_data_old[!duplicated(eth_data_old)]
eth_data_new <- eth_data_new[!duplicated(eth_data_new)]


print("Recombining")

eth_data <-
  rbind(
    eth_data_old,
    eth_data_new
  )


print("Merging into deliveries")

deliveries_processed <-
  merge(
    deliveries_processed,
    eth_data,
    by = "tokenid",
    all.x = T
  )

rm(eth_data, eth_data_old, eth_data_new, get_hes_data)


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_06.rda")

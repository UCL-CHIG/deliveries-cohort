
get_hes_data <- function(financial_years_starting) {

  oprtn_codes <-
    paste0("'R", c(14:15, 17:25, 27), "'", collapse = ", ")

  diag_codes <-
    paste0("'Z", c(37:38), "'", collapse = ", ")

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

            "SELECT tokenid, epikey, sex, startage, mydob, dob_full, ",
            "admidate, disdate, epistart, epiend, admimeth, ",
            "epistat, procode, ",
            paste0("diag_", sprintf("%02d", 1:20), collapse = ", "), ", ",
            paste0("opertn_", sprintf("%02d", 1:24), collapse = ", "), ", ",
            paste0("opdate_", sprintf("%02d", 1:24), collapse = ", "), ", ",
            "neocare, numpreg, numbaby, ",
            "anagest, anasdate, antedur, ",
            paste0("biresus_", 1:9, collapse = ", "), ", ",
            paste0("birordr_", 1:9, collapse = ", "), ", ",
            paste0("birstat_", 1:9, collapse = ", "), ", ",
            paste0("birweit_", 1:9, collapse = ", "), ", ",
            "delchang, delinten, ",
            paste0("delmeth_", 1:9, collapse = ", "), ", ",
            "delonset, delposan, delprean, ",
            paste0("delstat_", 1:9, collapse = ", "), ", ",
            paste0("gestat_", 1:9, collapse = ", "), ", ",
            "matage, postdur, ",
            paste0("sexbaby_", 1:9, collapse = ", "), " ",

            "FROM ", table_name, " ",

            "WHERE sex = 2 ",
            "AND startage > 11 AND startage < 51 ",
            "AND (", paste0("substring(opertn_", sprintf("%02d", 1:24), ", 1, 3) IN (", oprtn_codes, ")", collapse = " or "), " ",
            "or ", paste0("substring(diag_", sprintf("%02d", 1:20), ", 1, 3) IN (", diag_codes, ")", collapse = " or "), " ",
            "or delmeth_1 IN (", paste0("'", 0:9, "'", collapse = ", "), ")) ",
            "AND epiend IS NOT NULL ",
            "AND epistat != 1 ",
            "AND procode NOT IN ('N', '8') ",
            "AND (gestat_1 > 23 OR gestat_1 is null) ",
            "AND ((anagest is not null) + (anasdate is not null) + (antedur is not null) + ",
            "(biresus_1 is not null) + (birordr_1 is not null) + (birstat_1 is not null) + (birweit_1 is not null) + ",
            "(delchang is not null) + (delinten is not null) + (delmeth_1 is not null) + (delonset is not null) + ",
            "(delposan is not null) + (delprean is not null) + (delstat_1  is not null) + ",
            "(gestat_1 is not null) + (matage  is not null) + (postdur  is not null) + (sexbaby_1 is not null) >= 2)"

          )
        )
      )

    temp <- temp[!duplicated(temp)]
    hes_source <- rbind(hes_source, temp)

  }

  return(hes_source)

}

print("Extracting deliveries")

deliveries <- get_hes_data(cohort_years)
dbDisconnect(sql_channel)


print("Saving")
save(deliveries, file = "processed/tmp_deliveries_01.rda")
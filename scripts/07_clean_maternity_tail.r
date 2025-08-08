
deliveries_processed[, numbaby := as.integer(numbaby)]
deliveries_processed[, numbaby_clean := as.integer(NA)]
deliveries_processed[, numbaby_cleaning_rule := as.double(NA)]


print("Identifying conflicting diagnoses")

col_diag <- paste0("diag_", sprintf("%02d", 1:20))
diag_singleton <- c("Z370", "Z371", "Z380", "Z381", "Z382")
diag_twin <- c("Z372", "Z373", "Z374", "Z383", "Z384", "Z385")
diag_multiple <- c("Z375", "Z376", "Z377", "Z386", "Z387", "Z388")

deliveries_processed[, any_diag_singleton := apply(deliveries_processed[, col_diag, with = F], 1, function(x) any(x %in% diag_singleton))]
deliveries_processed[, any_diag_twin := apply(deliveries_processed[, col_diag, with = F], 1, function(x) any(x %in% diag_twin))]
deliveries_processed[, any_diag_multiple := apply(deliveries_processed[, col_diag, with = F], 1, function(x) any(x %in% diag_multiple))]

deliveries_processed[, conflict_diag := sum(any_diag_singleton, any_diag_twin, any_diag_multiple) > 1, by = 1:nrow(deliveries_processed)]


print("Identifying conflicting number of maternity tail variables")

mt_vars <- c("sexbaby_",
             "birordr_",
             "birweit_",
             "delmeth_",
             "gestat_",
             "birstat_",
             "biresus_")

for (col_name in mt_vars) {
  
  print(paste0("Now doing ", col_name))
  new_col <- paste0("count_", col_name)
  
  deliveries_processed[, (new_col) :=
                         as.integer(!is.na(get(paste0(col_name, 1)))) + 
                         as.integer(!is.na(get(paste0(col_name, 2)))) + 
                         as.integer(!is.na(get(paste0(col_name, 3)))) + 
                         as.integer(!is.na(get(paste0(col_name, 4)))) + 
                         as.integer(!is.na(get(paste0(col_name, 5)))) + 
                         as.integer(!is.na(get(paste0(col_name, 6)))) + 
                         as.integer(!is.na(get(paste0(col_name, 7)))) + 
                         as.integer(!is.na(get(paste0(col_name, 8)))) + 
                         as.integer(!is.na(get(paste0(col_name, 9))))
  ]
  
  deliveries_processed[get(new_col) == 0, (new_col) := NA]
  
}

mt_vars_count <- c("count_sexbaby_",
                   "count_birordr_",
                   "count_birweit_",
                   "count_delmeth_",
                   "count_gestat_",
                   "count_birstat_",
                   "count_biresus_")

print("Identifying conflicts")

deliveries_processed[, conflict_mt_vars := apply(deliveries_processed[, mt_vars_count, with = F], 1, function(x) length(unique(x[!is.na(x)])) > 1)]
deliveries_processed[, max_count_mt_vars := apply(deliveries_processed[, mt_vars_count, with = F], 1, function(x) max(x, na.rm = T))]
deliveries_processed[max_count_mt_vars == -Inf, max_count_mt_vars := NA]
deliveries_processed[is.na(max_count_mt_vars), conflict_mt_vars := NA]


print("Implementing rule set 1")
deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & any_diag_singleton == T) |
                          (numbaby == 2 & any_diag_twin == T) |
                          (numbaby > 2 & any_diag_multiple == T)) &
                       (conflict_mt_vars == F & numbaby == max_count_mt_vars),
                     numbaby_cleaning_rule := 1]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & any_diag_singleton == T) |
                          (numbaby == 2 & any_diag_twin == T) |
                          (numbaby > 2 & any_diag_multiple == T)) &
                       (conflict_mt_vars == F & numbaby == max_count_mt_vars),
                     numbaby_clean := numbaby]


print("Implementing rule set 2")
deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == F,
                     numbaby_cleaning_rule := 2.1]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == F,
                     numbaby_clean := max_count_mt_vars]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == T &
                       ((any_diag_singleton == T & any_diag_twin == F) | (any_diag_singleton == F & any_diag_twin == T)) & any_diag_multiple == F,
                     numbaby_cleaning_rule := 2.2]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == T &
                       any_diag_singleton == T & any_diag_twin == F & any_diag_multiple == F,
                     numbaby_clean := 1]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == T &
                       any_diag_singleton == F & any_diag_twin == T & any_diag_multiple == F,
                     numbaby_clean := 2]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == T & conflict_diag == T,
                     numbaby_cleaning_rule := 2.3]

deliveries_processed[!is.na(numbaby) &
                       ((numbaby == 1 & (any_diag_twin == T | any_diag_multiple == T)) |
                          (numbaby == 2 & (any_diag_singleton == T | any_diag_multiple == T)) |
                          (numbaby > 2 & (any_diag_singleton == T | any_diag_twin == T))) &
                       conflict_mt_vars == T & conflict_diag == T,
                     numbaby_clean := numbaby]


print("Implementing rule set 3")
deliveries_processed[!is.na(numbaby) &
                       numbaby != max_count_mt_vars & conflict_mt_vars == F &
                       conflict_mt_vars == F & conflict_diag == F &
                       ((any_diag_singleton == T & any_diag_twin == F) | (any_diag_singleton == F & any_diag_twin == T)) & any_diag_multiple == F,
                     numbaby_cleaning_rule := 3.1]

deliveries_processed[!is.na(numbaby) &
                       numbaby != max_count_mt_vars & conflict_mt_vars == F &
                       conflict_mt_vars == F & conflict_diag == F &
                       any_diag_singleton == T & any_diag_twin == F & any_diag_multiple == F,
                     numbaby_clean := 1]

deliveries_processed[!is.na(numbaby) &
                       numbaby != max_count_mt_vars & conflict_mt_vars == F &
                       conflict_mt_vars == F & conflict_diag == F &
                       any_diag_singleton == F & any_diag_twin == T & any_diag_multiple == F,
                     numbaby_clean := 2]

deliveries_processed[!is.na(numbaby) &
                       numbaby != max_count_mt_vars & conflict_mt_vars == F &
                       conflict_mt_vars == F & conflict_diag == T,
                     numbaby_cleaning_rule := 3.2]

deliveries_processed[!is.na(numbaby) &
                       numbaby != max_count_mt_vars & conflict_mt_vars == F &
                       conflict_mt_vars == F & conflict_diag == T,
                     numbaby_clean := numbaby]


print("Implementing rule set 4")
deliveries_processed[!is.na(numbaby) & is.na(numbaby_clean), numbaby_cleaning_rule := 4]
deliveries_processed[!is.na(numbaby) & is.na(numbaby_clean), numbaby_clean := numbaby]


print("Implementing rule set 5")
deliveries_processed[is.na(numbaby) & conflict_diag == F &
                       ((any_diag_singleton == T & any_diag_twin == F) | (any_diag_singleton == F & any_diag_twin == T)) & any_diag_multiple == F,
                     numbaby_cleaning_rule := 5.1]

deliveries_processed[is.na(numbaby) & conflict_diag == F &
                       any_diag_singleton == T & any_diag_twin == F & any_diag_multiple == F,
                     numbaby_clean := 1]

deliveries_processed[is.na(numbaby) & conflict_diag == F &
                       any_diag_singleton == F & any_diag_twin == T & any_diag_multiple == F,
                     numbaby_clean := 2]

deliveries_processed[is.na(numbaby) & is.na(numbaby_clean) &
                       conflict_mt_vars == F,
                     numbaby_cleaning_rule := 5.2]

deliveries_processed[is.na(numbaby) & is.na(numbaby_clean) &
                       conflict_mt_vars == F,
                     numbaby_clean := max_count_mt_vars]


print("Implementing rule set 6")
deliveries_processed[is.na(numbaby_clean), numbaby_cleaning_rule := 6]


print("Tidying up")

cols_to_drop <- c("multiples",
                  "count_sexbaby_",
                  "count_birordr_",
                  "count_birweit_",
                  "count_delmeth_",
                  "count_gestat_",
                  "count_birstat_",
                  "count_biresus_",
                  "conflict_mt_vars",
                  "max_count_mt_vars",
                  "numbaby",
                  "any_diag_singleton",
                  "any_diag_twin",
                  "any_diag_multiple",
                  "conflict_diag")

deliveries_processed <- deliveries_processed[, -cols_to_drop, with = F]

rm(col_diag, col_name, cols_to_drop, diag_multiple,
   diag_singleton, diag_twin, mt_vars, mt_vars_count,
   new_col)


print("Fixing order of  maternity tail variables")

mt_vars <- c("sexbaby_",
             "birordr_",
             "birweit_",
             "delmeth_",
             "gestat_",
             "birstat_",
             "biresus_")

fix_order <- function(vec) {
  vec_stripped <- vec[!is.na(vec)]
  return(c(vec_stripped, rep(NA, 9 - length(vec_stripped))))
}

for (curr_var in mt_vars) {
  
  print(paste0("Now doing ", curr_var))
  
  curr_cols <- paste0(curr_var, 1:9)
  
  print("Converting to long and fixing")
  mt_long <-
    melt(
      deliveries_processed[, c("epikey", "numbaby_clean", curr_cols), with = F],
      id.vars = c("epikey", "numbaby_clean")
    )
  
  mt_long[, variable_order := as.integer(gsub(curr_var, "", variable))]
  mt_long <- mt_long[order(epikey, variable_order)]
  
  mt_long[, value_fixed := fix_order(value), by = .(epikey)]
  mt_long[variable_order > numbaby_clean, value_fixed := NA]
  
  # mt_long[, variable := paste0(variable, "_fixed")]
  
  print("Converting back to wide")
  mt_wide <-
    dcast(
      mt_long[, c("epikey", "variable", "value_fixed")],
      formula = epikey ~ variable,
      value.var = "value_fixed"
    )
  
  if (nrow(mt_wide) != nrow(deliveries_processed)) {
    print("Error: nrow(mt_wide) != nrow(deliveries_processed)")
    break
  }
  
  print("Merging back into main data table")
  deliveries_processed <- deliveries_processed[, -curr_cols, with = F]
  
  deliveries_processed <- 
    merge(
      deliveries_processed,
      mt_wide,
      by = "epikey",
      all.x = T
    )
  
}

rm(mt_wide, mt_long, curr_cols, curr_var, mt_vars)


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_07.rda")
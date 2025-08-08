
print("Processing unassigned deliveries")
print("Episodes between delivery windows")

delivery_date_terminus_cols <- names(deliveries)[grepl("_date_plus168", names(deliveries))]
max_n_deliveries <- length(delivery_date_terminus_cols)

tmp <- deliveries_processed[, c("tokenid", "min_epistart", "delivery_n")]
tmp <- tmp[!duplicated(tmp)]
tmp[, delivery_n := paste0("delivery_", sprintf("%02d", delivery_n), "_min_epistart")]

tmp <-
  dcast(
    tmp,
    formula = tokenid ~ delivery_n,
    value.var = "min_epistart"
  )

deliveries <-
  merge(
    deliveries,
    tmp,
    by = "tokenid",
    all.x = T
  )

rm(tmp)

deliveries[, unassigned := NA]

for (i in 1:max_n_deliveries) {
  
  print(paste0("From delivery ", i))
  
  curr_min_epistart_col <- paste0("delivery_", sprintf("%02d", i), "_min_epistart")
  
  deliveries[!is.na(get(delivery_date_terminus_cols[i])) &
               epistart > get(curr_min_epistart_col) + 150 & 
               epistart < get(delivery_date_terminus_cols[i]), unassigned := T]
  
}

rm(i, curr_min_epistart_col, max_n_deliveries, delivery_date_terminus_cols)

deliveries_with_unassigned_episodes <- deliveries[tokenid %in% deliveries[unassigned == T]$tokenid]
rm(deliveries)


print("Saving")
save(deliveries_with_unassigned_episodes, file = "processed/deliveries_with_unassigned_episodes.rda")
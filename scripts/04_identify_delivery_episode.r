print("Processing deliveries")

delivery_list <- list()

for (i in 1:25) {
  
  print(paste0("Processing delivery number ", i))
  
  if (i > 17) {
    
    print("Checking if any more episodes to process")
    print("(only checking where i > 17 to save time)")
    
    n_remaining <- nrow(deliveries[epistart > get(paste0("delivery_", i - 1, "_date_plus168"))])
    
    if (n_remaining == 0) {
      print("No more episodes to process, ending loop")
      break
    } else {
      print(paste0("Found ", n_remaining, " more episodes; proceeding..."))
    }
    
  }
  
  
  print("Identifying relevant subset of data")
  
  if (i == 1) {
    
    deliveries[, min_epistart := min(epistart), by = .(tokenid)]
    deliveries[, episode_in_window := epistart >= min_epistart & epistart <= min_epistart + 150]
    delivery_list[[i]] <- deliveries[episode_in_window == T]
    deliveries[, min_epistart := NULL]
    deliveries[, episode_in_window := NULL]
    
  } else {
    
    excl_cols <- names(deliveries)[grepl("_date_plus168", names(deliveries))]
    
    delivery_list[[i]] <- deliveries[epistart >= get(paste0("delivery_", i - 1, "_date_plus168")), -excl_cols, with = F]
    delivery_list[[i]][, min_epistart := min(epistart), by = .(tokenid)]
    delivery_list[[i]][, episode_in_window := epistart >= min_epistart & epistart <= min_epistart + 150]
    delivery_list[[i]] <- delivery_list[[i]][episode_in_window == T]
    
    rm(excl_cols)
    
  }
  
  delivery_list[[i]][, n_episodes_in_window := sum(episode_in_window), by = .(tokenid)]
  delivery_list[[i]][, episode_in_window := NULL]
  
  
  print("Only one episode in window")
  
  delivery_list[[i]][n_episodes_in_window == 1, keep_episode := T]
  
  
  print("Multiple episodes - most complete record")
  
  delivery_list[[i]][n_episodes_in_window > 1,
                     max_complete := max(mat_tail_complete),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1]$tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1,
                     cnt := sum(mat_tail_complete == max_complete),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1]$tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & cnt == 1 & max_complete == mat_tail_complete, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & cnt == 1 & max_complete != mat_tail_complete, keep_episode := F]
  
  delivery_list[[i]][, cnt := NULL]
  delivery_list[[i]][, max_complete := NULL]
  
  
  print("Multiple episodes - closest to delivery using antedur")
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(antedur),
                     min_antedur := min(antedur, na.rm = T),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(antedur)]$tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(antedur),
                     cnt := sum(min_antedur == antedur),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(antedur)]$tokenid)]
  
  delivery_list[[i]][, min_antedur := min_antedur[!is.na(min_antedur)][1], by = .(tokenid)]
  delivery_list[[i]][, cnt := cnt[!is.na(cnt)][1], by = .(tokenid)] 
  
  delivery_list[[i]][, any_unique_antedur := any(cnt == 1, na.rm = T), by = .(tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_antedur == T & min_antedur == antedur, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_antedur == T & (min_antedur != antedur | is.na(antedur)), keep_episode := F]
  
  delivery_list[[i]][, any_unique_antedur := NULL]
  delivery_list[[i]][, cnt := NULL]
  delivery_list[[i]][, min_antedur := NULL]
  
  
  print("Multiple episodes - closest to delivery using postdur")
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(postdur),
                     min_postdur := min(postdur, na.rm = T),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(postdur)]$tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(postdur),
                     cnt := sum(min_postdur == postdur),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & !is.na(postdur)]$tokenid)]
  
  delivery_list[[i]][, any_unique_postdur := any(cnt == 1, na.rm = T), by = .(tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_postdur == T & min_postdur == postdur, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_postdur == T & (min_postdur != postdur | is.na(postdur)), keep_episode := F]
  
  delivery_list[[i]][, any_unique_postdur := NULL]
  delivery_list[[i]][, cnt := NULL]
  delivery_list[[i]][, min_postdur := NULL]
  
  
  print("Multiple episodes - keep using the greatest number of procedures")
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode),
                     max_n_delivery_procedures := min(n_delivery_procedures),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode)]$tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode),
                     cnt := sum(max_n_delivery_procedures == n_delivery_procedures),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode)]$tokenid)]
  
  delivery_list[[i]][, any_unique_n_proc := any(cnt == 1, na.rm = T), by = .(tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_n_proc == T & max_n_delivery_procedures == n_delivery_procedures, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_n_proc == T & (max_n_delivery_procedures != n_delivery_procedures | is.na(n_delivery_procedures)), keep_episode := F]
  
  delivery_list[[i]][, any_unique_n_proc := NULL]
  delivery_list[[i]][, cnt := NULL]
  delivery_list[[i]][, max_n_delivery_procedures := NULL]
  
  
  print("Multiple episodes - epi with first live birth recorded")
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & livebirth == 1,
                     min_epistart_livebirth := min(epistart),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & livebirth == 1]$tokenid)]
  
  # set min to all in child's record
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode),
                     min_epistart_livebirth := min_epistart_livebirth[!is.na(min_epistart_livebirth)][1],
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode)]$tokenid,
                            delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode)]$min_epistart)]
  
  # count how many epistart match
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode),
                     cnt := sum(epistart == min_epistart_livebirth),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode)]$tokenid)]
  
  # if one, use that and discard the rest
  delivery_list[[i]][, any_unique_livebirth := any(cnt == 1, na.rm = T), by = .(tokenid)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_livebirth == T & min_epistart_livebirth == epistart, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & any_unique_livebirth == T & (min_epistart_livebirth != epistart | is.na(n_delivery_procedures)), keep_episode := F]
  
  # if multiple, take minepikey
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1,
                     min_epikey := min(epikey),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1]$tokenid,
                            delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1]$min_epistart)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1 & min_epikey == epikey, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1 & min_epikey != epikey, keep_episode := F]
  
  delivery_list[[i]][, any_unique_livebirth := NULL]
  delivery_list[[i]][, min_epistart_livebirth := NULL]
  delivery_list[[i]][, cnt := NULL]
  delivery_list[[i]][, min_epikey := NULL]
  
  
  print("Multiple episodes - earliest episode start date")
  # count how many epistart match
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode),
                     cnt := sum(epistart == min_epistart),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode)]$tokenid)]
  
  # if one, use that and discard the rest
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt == 1 & min_epistart == epistart, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt == 1 & min_epistart != epistart, keep_episode := F]
  
  # if multiple, take minepikey
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1,
                     min_epikey := min(epikey),
                     by = .(delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1]$tokenid,
                            delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1]$min_epistart)]
  
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1 & min_epikey == epikey, keep_episode := T]
  delivery_list[[i]][n_episodes_in_window > 1 & is.na(keep_episode) & cnt > 1 & min_epikey != epikey, keep_episode := F]
  
  delivery_list[[i]][, cnt := NULL]
  delivery_list[[i]][, min_epikey := NULL]
  
  
  print("Dropping non-delivery episodes")
  delivery_list[[i]] <- delivery_list[[i]][keep_episode == T]
  delivery_list[[i]][, keep_episode := NULL]
  
  
  print("Updating delivery date")
  print("Using antedur and postdur")
  
  # delivery_list[[i]][!is.na(antedur) & keep_episode == T, delivery_date := as.Date(epistart + antedur)]
  # delivery_list[[i]][is.na(delivery_date) & is.na(antedur) & !is.na(postdur) & keep_episode == T, delivery_date := as.Date(epiend - postdur)]
  delivery_list[[i]][!is.na(antedur), delivery_date := as.Date(epistart + antedur)]
  delivery_list[[i]][is.na(delivery_date) & is.na(antedur) & !is.na(postdur), delivery_date := as.Date(epiend - postdur)]
  
  print("Using opdate")
  
  proc_codes <- paste0("R", c(17:25, 27))
  proc_cols <- paste0("opertn_", sprintf("%02d", 1:24))
  proc_date_cols <- paste0("opdate_", sprintf("%02d", 1:24))
  
  # tmp <- delivery_list[[i]][is.na(delivery_date) & keep_episode == T, c("epikey", proc_cols, proc_date_cols), with = F]
  tmp <- delivery_list[[i]][is.na(delivery_date), c("epikey", proc_cols, proc_date_cols), with = F]
  
  tmp_opertn <-
    melt(
      tmp[, c("epikey", proc_cols), with = F],
      id.vars = "epikey",
      variable.name = "op",
      value.name = "code"
    )
  
  tmp_opdate <-
    melt(
      tmp[, c("epikey", proc_date_cols), with = F],
      id.vars = "epikey",
      variable.name = "op",
      value.name = "opdate"
    )
  
  tmp_opertn[, op := as.integer(gsub("opertn_", "", op))]
  tmp_opdate[, op := as.integer(gsub("opdate_", "", op))]
  
  tmp <-
    merge(
      tmp_opertn,
      tmp_opdate,
      by = c("epikey", "op"),
      all.x = T
    )
  
  tmp <- tmp[substr(code, 1, 3) %in% proc_codes]
  tmp[, min_proc_date := min(opdate), by = .(epikey)]
  tmp <- tmp[, c("epikey", "min_proc_date")]
  tmp <- tmp[!duplicated(tmp)]
  
  delivery_list[[i]] <-
    merge(
      delivery_list[[i]],
      tmp,
      by = "epikey",
      all.x = T
    )
  
  rm(tmp, proc_codes, proc_cols, proc_date_cols, tmp_opertn, tmp_opdate)
  
  # delivery_list[[i]][min_proc_date < epistart & keep_episode == T, min_proc_date := NA]
  # delivery_list[[i]][is.na(delivery_date) & keep_episode == T, delivery_date := min_proc_date]
  delivery_list[[i]][min_proc_date < epistart, min_proc_date := NA]
  delivery_list[[i]][is.na(delivery_date), delivery_date := min_proc_date]
  
  
  print("Using epistart for the remainder")
  # delivery_list[[i]][is.na(delivery_date) & keep_episode == T, delivery_date := epistart]
  delivery_list[[i]][is.na(delivery_date), delivery_date := epistart]
  
  print("--- Checking only one valid delivery record per tokenid")
  # if (length(unique(delivery_list[[i]][keep_episode == T]$tokenid)) == nrow(delivery_list[[i]][keep_episode == T])) {
  if (length(unique(delivery_list[[i]]$tokenid)) == nrow(delivery_list[[i]])) {
    print("--- Fine")
  } else {
    print(paste0(" ***** More than one record per tokenid found in run ", i, " *****"))
  }
  

  print("Adding 168 days to delivery date for terminus post quem")
  delivery_date_plus168 <- paste0("delivery_", i, "_date_plus168")
  # delivery_list[[i]][keep_episode == T, (delivery_date_plus168) := as.Date(delivery_date + 168)]
  delivery_list[[i]][, (delivery_date_plus168) := as.Date(delivery_date + 168)]
  
  deliveries <- 
    merge(
      deliveries,
      delivery_list[[i]][!is.na(delivery_date),
                         c("tokenid", paste0("delivery_", i, "_date_plus168")), with = F],
      by = "tokenid",
      all.x = T
    )
  
  delivery_list[[i]][, (delivery_date_plus168) := NULL]
  rm(delivery_date_plus168)
  
  
  print(paste0("Assigning delivery number (", i, ")"))
  delivery_list[[i]][, delivery_n := i]
  
  print(paste0("Number of identified deliveries: ", nrow(delivery_list[[i]])))
  
  delivery_list[[i]][, mydob := NULL]
  delivery_list[[i]][, n_episodes_in_window := NULL]
  delivery_list[[i]][, mat_tail_complete := NULL]
  delivery_list[[i]][, n_delivery_procedures := NULL]
  delivery_list[[i]][, min_proc_date := NULL]
  
  print("-----------------------------------------------")
  
}

print("Combining into one file")
deliveries_processed <- do.call("rbind", delivery_list)
deliveries_processed <- deliveries_processed[order(tokenid, delivery_n)]
rm(delivery_list, i, n_remaining)


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_04.rda")
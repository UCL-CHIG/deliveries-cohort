
print("Classifying birth status of deliveries")

all_live_diags <- c("Z370", "Z372", "Z375")
some_live_diags <- c("Z373", "Z376")
all_stillbirth_diags <- c("Z371", "Z374", "Z377")

birstat_cols <- names(deliveries_processed)[grepl("birstat_", names(deliveries_processed))]
diag_cols <- names(deliveries_processed)[grepl("diag_", names(deliveries_processed))]

deliveries_processed[, birth_status := factor(NA,
                                              levels = c("all_liveborn",
                                                         "some_liveborn",
                                                         "no_liveborn",
                                                         "uncategorised"))]

print("By birstat")

tmp <-
  melt(
    deliveries_processed[, c("epikey", birstat_cols), with = F],
    id.vars = "epikey"
  )

tmp <- tmp[order(epikey, variable)]

# Need to remove all NA because all(logical(0)) == T as all zero of the elements are true.
# This is ok as when we merge back into the main data.table, this will induce NA.
tmp[, all_birstat_na := all(is.na(value)), by = "epikey"]
tmp <- tmp[all_birstat_na == F]
tmp[, all_birstat_na := NULL]

tmp[, all_birstat_liveborn := all(value == 1, na.rm = T), by = epikey]
tmp[, some_birstat_liveborn := any(value == 1, na.rm = T) & any(value %in% 2:4, na.rm = T), by = epikey]

strip_vec <- function(x) { # all(value %in% 2:4, na.rm = T) does not work for this application
  return(x[!is.na(x)])
}

tmp[, no_birstat_liveborn := all(strip_vec(value) %in% 2:4), by = epikey]

tmp <- tmp[, c("epikey", "all_birstat_liveborn", "some_birstat_liveborn", "no_birstat_liveborn")]
tmp <- tmp[!duplicated(tmp)]


print("Merging back in")
deliveries_processed <-
  merge(
    deliveries_processed,
    tmp,
    by = "epikey",
    all.x = T
  )


print("By diagnosis")
tmp <-
  melt(
    deliveries_processed[, c("epikey", diag_cols), with = F],
    id.vars = "epikey"
  )

tmp <- tmp[order(epikey, variable)]
tmp <- tmp[value %in% c(all_live_diags, some_live_diags, all_stillbirth_diags)]

tmp[, all_diag_liveborn := all(value %in% all_live_diags), by = epikey]
tmp[, some_diag_liveborn := all(value %in% some_live_diags), by = epikey]
tmp[, no_diag_liveborn := all(value %in% all_stillbirth_diags), by = epikey]

tmp <- tmp[, c("epikey", "all_diag_liveborn", "some_diag_liveborn", "no_diag_liveborn")]
tmp <- tmp[!duplicated(tmp)]


print("Merging back in")
deliveries_processed <-
  merge(
    deliveries_processed,
    tmp,
    by = "epikey",
    all.x = T
  )

rm(tmp, strip_vec)


print("Updating categorical flag")
deliveries_processed[all_birstat_liveborn == T | all_diag_liveborn == T, birth_status := "all_liveborn"]
deliveries_processed[some_birstat_liveborn == T | some_diag_liveborn == T, birth_status := "some_liveborn"]
deliveries_processed[no_birstat_liveborn == T | no_diag_liveborn == T, birth_status := "no_liveborn"]
deliveries_processed[is.na(birth_status), birth_status := "uncategorised"]


print("Dealing with conflicts")
# there are no conflicts wthin birstat and diag by design
deliveries_processed[, birth_status_conflict :=
                       (all_birstat_liveborn == T & (some_diag_liveborn == T | no_diag_liveborn == T)) |
                       (some_birstat_liveborn == T & (all_diag_liveborn == T | no_diag_liveborn == T)) |
                       (no_birstat_liveborn == T & (all_diag_liveborn == T | some_birstat_liveborn == T)) |
                       (all_diag_liveborn == T & (some_birstat_liveborn == T | no_birstat_liveborn == T)) |
                       (some_diag_liveborn == T & (all_birstat_liveborn == T | no_birstat_liveborn == T)) |
                       (no_diag_liveborn == T & (all_birstat_liveborn == T | some_birstat_liveborn == T))]

deliveries_processed[birth_status_conflict == T & all_diag_liveborn == T, birth_status := "all_liveborn"]
deliveries_processed[birth_status_conflict == T & some_diag_liveborn == T, birth_status := "some_liveborn"]
deliveries_processed[birth_status_conflict == T & no_diag_liveborn == T, birth_status := "no_liveborn"]


print("Tidying up")
rm(all_live_diags, some_live_diags, all_stillbirth_diags, birstat_cols, diag_cols)
deliveries_processed[, all_birstat_liveborn := NULL]
deliveries_processed[, some_birstat_liveborn := NULL]
deliveries_processed[, no_birstat_liveborn := NULL]
deliveries_processed[, all_diag_liveborn := NULL]
deliveries_processed[, some_diag_liveborn := NULL]
deliveries_processed[, no_diag_liveborn := NULL]
deliveries_processed[, livebirth := NULL]
deliveries_processed[, stillbirth := NULL]


print("Saving")
save(deliveries_processed, file = "processed/tmp_deliveries_08.rda")

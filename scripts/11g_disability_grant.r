print(" ------ Grant disability ------ ")
print("Loading and preparing code list")
grant <- fread("codelists/disability_grant_v1.csv", stringsAsFactors = F)
grant[, code := gsub("\\.", "", code)]


print("Flagging episodes")

diagnoses_long[, physical := F]
diagnoses_long[, hearing_impairments := F]
diagnoses_long[, vision_impairments := F]
diagnoses_long[, developmental_disabilities := F]


print("physical")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "physical"]$code, physical := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "physical"]$code, physical := T]


print("hearing_impairments")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "hearing_impairments"]$code, hearing_impairments := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "hearing_impairments"]$code, hearing_impairments := T]


print("vision_impairments")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "vision_impairments"]$code, vision_impairments := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "vision_impairments"]$code, vision_impairments := T]


print("developmental_disabilities")
diagnoses_long[substr(code, 1, 3) %in% grant[nchar(code) == 3 & group == "developmental_disabilities"]$code, developmental_disabilities := T]
diagnoses_long[substr(code, 1, 4) %in% grant[nchar(code) == 4 & group == "developmental_disabilities"]$code, developmental_disabilities := T]



print("Subsetting episodes")
diagnoses_grant <- diagnoses_long[physical | hearing_impairments | vision_impairments | developmental_disabilities]

diagnoses_long[, physical := NULL]
diagnoses_long[, hearing_impairments := NULL]
diagnoses_long[, vision_impairments := NULL]
diagnoses_long[, developmental_disabilities := NULL]


print("Identifying diagnoses in relevant window")
grps <- unique(grant$group)
deliveries_processed <- id_diag_in_window(diagnoses_grant, deliveries_processed, grps, "disabilities_grant", years)
rm(grps, diagnoses_grant, grant)

deliveries_processed[, disabilities_grant_any_3yrs_prior_del :=
                       disabilities_grant_physical_3yrs_prior_del |
                       disabilities_grant_hearing_impairments_3yrs_prior_del |
                       disabilities_grant_vision_impairments_3yrs_prior_del |
                       disabilities_grant_developmental_disabilities_3yrs_prior_del]

# table(deliveries_processed$disabilities_grant_any_3yrs_prior_del)
# table(deliveries_processed$disabilities_grant_physical_3yrs_prior_del)
# table(deliveries_processed$disabilities_grant_hearing_impairments_3yrs_prior_del)
# table(deliveries_processed$disabilities_grant_vision_impairments_3yrs_prior_del)
# table(deliveries_processed$disabilities_grant_developmental_disabilities_3yrs_prior_del)

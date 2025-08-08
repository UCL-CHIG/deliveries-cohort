print(" ------ SRPs ------")
print("Loading and preparing code list")
srp_nichobhthaigh <- fread("codelists/srp_nichobhthaigh_v2.csv", stringsAsFactors = F)
srp_nichobhthaigh[, code := gsub("\\.", "", code)]


print("Flagging episodes")

diagnoses_long[, psych := F]
diagnoses_long[, internalising := F]
diagnoses_long[, externalising := F]
diagnoses_long[, thought_dis := F]
diagnoses_long[, selfharm := F]


print("Potentially psychosomatic")
diagnoses_long[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "potentially_psych"]$code &
            code_no == 1, # All first diagnostic position only
          psych := T]

diagnoses_long[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "potentially_psych"]$code &
              code_no == 1, # All first diagnostic position only
          psych := T]


print("Medical/surgical flags")
med_surg <- fread("codelists/srp_nichobhthaigh_v2_medical.csv", stringsAsFactors = F)
med_surg[, code := gsub("\\.", "", code)]

print("Diagnosis codes")
diagnoses_long[, med_surg_diag := F]
diagnoses_long[substr(code, 1, 3) %in% med_surg[code_type == "icd10" & nchar(code) == 3]$code, med_surg_diag := T]
diagnoses_long[substr(code, 1, 4) %in% med_surg[code_type == "icd10" & nchar(code) == 4]$code, med_surg_diag := T]
diagnoses_long[, med_surg_diag := max(med_surg_diag), by = .(tokenid, epistart)]

print("Operations codes")
operations_long[, med_surg_opertn := F]
operations_long[substr(code, 1, 4) %in% med_surg[code_type == "opcs4"]$code, med_surg_opertn := T]
operations_long[, med_surg_opertn := max(med_surg_opertn), by = .(tokenid, epistart)]

operations_tmp <- operations_long[med_surg_opertn == T]
operations_long[, med_surg_opertn := NULL]

operations_tmp <- operations_tmp[, c("tokenid", "epistart", "med_surg_opertn")]
operations_tmp <- operations_tmp[!duplicated(operations_tmp)]

diagnoses_long <-
  merge(
    diagnoses_long,
    operations_tmp,
    by = c("tokenid", "epistart"),
    all.x = T
  )

diagnoses_long[psych == T & (med_surg_diag == T | med_surg_opertn == T), psych := F]
diagnoses_long[, med_surg_diag := NULL]
diagnoses_long[, med_surg_opertn := NULL]
rm(operations_tmp, med_surg)


print("Internalising")
diagnoses_long[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "internalising"]$code &
            code_no == 1, # All first diagnostic position only
          internalising := T]

diagnoses_long[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "internalising"]$code &
              code_no == 1, # All first diagnostic position only
          internalising := T]


print("Externalising")
# There are some externalising codes that are only counted if an X or Y block
# self-harm code is present in a secondary position. However, these will always
# be counted as self-harm codes and so we ignore them here.
diagnoses_long[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "externalising" & flag2 != "selfharm_xz_codes"]$code &
              code_no == 1, # All first diagnostic position only
          externalising := T]

diagnoses_long[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "externalising" & flag2 != "selfharm_xz_codes"]$code &
              code_no == 1, # All first diagnostic position only
          externalising := T]


print("Thought disorder")
diagnoses_long[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "thought_disorder"]$code &
              code_no == 1, # All first diagnostic position only
          thought_dis := T]

diagnoses_long[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "thought_disorder"]$code &
              code_no == 1, # All first diagnostic position only
          thought_dis := T]


print("Self-harm")
# There are some self-harm codes that are only counted if an X or Y block
# self-harm code is present in a secondary position. However, these will always
# be counted as self-harm codes and so we ignore them here.
diagnoses_long[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "selfharm" & flag2 != "selfharm_xz_codes"]$code,
          selfharm := T]

diagnoses_long[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "selfharm" & flag2 != "selfharm_xz_codes"]$code,
          selfharm := T]


print("Subsetting episodes")
em_adm <- fread("codelists/emergency_admissions_v1.csv")$code
diagnoses_srp <- diagnoses_long[(psych | internalising | externalising | thought_dis | selfharm) & admimeth %in% em_adm]

diagnoses_long[, psych := NULL]
diagnoses_long[, internalising := NULL]
diagnoses_long[, externalising := NULL]
diagnoses_long[, thought_dis := NULL]
diagnoses_long[, selfharm := NULL]

rm(em_adm)


print("Aggregating to episode level")
diagnoses_srp <- diagnoses_srp[order(tokenid, epistart)]
diagnoses_srp[, psych := as.logical(max(psych)), by = .(tokenid, epistart)]
diagnoses_srp[, internalising := as.logical(max(internalising)), by = .(tokenid, epistart)]
diagnoses_srp[, externalising := as.logical(max(externalising)), by = .(tokenid, epistart)]
diagnoses_srp[, thought_dis := as.logical(max(thought_dis)), by = .(tokenid, epistart)]
diagnoses_srp[, selfharm := as.logical(max(selfharm)), by = .(tokenid, epistart)]


print("Identifying diagnoses in relevant window")
grps <-
  c(
    "psych",
    "internalising",
    "externalising",
    "thought_dis",
    "selfharm"
  )

deliveries_processed <- id_diag_in_window(diagnoses_srp, deliveries_processed, grps, "srp", years)

rm(grps, diagnoses_srp, srp_nichobhthaigh)


# table(deliveries_processed$srp_psych_3yrs_prior_del, useNA = "always")
# table(deliveries_processed$srp_internalising_3yrs_prior_del, useNA = "always")
# table(deliveries_processed$srp_externalising_3yrs_prior_del, useNA = "always")
# table(deliveries_processed$srp_thought_dis_3yrs_prior_del, useNA = "always")
# table(deliveries_processed$srp_selfharm_3yrs_prior_del, useNA = "always")

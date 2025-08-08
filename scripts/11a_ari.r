
print(" ------ ARIs/ARAs ------")
print("Loading and preparing code list")

herbert <- fread("codelists/ari_herbert_v1.csv", stringsAsFactors = F)
herbert[, code := gsub("\\.", "", code)]

new_f17 <- data.table(
  code = c("F17.0", paste0("F17.", 2:9)),
  dataset = "hes_apc",
  field = "diag",
  code_type = "icd10",
  description = "smoking_codes",
  group = "drug_alc",
  subgroup = "illict_drugs",
  flag1 = "emergency_admission",
  flag2 = ""
)

herbert <- rbind(herbert, new_f17)
herbert <- herbert[code != "F17"]
rm(new_f17)


print("Flagging episodes")
diagnoses_long[, first_episode := admidate == epistart]

diagnoses_long[, injury_episode := F]
diagnoses_long[, adversity_episode := F]
diagnoses_long[, accident_episode := F]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "injuries"]$code, injury_episode := T]
diagnoses_long[injury_episode == T & first_episode == F, injury_episode := F]

diagnoses_long[substr(code, 1, 3) %in% herbert[!(group %in% c("accidents", "injuries")) & nchar(code) == 3]$code, adversity_episode := T]
diagnoses_long[substr(code, 1, 4) %in% herbert[!(group %in% c("accidents", "injuries")) & nchar(code) == 4]$code, adversity_episode := T]

diagnoses_long[substr(code, 1, 3) %in% herbert[group == "accidents"]$code, accident_episode := T]

print("Subsetting episodes")
em_adm <- fread("codelists/emergency_admissions_v1.csv")$code
diagnoses_herbert <- diagnoses_long[(injury_episode | adversity_episode | accident_episode) & admimeth %in% em_adm]

diagnoses_long[, first_episode := NULL]
diagnoses_long[, injury_episode := NULL]
diagnoses_long[, adversity_episode := NULL]
diagnoses_long[, accident_episode := NULL]
rm(em_adm)


diagnoses_herbert[, injury_admission := as.logical(max(injury_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, adversity_admission := as.logical(max(adversity_episode)), by = .(tokenid, admidate)]
diagnoses_herbert[, accident_admission := as.logical(max(accident_episode)), by = .(tokenid, admidate)]

diagnoses_herbert[, adversity_injury_admission :=
                    injury_admission &
                    adversity_admission]

diagnoses_herbert[, accident_injury_admission :=
                    injury_admission &
                    !adversity_admission &
                    accident_admission]

diagnoses_herbert <- diagnoses_herbert[adversity_injury_admission == T |
                                         accident_injury_admission == T |
                                         adversity_admission == T]


print("Identifying diagnoses in relevant window")
for (i in 1:max_del) {
  
  print(paste0("Delivery ", i))
  del_col <- paste0("del_", i)
  
  new_col <- paste0("adversity_admission_", years, "yr_prior_del_", i)
  diagnoses_herbert[, (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col) & adversity_admission == T]
  
  new_col <- paste0("adversity_injury_admission_", years, "yr_prior_del_", i)
  diagnoses_herbert[, (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col) & adversity_injury_admission == T]
  
  new_col <- paste0("accident_injury_admission_", years, "yr_prior_del_", i)
  diagnoses_herbert[, (new_col) := epistart >= get(del_col) - 365 * years & epistart <= get(del_col) & accident_injury_admission == T]
  
}

rm(i, new_col, del_col)


print("Converting to long")
diagnoses_herbert_adv_inj <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("adversity_injury_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_adv_adm <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("adversity_admission_", names(diagnoses_herbert))]), with = F]
diagnoses_herbert_acc_inj <- diagnoses_herbert[, c("tokenid", names(diagnoses_herbert)[grepl("accident_injury_admission_", names(diagnoses_herbert))]), with = F]

diagnoses_herbert_adv_inj <- diagnoses_herbert_adv_inj[!duplicated(diagnoses_herbert_adv_inj)]
diagnoses_herbert_adv_adm <- diagnoses_herbert_adv_adm[!duplicated(diagnoses_herbert_adv_adm)]
diagnoses_herbert_acc_inj <- diagnoses_herbert_acc_inj[!duplicated(diagnoses_herbert_acc_inj)]

diagnoses_herbert_adv_inj <-
  melt(
    diagnoses_herbert_adv_inj,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("adversity_injury_", years, "yrs_prior_del")
  )

diagnoses_herbert_adv_inj <- diagnoses_herbert_adv_inj[get(paste0("adversity_injury_", years, "yrs_prior_del")) == T]
diagnoses_herbert_adv_inj[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_herbert_adv_inj <- diagnoses_herbert_adv_inj[!duplicated(diagnoses_herbert_adv_inj)]


diagnoses_herbert_adv_adm <-
  melt(
    diagnoses_herbert_adv_adm,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("adversity_admission_", years, "yrs_prior_del")
  )

diagnoses_herbert_adv_adm <- diagnoses_herbert_adv_adm[get(paste0("adversity_admission_", years, "yrs_prior_del")) == T]
diagnoses_herbert_adv_adm[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_herbert_adv_adm <- diagnoses_herbert_adv_adm[!duplicated(diagnoses_herbert_adv_adm)]


diagnoses_herbert_acc_inj <-
  melt(
    diagnoses_herbert_acc_inj,
    id.vars = "tokenid",
    variable.name = "delivery_n",
    value.name = paste0("accident_injury_", years, "yrs_prior_del")
  )

diagnoses_herbert_acc_inj <- diagnoses_herbert_acc_inj[get(paste0("accident_injury_", years, "yrs_prior_del")) == T]
diagnoses_herbert_acc_inj[, delivery_n := as.integer(gsub("[^0-9]+|3yr", "", delivery_n))]
diagnoses_herbert_acc_inj <- diagnoses_herbert_acc_inj[!duplicated(diagnoses_herbert_acc_inj)]


print("Merging flags into deliveries data")
deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_herbert_adv_inj,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_herbert_adv_adm,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed <-
  merge(
    deliveries_processed,
    diagnoses_herbert_acc_inj,
    by = c("tokenid", "delivery_n"),
    all.x = T
  )

deliveries_processed[is.na(get(paste0("adversity_injury_", years, "yrs_prior_del"))), (paste0("adversity_injury_", years, "yrs_prior_del")) := FALSE]
deliveries_processed[is.na(get(paste0("adversity_admission_", years, "yrs_prior_del"))), (paste0("adversity_admission_", years, "yrs_prior_del")) := FALSE]
deliveries_processed[is.na(get(paste0("accident_injury_", years, "yrs_prior_del"))), (paste0("accident_injury_", years, "yrs_prior_del")) := FALSE]

# table(deliveries_processed$adversity_injury_3yrs_prior_del, useNA = "always")
# table(deliveries_processed$adversity_admission_3yrs_prior_del, useNA = "always")
# table(deliveries_processed$accident_injury_3yrs_prior_del, useNA = "always")


print("Removing temporary data")
rm(herbert, diagnoses_herbert, diagnoses_herbert_adv_inj, diagnoses_herbert_adv_adm,
   diagnoses_herbert_acc_inj)

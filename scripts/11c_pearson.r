print(" ------ Pearson - MH/behavioural ------")
print("Loading and preparing code list")
mhbev_pearson <- fread("codelists/mhbev_pearson_v1.csv", stringsAsFactors = F)
mhbev_pearson[, code := gsub("\\.", "", code)]


print("Flagging episodes")

diagnoses_long[, anx_somat_stress := F]
diagnoses_long[, drug_alc := F]
diagnoses_long[, other_depr := F]
diagnoses_long[, other_pyschiatr := F]
diagnoses_long[, personality := F]
diagnoses_long[, psych_dev_beh_emo := F]
diagnoses_long[, schiz_del := F]
diagnoses_long[, sev_mood_dis := F]


print("anx_somat_stress") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "anx_somat_stress"]$code, anx_somat_stress := T]


print("drug_alc") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "drug_alc"]$code, drug_alc := T]


print("other_depr")
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[nchar(code) == 3 & group == "other_depr"]$code, other_depr := T]
diagnoses_long[substr(code, 1, 4) %in% mhbev_pearson[nchar(code) == 4 & group == "other_depr"]$code, other_depr := T]


print("other_pyschiatr")
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[nchar(code) == 3 & group == "other_pyschiatr"]$code, other_pyschiatr := T]
diagnoses_long[substr(code, 1, 4) %in% mhbev_pearson[nchar(code) == 4 & group == "other_pyschiatr"]$code, other_pyschiatr := T]


print("personality") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "personality"]$code, personality := T]


print("psych_dev_beh_emo") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "psych_dev_beh_emo"]$code, psych_dev_beh_emo := T]


print("schiz_del") # No 4 char codes
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[group == "schiz_del"]$code, schiz_del := T]


print("sev_mood_dis")
diagnoses_long[substr(code, 1, 3) %in% mhbev_pearson[nchar(code) == 3 & group == "sev_mood_dis"]$code, sev_mood_dis := T]
diagnoses_long[substr(code, 1, 4) %in% mhbev_pearson[nchar(code) == 4 & group == "sev_mood_dis"]$code, sev_mood_dis := T]


print("Subsetting episodes")
diagnoses_mhbev <- diagnoses_long[anx_somat_stress | drug_alc | other_depr | other_pyschiatr |
                                    personality | psych_dev_beh_emo | schiz_del | sev_mood_dis]

diagnoses_long[, anx_somat_stress := NULL]
diagnoses_long[, drug_alc := NULL]
diagnoses_long[, other_depr := NULL]
diagnoses_long[, other_pyschiatr := NULL]
diagnoses_long[, personality := NULL]
diagnoses_long[, psych_dev_beh_emo := NULL]
diagnoses_long[, schiz_del := NULL]
diagnoses_long[, sev_mood_dis := NULL]
 

print("Identifying diagnoses in relevant window")
grps <- unique(mhbev_pearson$group)
deliveries_processed <- id_diag_in_window(diagnoses_mhbev, deliveries_processed, grps, "mhbev", years)
rm(grps, diagnoses_mhbev, mhbev_pearson)

deliveries_processed[, mhbev_any_3yrs_prior_del :=
                       mhbev_anx_somat_stress_3yrs_prior_del |
                       mhbev_drug_alc_3yrs_prior_del |
                       mhbev_other_depr_3yrs_prior_del |
                       mhbev_other_pyschiatr_3yrs_prior_del |
                       mhbev_personality_3yrs_prior_del |
                       mhbev_psych_dev_beh_emo_3yrs_prior_del |
                       mhbev_schiz_del_3yrs_prior_del |
                       mhbev_sev_mood_dis_3yrs_prior_del]

# table(deliveries_processed$mhbev_anx_somat_stress_3yrs_prior_del)
# table(deliveries_processed$mhbev_drug_alc_3yrs_prior_del)
# table(deliveries_processed$mhbev_other_depr_3yrs_prior_del)
# table(deliveries_processed$mhbev_other_pyschiatr_3yrs_prior_del)
# table(deliveries_processed$mhbev_personality_3yrs_prior_del)
# table(deliveries_processed$mhbev_psych_dev_beh_emo_3yrs_prior_del)
# table(deliveries_processed$mhbev_schiz_del_3yrs_prior_del)
# table(deliveries_processed$mhbev_sev_mood_dis_3yrs_prior_del)
# table(deliveries_processed$mhbev_any_3yrs_prior_del)


print("Implementing phenotypes from ECHILD Phenotype Code List Repository")

years <- 3


print("Merging into diagnoses data the delivery dates in wide format")
max_del <- max(deliveries_processed$delivery_n)

print("Converting to wide")
dels_wide <-
  dcast(
    deliveries_processed[, c("tokenid", "delivery_n", "delivery_date")],
    formula = tokenid ~ delivery_n,
    value.var = "delivery_date"
  )

nms <- names(dels_wide)[names(dels_wide) %in% 1:max_del]
setnames(dels_wide, nms, paste0("del_", nms))
rm(nms)

print("Merging")
diagnoses_long <-
  merge(
    diagnoses_long,
    dels_wide,
    by = "tokenid",
    all.x = T
  )

operations_long <-
  merge(
    operations_long,
    dels_wide,
    by = "tokenid",
    all.x = T
  )

rm(dels_wide)


print("Implementing single phenotypes")

source("scripts/11a_ari.r")
source("scripts/11b_srp.r")
source("scripts/11c_pearson.r")
source("scripts/11d_hardelid.r")
source("scripts/11e_charlson.r")
source("scripts/11f_id_sheehan.r")
source("scripts/11g_disability_grant.r")
source("scripts/11h_enmmoi.r")
source("scripts/11i_high_risk_preg.r")
source("scripts/11j_other.r")


print("Saving")
rm(diagnoses_long, operations_long)
save(deliveries_processed, file = "processed/tmp_deliveries_11.rda")

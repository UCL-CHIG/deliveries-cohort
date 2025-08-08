print("Exclusions")

# under 15
# not born in England
# death before delivery
# hes after death
# epistart to delivery > 100 days
# has unassigned episode
# abortion
# numbaby_clean is missing

load("processed/deliveries_with_unassigned_episodes.rda")

deliveries_processed[, exclude :=
                       startage < 15 |
                       not_england == T |
                       (!is.na(dod_combined) & dod_combined < delivery_date) |
                       !is.na(hes_activity_after_dod) |
                       delivery_date - epistart > 100 |
                       tokenid %in% deliveries_with_unassigned_episodes$tokenid |
                       abortion == T |
                       numbaby_cleaning_rule == 6]

rm(deliveries_with_unassigned_episodes)

deliveries_processed <- deliveries_processed[exclude == F]

save(deliveries_processed, file = "processed/tmp_deliveries_14.rda")

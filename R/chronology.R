# demo_codes2 <- c("NIHR_HIC_ICU_0032", "NIHR_HIC_ICU_0033", "NIHR_HIC_ICU_0034", "NIHR_HIC_ICU_0035",
#                  "NIHR_HIC_ICU_0036", "NIHR_HIC_ICU_0037",
#                  "NIHR_HIC_ICU_0038", "NIHR_HIC_ICU_0042", "NIHR_HIC_ICU_0044", "NIHR_HIC_ICU_0048",
#                  "NIHR_HIC_ICU_0050", "NIHR_HIC_ICU_0406",
#                  "NIHR_HIC_ICU_0407", "NIHR_HIC_ICU_0408", "NIHR_HIC_ICU_0411", "NIHR_HIC_ICU_0412",
#                  "NIHR_HIC_ICU_0039", "NIHR_HIC_ICU_0043",
#                  "NIHR_HIC_ICU_0045", "NIHR_HIC_ICU_0049", "NIHR_HIC_ICU_0051")
#
# ddtb <- extract_demographics(metadata = tbls[["variables"]],
#                              events = tbls[["events"]],
#                              codes = demo_codes2)
#
#
# ### create timeline
#
# ## take each event, and calculate its difftime from 0411
# ddtb %>%
#   mutate_at(.vars = vars(demo_odes))
# ## change into long form
# ## change the timing into text
#
# ddtb

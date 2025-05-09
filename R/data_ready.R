# pak::pak("agdamsbo/REDCapCAST")



# summary(ds_all$event)

# ds_all$war

# ds_meta <- REDCapR::redcap_metadata_read(
#   redcap_uri = "https://redcap.rm.dk/api/",
#   token = keyring::key_get("EVT_ANEST_REDCAP_API"))
# 
# ds_meta$data |> View()
# 

df_raw <- targets::tar_read("data_for_ida")
ds_all <- targets::tar_read("data_merge")

df <- df_raw |>
  REDCapCAST::as_factor() |>
  REDCapCAST::as_logical() |>
  dplyr::as_tibble()

df <- dplyr::left_join(
  df,
  ds_all |>
    dplyr::mutate(ivt = !is.na(startlyse_datetime)) |>
    dplyr::select(forloebid, ivt, sidstopdateret_tremdrop)
)

icu_direct_levels <- levels(df$icu_direct)

df_final <- df |>
  dplyr::mutate(
    mors_hospital = ifelse(mors_date > as.Date(udskr_datetime),
                           "After discharge",
                           "During admission"
    ),
    excluded = dplyr::case_when(
      excluded == "Yes" ~ "Missing data",
      !is.na(excluded) ~ excluded,
      is.na(extubation_novent) ~ "Missing data",
      age < 18 ~ "Age <18 years",
      # Only one true duplication, which did not have data registered
      # as.character(forloebid) %in% duplet ~ "Duplication",
      .default = NA
    ),
    location = factor(location),
    mrs_3mdr_group = cut(as.numeric(mrs_3mdr), breaks = c(0, 3, Inf), labels = c("0-2", "3-6"), right = FALSE),
    mrs_pre_num = as.numeric(mrs_pre),
    mrs_3mdr_num = as.numeric(mrs_3mdr),
    tici_group = dplyr::if_else(tici %in% c("2b", "2c", "3"), "2b-3", "0-2a"),
    tici_group = dplyr::if_else(is.na(tici), NA, tici_group),
    ward_event_icu_type = dplyr::if_else(ward_event_icu, ward_events_type, NA),
    event_type = dplyr::case_when(
      icu_direct %in% c(
        icu_direct_levels[c(2,5,6)]
      ) ~ "Extubation failure",
      !is.na(icu_direct) ~ "Directly to ICU",
      !is.na(pacu_events_type) ~ "PACU to ICU transfer",
      !is.na(pacu_extend_type) ~ "PACU extention",
      !is.na(ward_event_icu_type) ~ "Ward to ICU transfer"
    ),
    event_type = factor(
      event_type,
      levels = c(
        "Extubation failure",
        "Directly to ICU",
        "PACU to ICU transfer",
        "PACU extention",
        "Ward to ICU transfer"
      )
    )
  ) |>
  labelling_data()

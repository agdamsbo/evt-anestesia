# pak::pak("agdamsbo/REDCapCAST")

ds_evt <- REDCapR::redcap_read(
  redcap_uri = "https://redcap.rm.dk/api/",
  token = keyring::key_get("EVT_ANEST_REDCAP_API"),
  fields = c(
    "forloebid",
    "extubation_novent",
    # "pacu_delay",
    # "pacu_delay_reason",
    "icu_direct",
    "reintubation_reason",
    "pacu_arrival_date",
    "pacu_arrival_time",
    "pacu_saox_first",
    "pacu_bp_sys_first",
    "pacu_bp_dia_first",
    "pacu_mabp_first",
    "pacu_saox_last",
    "pacu_bp_sys_last",
    "pacu_bp_dia_last",
    "pacu_mabp_last",
    "pacu_ox_req",
    "pacu_events_type",
    "pacu_extend_type",
    "pacu_dicharge_date",
    "pacu_dicharge_time",
    "reintubation_reason",
    "pacu_extend",
    "pacu_extend_type",
    "icu_arrival_date",
    "icu_arrival_time",
    "icu_dicharge_date",
    "icu_dicharge_time",
    "ward_events",
    "ward_event_icu",
    "ward_events_type",
    "ward_events_other",
    "pneumonia"
  ),
  raw_or_label = "label"
) |>
  purrr::pluck("data")

dd <- REDCapR::redcap_metadata_read(
  redcap_uri = "https://redcap.rm.dk/api/",
  token = keyring::key_get("EVT_ANEST_REDCAP_API"))

dd$data |> View()

dd$data$field_name[54:73]

ds_clin <- ds_evt |> evt_clinical_data()

# ds_clin |> dplyr::select(tidyselect::contains("doed")) |> skimr::skim()

ds_format <- ds_clin |>
  format_all() |>
  dplyr::rename(
    nihss = nihss_basisske
  ) |>
  merge_dato_tid()

ds_all <- dplyr::left_join(
  ds_evt |>
    merge_date_time() |>
    format_all(fmt.lst = evt_redcap_format_dic()),
  ds_format
) |>
  dplyr::mutate(
    art2rep_timediff = difftime(tidreperf_datetime, artpunkt_datetime, units = "mins"),
    ank2rep_timediff = difftime(tidreperf_datetime, modafdankomst_datetime, units = "mins"),
    onset2rep_timediff = difftime(tidreperf_datetime, symptdebexact_datetime, units = "mins"),
    art2procend_timediff = difftime(procafsl_datetime, artpunkt_datetime, units = "mins"),
    angio2art_timediff = difftime(artpunkt_datetime, ankneurorad_datetime, units = "mins"),
    mr2angio_timediff = difftime(ankneurorad_datetime, ankomstevtctmr_datetime, units = "mins"),
    admission_timediff = difftime(udskr_datetime, modafdankomst_datetime, units = "days"),
    mors_hospital = ifelse(mors_date > as.Date(udskr_datetime), "After discharge", "During admission")
  ) |>
  (\(.x){
    any_compl <- apply(dplyr::select(.x, dplyr::starts_with("compl_"), -dplyr::ends_with("text")), 1, \(.y){
      if (all(is.na(.y))){
        out <- NA
      } else {
        out <- any(.y[!is.na(.y)])
      }
      out
      })
    
    dplyr::bind_cols(.x,
                     compl_any = any_compl
    )
  })() |>
  dplyr::mutate(location = dplyr::case_when(
    trombelokva_trombekt | trombelokba_trombekt | trombelokpca_trombekt ~ "posterior",
    trombelokingen_trombekt ~ "none",
    trombelokcca_trombekt | trombelokica_trombekt | trombelokica_t_trombekt | trombelokmca1_trombekt | trombelokmca2_trombekt | trombelokaca_trombekt | trombelokcervical_trombekt ~ "anterior",
    .default = NA
  ))  |> REDCapCAST::as_logical() |> 
  dplyr::mutate(
    event = dplyr::if_else(!extubation_novent | pacu_extend | ward_event_icu | !is.na(reintubation_reason),true = TRUE,false = FALSE,missing = FALSE),
    event = factor(dplyr::case_when(
      event ~ "Complicated",
      .default = "Smooth"
    ))
  )

# summary(ds_all$event)

# ds_all$war

# ds_meta <- REDCapR::redcap_metadata_read(
#   redcap_uri = "https://redcap.rm.dk/api/",
#   token = keyring::key_get("EVT_ANEST_REDCAP_API"))
# 
# ds_meta$data |> View()
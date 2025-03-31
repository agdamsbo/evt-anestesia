source(here::here("R/functions.R"))
source(here::here("R/data_ready.R"))

ds_labelled <- name2label(ds_all)

# ds_upload <- ds_clean[c(1, 25:ncol(ds_clean))]

dd_old <- REDCapR::redcap_metadata_read(redcap_uri = "https://redcap.rm.dk/api/",
  token = keyring::key_get("EVT_ANEST_REDCAP_API"))$data

new_vars <- names(ds_labelled)[!names(ds_labelled) %in% dd_old$field_name]

for_upload <- new_vars[(length(new_vars)-1):length(new_vars)]

new_ds <- REDCapCAST::ds2dd_detailed(ds_labelled[c("forloebid",for_upload)], form.name = "background")$data

# dd <- REDCapCAST::ds2dd_detailed(ds_upload, form.name = "background")$meta
# 
# dd[-1, ] |> REDCapCAST::export_redcap_instrument(file = here::here("background.zip"))
# 
# ds_upload_adj <- REDCapCAST::ds2dd_detailed(ds_upload, form.name = "background")$data

# REDCapR::redcap_write(new_ds,
#                       redcap_uri = "https://redcap.rm.dk/api/",
#   token = keyring::key_get("EVT_ANEST_REDCAP_API"))

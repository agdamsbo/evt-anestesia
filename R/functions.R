###


#' Title
#'
#' @return
#' @export
#'
#' @examples
#' data <- get_evt_base()
#' data |> dplyr::filter()
get_evt_base <- function() {
  REDCapR::redcap_read_oneshot(
    token = keyring::key_get("DAP_REDCAP_API"),
    redcap_uri = "https://redcap.au.dk/api/",
    fields = c(
      "forloebid",
      "trombektomi_basisske",
      "trombektdato_basisske"
    )
  )$data |>
    dplyr::select(-redcap_repeat_instrument, -redcap_repeat_instance) |>
    dplyr::filter(
      trombektomi_basisske == 2,
      trombektdato_basisske >= "2020-01-01"
    )
}

evt_basis_data <- function(data) {
  REDCapR::redcap_read_oneshot(
    token = keyring::key_get("DAP_REDCAP_API"),
    redcap_uri = "https://redcap.au.dk/api/",
    records = dplyr::pull(data, forloebid),
    fields = c(
      "forloebid",
      "cpr_basisske",
      "firstname_patiente",
      "lastname_patiente",
      "forloebdato_patiente",
      "forloebtid_patiente"
    )
  )$data |>
    dplyr::select(-redcap_repeat_instrument, -redcap_repeat_instance)
}

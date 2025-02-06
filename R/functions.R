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
      "dateofbirth_patiente",
      "firstname_patiente",
      "lastname_patiente",
      "forloebdato_patiente",
      "forloebtid_patiente"
    )
  )$data |>
    dplyr::select(-redcap_repeat_instrument, -redcap_repeat_instance)
}

#' Simple function to generate REDCap choices from character vector
#'
#' @param data vector
#' @param char.split splitting character(s)
#' @param raw specific values. Can be used for options of same length.
#' @param .default deafult value for missing. Deafult is NA.
#'
#' @return vector
#' @export
#'
#' @examples
#' char2choice(c("yes/no", "  yep. / nope  ", "", NA, "what"), .default = NA)
char2choice <- function(data, char.split = "/", raw = NULL, .default = NA) {
  ls <- strsplit(x = data, split = char.split)

  ls |>
    purrr::map(function(.x) {
      if (is.null(raw)) {
        raw <- seq_len(length(.x))
      }
      if (length(.x) == 0 | all(is.na(.x))) {
        .default
      } else {
        paste(paste0(raw, ", ", trimws(.x)), collapse = " | ")
      }
    }) |>
    purrr::list_c()
}

#' Simple function to generate REDCap branching logic from character vector
#'
#' @param data vector
#' @param .default deafult value for missing. Deafult is NA.
#' @param minor.split
#' @param major.split
#' @param major.sep
#'
#' @return vector
#' @export
#'
#' @examples
#' data <- dd_inst$betingelse
#' c("Extubation_novent, 2; Pacu_delay, 1") |> char2cond()
char2cond <- function(data, minor.split = ",", major.split = ";", major.sep = " or ", .default = NA) {
  strsplit(x = data, split = major.split) |>
    purrr::map(function(.y) {
      strsplit(x = .y, split = minor.split) |>
        purrr::map(function(.x) {
          if (length(.x) == 0 | all(is.na(.x))) {
            .default
          } else {
            glue::glue("[{trimws(tolower(.x[1]))}]='{trimws(.x[2])}'")
          }
        }) |>
        purrr::list_c() |>
        glue::glue_collapse(sep = major.sep)
    }) |>
    purrr::list_c()
}


#' List-base regex case_when
#'
#' @description
#' Mimics case_when for list of regex patterns and values. Used for date/time
#' validation generation from name vector. Like case_when, the matches are in
#' order of priority.
#' Primarily used in REDCapCAST to do data type coding from systematic variable
#' naming.
#'
#' @param data vector
#' @param match.list list of case matches
#' @param .default Default value for non-matches. Default is NA.
#'
#' @return vector
#' @export
#'
#' @examples
#' case_match_regex_list(
#'   c("test_date", "test_time", "test_tida", "test_tid"),
#'   list(date_dmy = "_dat[eo]$", time_hh_mm_ss = "_ti[md]e?$")
#' )
case_match_regex_list <- function(data, match.list, .default = NA) {
  match.list |>
    purrr::imap(function(.z, .i) {
      dplyr::if_else(grepl(.z, data), .i, NA)
    }) |>
    (\(.x){
      dplyr::coalesce(!!!.x)
    })() |>
    (\(.x){
      dplyr::if_else(is.na(.x), .default, .x)
    })()
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' is_missing(data = c("", NA, 2))
#' is_missing(data = NULL)
is_missing <- function(data) {
  if (is.null(data)) {
    TRUE
  } else {
    is.na(data) | data %in% c("", "NA")
  }
}

#' Doc table to data dictionary
#'
#' @description
#' Allows defining a database in a text document (see provided template) for
#' an easier to use data base creation. This approach allows easier
#' collaboration when defining the database
#'
#' @param data tibble or data.frame with all variable names in one column
#' @param instrument.name character vecter length one. Instrument name.
#' @param col.variables variable names column (default = 1), allows dplyr subsetting
#' @param list.datetime.format formatting for date/time detection.
#' See `case_match_regex_list()`
#' @param col.description descriptions column, allows dplyr
#' subsetting. If empty, variable names will be used.
#' @param col.condition conditions for branching column, allows dplyr
#' subsetting. See `char2cond()`.
#' @param col.subheader subheader column, allows dplyr subsetting.
#' See `format_subheader()`.
#' @param subheader.tag formatting tag. Default is "h2"
#' @param condition.minor.sep
#' @param condition.major.sep
#' @param col.calculation
#' @param col.choices
#' @param choices.char.sep
#' @param missing.default
#'
#' @return tibble or data.frame (same as data)
#' @export
#'
#' @examples
#' data <- dd_inst
#' data |> doc2dd(instrument.name = "evt", col.description = 3, col.condition = 4, col.subheader = 2, col.calculation = 5, col.choices = 6)
doc2dd <- function(data,
                   instrument.name,
                   col.variables = 1,
                   list.datetime.format = list(
                     date_dmy = "_dat[eo]$",
                     time = "_ti[md]e?$"
                   ),
                   col.description = NULL,
                   col.condition = NULL,
                   col.subheader = NULL,
                   subheader.tag = "h2",
                   condition.minor.sep = ",",
                   condition.major.sep = ";",
                   col.calculation = NULL,
                   col.choices = NULL,
                   choices.char.sep = "/",
                   missing.default = NA) {
  data <- data |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, c(""))))


  ## Defining the field name MANDATORY
  out <- data |>
    dplyr::mutate(
      field_name = dplyr::pick(col.variables) |> unlist()
    )

  if (is_missing(col.description)) {
    out <- out |>
      dplyr::mutate(
        field_label = field_name
      )
  } else {
    out <- out |>
      dplyr::mutate(
        field_label = dplyr::pick(col.description) |> unlist()
      )
  }


  if (!is_missing(col.subheader)) {
    out <- out |>
      dplyr::mutate(
        section_header = dplyr::pick(col.subheader) |> unlist() |> format_subheader(tag = subheader.tag)
      )
  }

  if (is_missing(col.choices)) {
    out <- out |>
      dplyr::mutate(
        choices = missing.default
      )
  } else {
    out <- out |>
      dplyr::mutate(
        choices = dplyr::pick(col.choices) |> unlist() |> char2choice(char.split = choices.char.sep)
      )
  }

  if (is_missing(col.choices)) {
    out <- out |>
      dplyr::mutate(
        calculations = missing.default
      )
  } else {
    out <- out |>
      dplyr::mutate(
        calculations = dplyr::pick(col.calculation) |> unlist() |> tolower() |> (\(.x) gsub("’", "'", .x))()
      )
  }


  out <- out |>
    dplyr::mutate(
      select_choices_or_calculations = dplyr::coalesce(calculations, choices),
      field_type = dplyr::case_when(!is.na(choices) ~ "radio",
        !is.na(calculations) ~ "calc",
        .default = "text"
      ),
      form_name = instrument.name
    )

  if (is_missing(col.condition)) {
    out <- out |>
      dplyr::mutate(
        branching_logic = missing.default
      )
  } else {
    out <- out |>
      dplyr::mutate(
        branching_logic = dplyr::pick(col.condition) |> unlist() |> char2cond(minor.split = condition.minor.sep, major.split = condition.major.sep)
      )
  }

  if (is.null(list.datetime.format)) {
    out <- out |>
      dplyr::mutate(
        text_validation_type_or_show_slider_number = missing.default
      )
  } else {
    out <- out |>
      dplyr::mutate(
        text_validation_type_or_show_slider_number = case_match_regex_list(
          field_name,
          list.datetime.format
        )
      )
  }

  out <- out |>
    dplyr::select(dplyr::any_of(names(REDCapCAST::redcapcast_meta)))

  out |>
    list(REDCapCAST::redcapcast_meta |> dplyr::slice(0)) |>
    dplyr::bind_rows() |>
    dplyr::select(names(REDCapCAST::redcapcast_meta))
}


#' Create two-column HTML table for data piping in REDCap instruments
#'
#' @param text descriptive text
#' @param variable variable to pipe
#'
#' @return character vector
#' @export
#'
#' @examples
#' create_html_table(text = paste("assessor", 1:2, sep = "_"), variable = c("[cpr]"))
#' create_html_table(text = c("CPR nummer"), variable = c("[cpr][1]", "[cpr][2]"))
create_html_table <- function(text, variable) {
  start <- '<table style="border-collapse: collapse; width: 100%;" border="0"> <tbody>'
  end <- "</tbody> </table>"

  # Extension would allow defining number of columns and specify styling
  items <- purrr::map2(text, variable, function(.x, .y) {
    glue::glue('<tr> <td style="width: 58%;"> <h5><span style="font-weight: normal;">{.x}<br /></span></h5> </td> <td style="width: 42%; text-align: left;"> <h5><span style="font-weight: bold;">{.y}</span></h5> </td> </tr>')
  })

  glue::glue(start, glue::glue_collapse(purrr::list_c(items)), end)
}

#' Title
#'
#' @param data character vector
#' @param tag character vector length 1
#' @param extra character vector
#'
#' @return
#' @export
#'
#' @examples
#' html_tag_wrap("Titel", tag = "div", extra = 'class="rich-text-field-label"')
#' html_tag_wrap("Titel", tag = "h2")
html_tag_wrap <- function(data, tag = "h2", extra = NULL) {
  et <- ifelse(is.null(extra), "", paste0(" ", extra))
  glue::glue("<{tag}{et}>{data}</{tag}>")
}


#' Sub-header formatting wrapper
#'
#' @param data character vector
#' @param tag character vector length 1
#'
#' @return character vector
#' @export
#'
#' @examples
format_subheader <- function(data, tag = "h2") {
  dplyr::if_else(is.na(data) | data == "",
    NA,
    data |>
      html_tag_wrap(tag = tag) |>
      html_tag_wrap(
        tag = "div",
        extra = 'class="rich-text-field-label"'
      )
  )
}


evt_clinical_data <- function(data) {
  REDCapR::redcap_read_oneshot(
    token = keyring::key_get("DAP_REDCAP_API"),
    redcap_uri = "https://redcap.au.dk/api/",
    records = dplyr::pull(data, forloebid),
    fields = c(
      "forloebid",
      "modafdankomstdato_basisske",
      "modafdankomsttid_basisske",
      "ageforloeb_patiente",
      "sex_patiente",
      "status_basisske",
      "bolig_basisske",
      "alkohol_basisske",
      "rygning_basisske",
      "ami2_basisske",
      "tidlapo2_basisske",
      "tidltci_basisske",
      "pad2_basisske",
      "diabetesdap_basisske",
      "atrieflimrendap_basisske",
      "hypertensiondap_basisske",
      "startlysedato_tromboly",
      "startlysetid_tromboly",
      "artpunktdato_trombekt",
      "artpunkttid_trombekt",
      "procafsldato_trombekt",
      "procafsltid_trombekt",
      "reperfusion_trombekt",
      "tidreperfdato_trombekt",
      "tidreperftid_trombekt",
      "tici_trombekt",
      "perforation_trombekt",
      "procintrahaem_trombekt",
      "procsah_trombekt",
      "nyemboli_trombekt",
      "procandet_trombekt",
      "procandettekst_trombekt",
      "mrsfoer_basisske",
      "mdmrs3_tremdrop",
      "nihss_basisske",
      "nihss24t_tromboly",
      # "nihssudskrivning_trombekt", # Only very few observations
      "kendtsymptomdebut_basisske",
      "symptdebexactdato_basisske",
      "symptdebexacttid_basisske",
      # Lokalisation
      "trombelokcca_trombekt",
      "trombelokica_trombekt",
      "trombelokica_t_trombekt",
      "trombelokmca1_trombekt",
      "trombelokmca2_trombekt",
      "trombelokaca_trombekt",
      "trombelokva_trombekt",
      "trombelokba_trombekt",
      "trombelokpca_trombekt",
      "trombelokingen_trombekt",
      "trombelokcervical_trombekt",
      # Tider
      "ankneuroraddato_trombekt",
      "ankneuroradtid_trombekt",
      "ankomstevttrombektomidato_trombekt",
      "ankomstevttrombektomitid_trombekt",
      "ankomstevtctmrdato_trombekt",
      "ankomstevtctmrtid_trombekt",
      "udskrdato_basisske",
      "udskrsygkode_basisske",
      "status_basisske",
      "datodoed_tremdrop",
      "doedaarsag_tremdrop",
      "lysedoed_tremdrop"
    )
  )$data |>
    dplyr::select(-redcap_repeat_instrument, -redcap_repeat_instance)
}


#' Supposes a systematic naming of paired data and time variables
#'
#' @param data
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
merge_date_time <- function(data, pattern = "(date|time)$", keep = FALSE) {
  ndx <- grep(pattern, names(data))

  nms <- names(data)[ndx]

  fct <- unique(Reduce(c, strsplit(nms, pattern)))

  ds <- purrr::map(fct, \(.x){
    df <- dplyr::select(data, tidyselect::starts_with(.x))

    tibble::tibble(as.POSIXct(paste(df[[1]], df[[2]]), format = "%Y-%m-%d %H:%M:%S")) |>
      setNames(paste0(.x, "datetime"))
  }) |> dplyr::bind_cols()

  ds_diff <- names(ds) |>
    strsplit("_") |>
    purrr::map(\(.x) purrr::pluck(.x, 1)) |>
    unique() |>
    (\(.x){
      Reduce(c, .x)
    })() |>
    purrr::map(\(.x){
      df <- dplyr::select(ds, tidyselect::starts_with(.x))

      tibble::tibble(difftime(df[[2]], df[[1]], units = "hours")) |>
        setNames(paste0(.x, "_timediff"))
    }) |>
    dplyr::bind_cols()

  if (keep) {
    out <- dplyr::bind_cols(list(data, ds, ds_diff))
  } else {
    out <- dplyr::bind_cols(list(data[-ndx], ds, ds_diff))
  }
  out
}

evt_redcap_format_dic <- function() {
  list(
    pneumonia = function(data) {
      dplyr::case_match(data,
        "Yes" ~ TRUE,
        "No" ~ FALSE,
        .default = NA
      )
    },
    extubation_novent = function(data) {
      factor(
        dplyr::case_match(data,
          "Yes" ~ "Smooth",
          "No" ~ "Event",
          .default = NA
        ),
        levels = c("Smooth", "Event")
      )
    },
    pacu_saox_first = function(data) {
      as.numeric(data)
    },
    pacu_saox_last = function(data) {
      as.numeric(data)
    },
    pacu_mabp_first = function(data) {
      as.numeric(data)
    },
    pacu_mabp_last = function(data) {
      as.numeric(data)
    },
    pacu_ox_req = function(data) {
      as.numeric(data)
    }
  )
}


dap_redcap_format_dic <- function() {
  list(
    mrsfoer_basisske = function(data) {
      dplyr::case_match(data,
        2 ~ 0,
        3 ~ 1,
        4 ~ 2,
        5 ~ 3,
        6 ~ 4,
        7 ~ 5,
        .default = NA
      )
    },
    mdmrs3_tremdrop = function(data) {
      dplyr::case_match(data,
        0 ~ 0,
        1 ~ 1,
        2 ~ 2,
        3 ~ 3,
        4 ~ 4,
        5 ~ 5,
        6 ~ 6,
        .default = NA
      )
    },
    status_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ FALSE,
        2 ~ TRUE,
        .default = NA
      )
    },
    bolig_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ FALSE,
        2 ~ TRUE,
        .default = NA
      )
    },
    alkohol_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ FALSE,
        2 ~ TRUE,
        .default = NA
      )
    },
    rygning_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ "smoking",
        2 ~ "previous",
        3 ~ "never",
        .default = NA
      ) |> factor(levels = c("never", "smoking", "previous"))
    },
    diabetesdap_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    hypertensiondap_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    atrieflimrendap_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    ami2_basisske = function(data) {
      dplyr::case_match(data,
        1 ~ TRUE,
        3 ~ FALSE,
        .default = NA
      )
    },
    tici_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ "0",
        2 ~ "1",
        3 ~ "2a",
        4 ~ "2b",
        5 ~ "2c",
        6 ~ "3"
      ) |> factor()
    },
    tidltci_basisske = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2:3 ~ FALSE,
        .default = NA
      )
    },
    tidlapo2_basisske = function(data) {
      dplyr::case_match(
        data,
        1:2 ~ TRUE,
        3:4 ~ FALSE,
        .default = NA
      )
    },
    pad2_basisske = function(data) {
      dplyr::case_match(
        data,
        1:2 ~ TRUE,
        3:4 ~ FALSE,
        .default = NA
      )
    },
    sex_patiente = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        0 ~ FALSE,
        .default = NA
      )
    },
    ageforloeb_patiente = function(data) {
      as.numeric(
        data
      )
    },
    reperfusion_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    perforation_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    procintrahaem_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    procsah_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    nyemboli_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    procandet_trombekt = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    kendtsymptomdebut_basisske = function(data) {
      dplyr::case_match(
        data,
        1 ~ TRUE,
        2 ~ FALSE,
        .default = NA
      )
    },
    nihss24t_tromboly = function(data) {
      as.numeric(data)
    },
    doedaarsag_tremdrop = function(data) {
      dplyr::case_match(
        data,
        1~"Hjerneinfarkt",
        2~"Hjerneblødning",
        3~"Hjerneinfarkt og blødning ingen specifikation",
        4~"Myokardie infarkt",
        5~"Lungeemboli",
        6~"Lungebetændelse",
        7~"Anden vaskulær årsag",
        8~"Andet",
        9~"Ukendt",
        .default = NA
      )
    },
    datodoed_tremdrop = function(data) {
      as.Date(data)
    }
  )
}


#' Format variables specified by function in list and named from vector, optionally keep old
#'
#' @param data
#' @param new.names.vec
#' @param keep
#' @param new.name.default
#' @param fmt.lst
#'
#' @return
#' @export
#'
#' @examples
format_all <- function(data, new.names.vec = var_short(), keep = FALSE, new.name.default = "{.nm}_fmt", fmt.lst = dap_redcap_format_dic()) {
  data |>
    purrr::imap(\(.ds, .nm){
      if (.nm %in% names(fmt.lst)) {
        .f <- fmt.lst[[.nm]]
        .lbl <- subset_named_labels(.nm, labels.raw = new.names.vec)
        .new_name <- ifelse(is.na(.lbl), glue::glue(new.name.default), .lbl)
        if (keep) {
          tibble::tibble(.ds, .f(.ds)) |> setNames(c(.nm, .new_name))
        } else {
          tibble::tibble(.f(.ds)) |> setNames(.new_name)
        }
      } else {
        tibble::tibble(.ds) |> setNames(.nm)
      }
    }) |>
    dplyr::bind_cols()
}


var_labels <- function() {
  c(
    age = "Age",
    female = "Female sex",
    simple_score = "SVD score",
    bmi = "Body mass index",
    smoking = "Smoking",
    never = "Smoking (never)",
    current = "Smoking (current)",
    prior = "Smoking (prior)",
    living_alone = "Living alone",
    alc_above = "High alcohol consumption",
    hypertension = "Hypertension",
    diabetes = "Diabetes",
    afib = "Atrial fibrillation",
    pad = "Peripheral arterial disease",
    tci = "Previous TIA",
    ami = "Previous MI",
    tpa = "Treated with tPA",
    ais = "Previous AIS",
    evt = "Treated with EVT",
    # reg_any_perf,
    # rtreat = "Study group allocation",
    rtreat_placebo = "Placebo trial treatment",
    pase_0 = "Pre-stroke PASE score",
    pase_0_high = "High pre-stroke PA",
    pase_0_q = "Pre-stroke PA quartile",
    pase_4 = "6 months post-stroke PASE score",
    # pase_change,
    nihss = "Admission NIHSS",
    nihss_24h = "24H NIHSS",
    # soc_status,
    soc_status_work = "Employed",
    soc_status_nowork = "Not employed",
    fam_indk = "Family income group",
    fam_indk_hl = "Lower family income",
    fam_indk_high = "Higher family income",
    fam_indk_low = "Lower family income",
    edu_level = "Educational level group",
    edu_level_hl = "Lower educational level",
    edu_high = "Higher educational level",
    edu_low = "Low educational level",
    who_4 = "WHO-5 score 6 months post-stroke",
    mdi_4 = "MDI score 6 months post-stroke",
    mrs_4_above1 = "mRS > 1 at 6 months post-stroke",
    mfi_gen_4 = "General fatigue (MFI domain) 6 months post-stroke",
    time = "Time",
    status = "Status",
    event.include = "Include event",
    who_0 = "Pre-stroke WHO-5 score",
    mrs_0_above0 = "Pre-stroke mRS > 0",
    mrs_eos = "mRS at End of Study",
    mrs_3mdr = "mRS at 3 months",
    mrs_pre = "Pre-stroke mRS",
    pneumonia = "Pneumonia treatment",
    pacu_timediff = "Time at PACU (hours)",
    icu_timediff = "Time at ICU (hours)",
    tici = "TICI classification",
    compl_any = "Complications during EVT",
    art2rep_timediff = "Time from artery puncture to reperfusion (minutes)",
    ank2rep_timediff = "Time from arrival to reperfusion (minutes)",
    onset2rep_timediff = "Onset to reperfusion (minutes)",
    art2procend_timediff = "Time from artery puncture to procedure termination (minutes)",
    angio2art_timediff = "Time from angio suite arrival to artery puncture (minutes)",
    mr2angio_timediff = "Time from MR arrival to angio suite arrival (minutes)",
    pacu_delay = "Reason for PACU delay",
    icu_direct = "Reason for direct ICU transfer",
    pacu_mabp_first = "First MABP in PACU",
    pacu_mabp_last = "Last MABP in PACU",
    pacu_saox_first = "First SaOx in PACU",
    pacu_saox_last = "Last SaOx in PACU",
    pacu_ox_req = "Avg. oxygen requirement in PACU",
    pacu_events_type = "Events in PACU",
    pacu_extend_type = "Reason for PACU stay extension",
    ward_events_type = "Events at the stroke ward (within 24 h of transfer)",
    ward_events_other = "Ward events, other",
    location = "Vascular location of embolus",
    admission_timediff = "Hospital admission (days)",
    mors_hospital = "Mors"
  )
}

var_short <- function() {
  c(
    hypertensiondap_basisske = "hypertension",
    rygning_basisske = "smoking",
    diabetesdap_basisske = "diabetes",
    mrsfoer_basisske = "mrs_pre",
    mdmrs3_tremdrop = "mrs_3mdr",
    status_basisske = "living_alone",
    bolig_basisske = "nursing_home",
    alkohol_basisske = "alc_above",
    atrieflimrendap_basisske = "afib",
    ami2_basisske = "ami",
    tici_trombekt = "tici",
    tidltci_basisske = "tci",
    tidlapo2_basisske = "ais",
    pad2_basisske = "pad",
    ageforloeb_patiente = "age",
    sex_patiente = "female",
    pneumonia = "pneumonia",
    reperfusion_trombekt = "reperfusion_evt",
    perforation_trombekt = "compl_evt_perforation",
    procintrahaem_trombekt = "compl_evt_ich",
    procsah_trombekt = "compl_evt_sah",
    nyemboli_trombekt = "compl_evt_embulus",
    procandet_trombekt = "compl_evt_other",
    procandettekst_trombekt = "compl_evt_other_text",
    kendtsymptomdebut_basisske = "onset_known",
    trombelokcca_trombekt = "location_cca",
    trombelokica_trombekt = "location_ica",
    trombelokica_t_trombekt = "location_ica_t",
    trombelokmca1_trombekt = "location_mca1",
    trombelokmca2_trombekt = "location_mca2",
    trombelokaca_trombekt = "location_aca",
    trombelokva_trombekt = "location_va",
    trombelokba_trombekt = "location_ba",
    trombelokpca_trombekt = "location_pca",
    trombelokingen_trombekt = "location_ingen",
    trombelokcervical_trombekt = "location_cervical",
    extubation_novent = "extubation_novent",
    pacu_saox_first = "pacu_saox_first",
    pacu_saox_last = "pacu_saox_last",
    pacu_mabp_first = "pacu_mabp_first",
    pacu_mabp_last = "pacu_mabp_last",
    pacu_ox_req = "pacu_ox_req",
    nihss24t_tromboly = "nihss_24h",
    doedaarsag_tremdrop = "mors_cause",
    datodoed_tremdrop = "mors_date"
  )
}


#' Subset labels
#'
#' @param data
#' @param labels.raw
#'
#' @return character vector
#' @export
#'
subset_named_labels <- function(data, labels.raw = var_labels()) {
  labels.raw[match(data, names(labels.raw))]
}

#' Assign labels to data.frame or tibble
#'
#' @param data
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
assign_labels <- function(data, labels) {
  # data |> labelled::set_variable_labels(labels)

  labelled::var_label(data) <- labels

  data
}

#' Flexible labelling using labelled for nicer tables
#'
#' @param data data set
#'
#' @return
#' @export labelled data.frame/tibble
#'
#' @examples
#' data <- targets::tar_read(df_pred_data)
#' data <- data |> dplyr::mutate(test = "test")
#' data |>
#'   labelling_data() |>
#'   labelled::var_label()
labelling_data <- function(data, label.list = var_labels()) {
  labs <- subset_named_labels(names(data), label.list)
  labs[is.na(labs)] <- names(data)[is.na(labs)]

  data |> assign_labels(labels = labs)
}

get_label <- function(vars = "pase_0") {
  subset_named_labels(vars, var_labels())
}

get_set_label <- function(set) {
  get_label(get_var_vec(set))
}

merge_dato_tid <- function(data, pattern = "(dato|tid)_", keep = FALSE) {
  ndx <- grep(pattern, names(data))

  nms <- names(data)[ndx]

  fct <- unique(Reduce(c, purrr::map(strsplit(nms, pattern), \(.x) purrr::pluck(.x, .1))))

  ds <- purrr::map(fct, \(.x){
    df <- dplyr::select(data, tidyselect::starts_with(.x)) |>
      dplyr::select(tidyselect::contains("dato_"), tidyselect::contains("tid_"))

    if (ncol(df) == 1) {
      out <- tibble::tibble(format(df[[1]], "%Y-%m-%d %H:%M:%S"))
    } else {
      out <- tibble::tibble(as.POSIXct(
        paste(
          df[[1]],
          format(df[[2]], "%H:%M:%S")
        ),
        format = "%Y-%m-%d %H:%M:%S"
      ))
    }
    out |>
      setNames(paste0(.x, "_datetime"))
  }) |> dplyr::bind_cols()

  if (keep) {
    out <- dplyr::bind_cols(list(data, ds))
  } else {
    out <- dplyr::bind_cols(list(data[-ndx], ds))
  }
  out
}


fix_labels <- function(data, variable.labels = var_labels()) {
  cls <- class(data)

  labels.old <- data[[1]][["variable"]][data[[1]][["row_type"]] == "label"]

  labels.new <- labels.old |>
    subset_named_labels(variable.labels) |>
    unname()

  data[[1]][["label"]][data[[1]][["row_type"]] == "label"] <-
    ifelse(is.na(labels.new), labels.old, labels.new)

  class(data) <- cls

  data
}

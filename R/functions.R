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

# record_update.R
# Logs the GOV.UK page's latest update timestamp and last-row data from the arrivals table

library(httr2)
library(rvest)
library(lubridate)
library(dplyr)
library(stringr)

URL <- paste0(
  "https://www.gov.uk/government/publications/",
  "migrants-detected-crossing-the-english-channel-in-small-boats/",
  "migrants-detected-crossing-the-english-channel-in-small-boats-last-7-days"
)
CSV_FILE <- "smallboats_updates.csv"

# --- 1. Fetch HTML ---

response <- request(URL) |>
  req_user_agent("SmallBoatsUpdateMonitor/1.0") |>
  req_perform() |>
  resp_check_status()

html_text <- resp_body_string(response)
page <- read_html(html_text)

# --- 2. Extract GOV.UK update timestamp ---

timestamp_node <- page |>
  html_element("meta[name='govuk:updated-at']")

timestamp <- html_attr(timestamp_node, "content")

if (length(timestamp) == 0 || is.na(timestamp) || !nzchar(timestamp)) {
  stop("No govuk:updated-at timestamp found.")
}

ts_utc <- ymd_hms(timestamp, tz = "UTC", quiet = TRUE)

if (is.na(ts_utc)) {
  stop(paste("Could not parse GOV.UK timestamp:", timestamp))
}

ts_london <- with_tz(ts_utc, "Europe/London")

# --- 3. Extract arrivals table ---

table_node <- page |>
  html_element("table")

if (inherits(table_node, "xml_missing")) {
  stop("No table found on the GOV.UK page.")
}

table_data <- table_node |>
  html_table(convert = FALSE)

if (nrow(table_data) == 0 || ncol(table_data) < 4) {
  stop("The arrivals table is empty or has an unexpected structure.")
}

# Standardise column names
expected_names <- c(
  "Date",
  "Migrants_arrived",
  "Boats_arrived",
  "Boats_uncontrolled_landings",
  "Notes"
)

if (ncol(table_data) <= length(expected_names)) {
  names(table_data) <- expected_names[seq_len(ncol(table_data))]
} else {
  names(table_data) <- c(
    expected_names,
    paste0("Extra_", seq_len(ncol(table_data) - length(expected_names)))
  )
}

# Clean whitespace
table_data <- table_data |>
  mutate(across(everything(), ~ str_trim(as.character(.x))))

# --- 4. Take the latest row ---

last_row <- tail(table_data, 1)

# Handle optional Notes column
notes_value <- if ("Notes" %in% names(last_row)) {
  last_row$Notes
} else {
  NA_character_
}

# --- 5. Build output entry ---

entry <- data.frame(
  date_checked = format(
    with_tz(now(), "Europe/London"),
    "%Y-%m-%d %H:%M:%S"
  ),

  govuk_updated_at_utc = format(
    ts_utc,
    "%Y-%m-%d %H:%M:%S"
  ),

  govuk_updated_at_london = format(
    ts_london,
    "%Y-%m-%d %H:%M:%S"
  ),

  table_date = last_row$Date,
  migrants_arrived = last_row$Migrants_arrived,
  boats_arrived = last_row$Boats_arrived,
  boats_uncontrolled_landings = last_row$Boats_uncontrolled_landings,
  notes = notes_value,

  stringsAsFactors = FALSE
)

# --- 6. Append only if data has changed ---

if (!file.exists(CSV_FILE) || file.info(CSV_FILE)$size == 0) {

  write.csv(entry, CSV_FILE, row.names = FALSE)

  cat("Created new CSV file.\n")

} else {

  existing <- tryCatch(
    read.csv(CSV_FILE, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )

  comparison_columns <- c(
    "table_date",
    "migrants_arrived",
    "boats_arrived",
    "boats_uncontrolled_landings"
  )

  new_key <- paste(
    as.character(entry[1, comparison_columns]),
    collapse = "|"
  )

  old_key <- if (
    nrow(existing) > 0 &&
    all(comparison_columns %in% names(existing))
  ) {
    paste(
      as.character(tail(existing, 1)[1, comparison_columns]),
      collapse = "|"
    )
  } else {
    NA_character_
  }

  if (nrow(existing) == 0 || is.na(old_key) || new_key != old_key) {

    write.table(
      entry,
      CSV_FILE,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE,
      append = TRUE
    )

    cat("New data recorded.\n")

  } else {

    cat("No change in latest arrivals data. CSV not updated.\n")
  }
}

# --- 7. Confirmation ---

cat(
  "GOV.UK updated:",
  format(ts_london, "%Y-%m-%d %H:%M:%S %Z"),
  "\n"
)

cat(
  "Latest table data:",
  last_row$Date,
  "|",
  last_row$Migrants_arrived,
  "migrants |",
  last_row$Boats_arrived,
  "boats\n"
)

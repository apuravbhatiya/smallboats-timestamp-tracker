# record_update.R
# Logs the GOV.UK page's latest update timestamp and last-row data from the arrivals table

library(httr2)
library(rvest)
library(lubridate)
library(dplyr)
library(stringr)

URL <- "https://www.gov.uk/government/publications/migrants-detected-crossing-the-english-channel-in-small-boats/migrants-detected-crossing-the-english-channel-in-small-boats-last-7-days"
CSV_FILE <- "smallboats_updates.csv"

# --- 1. Fetch HTML and extract timestamp ---
html_text <- request(URL) |> req_perform() |> resp_body_string()
page <- read_html(html_text)

timestamp <- page |>
  html_element("meta[name='govuk:public-updated-at']") |>
  html_attr("content")

if (is.na(timestamp) || is.null(timestamp)) stop("No govuk:public-updated-at tag found.")

ts_utc <- ymd_hms(timestamp, tz = "UTC")
ts_london <- with_tz(ts_utc, "Europe/London")

# --- 2. Extract the arrivals table ---
table_node <- page |> html_element("table")
if (is.na(table_node)) stop("No table found on the page.")

table_data <- table_node |> html_table()

# Ensure consistent column names
names(table_data) <- str_trim(names(table_data))
colnames(table_data) <- c("Date", "Migrants_arrived", "Boats_arrived",
                          "Boats_uncontrolled_landings", "Notes")[1:ncol(table_data)]

# --- 3. Take the last row (most recent date) ---
last_row <- tail(table_data, 1)

# --- 4. Build entry with both timestamp and table data ---
entry <- data.frame(
  date_checked = format(with_tz(now(), "Europe/London"), "%Y-%m-%d %H:%M:%S"),
  govuk_public_updated_at_utc = format(ts_utc, "%Y-%m-%d %H:%M:%S"),
  govuk_public_updated_at_london = format(ts_london, "%Y-%m-%d %H:%M:%S"),
  table_date = last_row$Date,
  migrants_arrived = last_row$Migrants_arrived,
  boats_arrived = last_row$Boats_arrived,
  boats_uncontrolled_landings = last_row$Boats_uncontrolled_landings,
  notes = if ("Notes" %in% names(last_row)) last_row$Notes else NA,
  stringsAsFactors = FALSE
)

# --- 5. Append only if new (based on table_date or data change) ---
if (!file.exists(CSV_FILE) || file.info(CSV_FILE)$size == 0) {
  write.csv(entry, CSV_FILE, row.names = FALSE)
} else {
  existing <- tryCatch(read.csv(CSV_FILE, stringsAsFactors = FALSE), error = function(e) data.frame())
  
  # Detect if the last recorded row already matches this one
  if (nrow(existing) == 0 ||
      !(entry$table_date == tail(existing$table_date, 1) &
        entry$migrants_arrived == tail(existing$migrants_arrived, 1) &
        entry$boats_arrived == tail(existing$boats_arrived, 1))) {
    
    write.table(entry, CSV_FILE, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  }
}

cat("Recorded:", format(ts_london, "%Y-%m-%d %H:%M:%S %Z"),
    "| Latest data →", last_row$Date, last_row$Migrants_arrived, "migrants,", last_row$Boats_arrived, "boats\n")

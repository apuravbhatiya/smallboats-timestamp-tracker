# record_update.R
# Logs the govuk:public-updated-at timestamp once per day

library(httr2)
library(rvest)
library(lubridate)

URL <- "https://www.gov.uk/government/publications/migrants-detected-crossing-the-english-channel-in-small-boats/migrants-detected-crossing-the-english-channel-in-small-boats-last-7-days"
CSV_FILE <- "smallboats_updates.csv"

# Fetch the live HTML page
html_text <- request(URL) |> req_perform() |> resp_body_string()
page <- read_html(html_text)

# Extract <meta name="govuk:public-updated-at">
timestamp <- page |>
  html_element("meta[name='govuk:public-updated-at']") |>
  html_attr("content")

if (is.na(timestamp) || is.null(timestamp)) {
  stop("No govuk:public-updated-at tag found.")
}

# Convert to POSIXct (UTC and London time)
ts_utc <- ymd_hms(timestamp, tz = "UTC")
ts_london <- with_tz(ts_utc, "Europe/London")

# Build one-row data frame
entry <- data.frame(
  date_checked = format(with_tz(now(), "Europe/London"), "%Y-%m-%d %H:%M:%S"),
  govuk_public_updated_at_utc = format(ts_utc, "%Y-%m-%d %H:%M:%S"),
  govuk_public_updated_at_london = format(ts_london, "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE
)

# Append or create the CSV
if (!file.exists(CSV_FILE)) {
  write.csv(entry, CSV_FILE, row.names = FALSE)
} else {
  existing <- read.csv(CSV_FILE, stringsAsFactors = FALSE)
  if (!(entry$govuk_public_updated_at_utc %in% existing$govuk_public_updated_at_utc)) {
    write.table(entry, CSV_FILE, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  }
}

cat("Recorded GOV.UK update:", format(ts_london, "%Y-%m-%d %H:%M:%S %Z"), "\n")

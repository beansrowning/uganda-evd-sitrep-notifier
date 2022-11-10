library(rvest)
library(dplyr)
library(readr)
library(lubridate)
library(urltools)
library(cli)

# === globals ===================================================
ug_website_url <- "https://www.afro.who.int/countries/publications?country=879"
time_stamp <- format(with_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %T %Z")
previous_data_path <- file.path("data", "sitreps.csv")

source("R/funcs.R")

# === Scrape operation ==========================================
# Initializing table
sitrep_table <- tibble(
  name = character(0),
  url = character(0)
)

pg_num <- 0

repeat {
  query_url <- param_set(ug_website_url, "page", pg_num)

  out_table <- try(scrape_moh_sitrep_by_page(query_url), silent = TRUE)

  # Run until we can't find any more sitreps, or the function fails
  if (inherits(out_table, "try-error") || !NROW(out_table)) {
    break
  }
  # Bind new rows to existing data
  sitrep_table <- bind_rows(
    sitrep_table,
    out_table
  )
  # Iterate to next page
  pg_num <- pg_num + 1
}

# Append base URL to a link to construct full URL
# Then pull the actual PDF link
# and append a scrape time
if (NROW(sitrep_table)) {
  sitrep_table <- sitrep_table |>
    mutate(
      scrape_time = with_tz(Sys.time(), "America/New_York"),
      url = paste0(scheme(ug_website_url), "://", domain(ug_website_url), url),
      pdf_url = vapply(url, scrape_pdf_link_from_page, character(1))
    )
}

# === Append existing data, determine if any new posts ========================
# Try pulling existing data, if we have any
if (file.exists(previous_data_path)) {
  last_data <- read_csv(previous_data_path) |>
  mutate(
    scrape_time = as.POSIXct(scrape_time)
  )
} else {
  # If no data, just return an empty data frame
  # in the same structure as our pull
  last_data <- sitrep_table |>
    filter(FALSE)
}

# Compute whether new sitreps were identified
new_sitreps <- sitrep_table |>
  anti_join(last_data, by = c("name", "url"))

n_new_sitreps <- NROW(new_sitreps)

data_changed <- n_new_sitreps > 0

# Combine our new scraped observation with the previous data
# And if anything changed, signal an update to the pin and alert via email

out_table <- new_sitreps |>
  bind_rows(last_data)

# === Write out changes ============================
if (data_changed) {
  write_csv(
    out_table,
    previous_data_path
  )

  latest_sitrep_name <- sitrep_table |>
    slice(1) |>
    pull(name)
  
  latest_sitrep_url <- sitrep_table |>
    slice(1) |>
    pull(pdf_url)
  
  if (latest_sitrep_url == "<no PDF link found>") {
    latest_sitrep_url <- sitrep_table |>
      slice(1) |>
      pull(url)
  }

  new_sitrep_str <- pluralize("As of {time_stamp}, {n_new_sitreps} new #Uganda #Ebola Situation Report{?s} {?was/were} posted on the WHO AFRO (@WHOAFRO) Website. The latest one is {latest_sitrep_name}, which you can find here: {latest_sitrep_url}.")
  cat(new_sitrep_str, file = "toot.txt")
}

# A function to scrape the hyperlinks to each MoH Sitrep on the main page
scrape_moh_sitrep_by_page <- function(page) {
  page_con <- url(page, "rb")

  on.exit(close(page_con))

  scraped_page <- read_html(page_con)

  sitrep_urls <- scraped_page %>%
    html_elements(xpath="//div[contains(@class, 'title-wrapper')]") %>%
    html_elements("a") %>%
    html_attr("href")

  sitrep_names <- scraped_page %>%
    html_elements(xpath="//div[contains(@class, 'title-wrapper')]") %>%
    html_elements("a") %>%
    html_text()

  is_evd_sitrep <- function(x) {
    grepl(".*ebola.*sitrep.*", x, ignore.case = TRUE)
  }

  sitrep_idx <- is_evd_sitrep(sitrep_names)

  out_table <- tibble(
    name = sitrep_names[sitrep_idx],
    url = sitrep_urls[sitrep_idx]
  )

  return(out_table)
}

# A function to extract the PDF link from a Sitrep page provided from the AFRO Main page
scrape_pdf_link_from_page <- function(page) {
  scraped_page <- read_html(page)

  pdf_urls <- scraped_page %>%
    html_elements(xpath="//a[contains(@type, 'application/pdf')]") %>%
    html_attr("href")
  
  # Handle condition that no PDF link was found
  if (!length(pdf_urls)) {
    pdf_urls <- "<no PDF link found>"
  }

  return(pdf_urls)
}
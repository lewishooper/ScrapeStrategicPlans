# hospital Card Yaml utlities

# to change order 
new_card <- list(
  FAC                = FAC,
  name               = name,
  hospital_type      = hospital_type,
  url                = url,
  pattern            = pattern,
  expected_executives = expected_executives,
  html_structure     = html_structure
)


# to add additional fields
# extract it
scrape_method <- extract_if_present(card, "scrape_method")

#then insert 

if (!is.null(scrape_method)) {
  new_card$scrape_method <- scrape_method
}

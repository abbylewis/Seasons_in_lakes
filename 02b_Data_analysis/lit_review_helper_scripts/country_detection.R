#Vectors to use
canadian_provinces <- c("Alberta","British Columbia","Manitoba","New Brunswick",
                        "Newfoundland and Labrador","Nova Scotia","Ontario",
                        "Prince Edward Island","Quebec","Saskatchewan")

uk_countries <- c("England","Scotland","Wales","Northern Ireland")

russia <- c("Siberia")

china <- c("Taihu")

# Vectorize country detection
detect_country <- function(text) {
  # Combine all country names into one regex
  country_pattern <- paste0("\\b(", paste(countries$country_lower, collapse = "|"), ")\\b")
  iso3_pattern    <- paste0("\\b(", paste(countries_iso3$iso3_lower, collapse = "|"), ")\\b")
  
  # Extract country name matches
  matches <- unique(str_extract_all(text, regex(country_pattern, ignore_case = TRUE))[[1]])
  
  # Extract ISO3 matches if no country name matched
  if (length(matches) == 0) {
    matches <- str_extract_all(text, regex(iso3_pattern, ignore_case = TRUE)[[1]])
    if (length(matches) == 0) return(NA_character_)
    idx <- match(matches, countries_iso3$iso3_lower)
    matches <- countries_iso3$country[idx]
  } else {
    idx <- match(matches, countries$country_lower)
    matches <- countries$country[idx]
  }
  
  # Return
  if (length(matches) > 1) return("Multi")
  if (length(matches) == 1) return(matches)
  
  return(NA_character_)
}

detect_state <- function(text) {
  # Use a single regex per group, check for presence
  us_found <- any(stri_detect_regex(text, paste0("\\b(", paste(tolower(state.name), collapse = "|"), ")\\b")))
  ca_found <- any(stri_detect_regex(text, paste0("\\b(", paste(tolower(canadian_provinces), collapse = "|"), ")\\b")))
  uk_found <- any(stri_detect_regex(text, paste0("\\b(", paste(tolower(uk_countries), collapse = "|"), ")\\b")))
  ru_found <- any(stri_detect_regex(text, paste0("\\b(", paste(tolower(russia), collapse = "|"), ")\\b")))
  ch_found <- any(stri_detect_regex(text, paste0("\\b(", paste(tolower(china), collapse = "|"), ")\\b")))
  
  total_hits <- us_found + ca_found + uk_found
  
  if (total_hits > 1) return("Multi")
  if (us_found) return("United States")
  if (ca_found) return("Canada")
  if (uk_found) return("United Kingdom")
  if (ru_found) return("Russia")
  if (ch_found) return("China")
  
  return(NA_character_)
}
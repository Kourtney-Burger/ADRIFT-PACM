library(DBI)
library(RSQLite)
library(dplyr)
library(purrr)



read_pg_events <- function(
    db_file,
    deployment_codes,
    event_table_pattern = "OfflineEvents"
) {
  
  # Extract the deployment number from the database filename.
  # This assumes the relevant deployment number is the last
  # three-digit sequence in the filename.
  deployment_number <- stringr::str_extract(
    tools::file_path_sans_ext(basename(db_file)),
    "\\d{3}$"
  )
  
  if (is.na(deployment_number)) {
    warning(
      "Could not extract a three-digit deployment number from: ",
      basename(db_file)
    )
    deployment_code <- NA_character_
    
  } else {
    
    possible_matches <- deployment_codes[
      stringr::str_detect(
        deployment_codes,
        paste0("CALCURCEAS_", deployment_number, "$")
      )
    ]
    
    if (length(possible_matches) == 1) {
      deployment_code <- possible_matches
      
    } else if (length(possible_matches) == 0) {
      warning(
        "No CALCURCEAS deployment code found for database: ",
        basename(db_file),
        " (deployment number ", deployment_number, ")"
      )
      deployment_code <- NA_character_
      
    } else {
      stop(
        "Multiple CALCURCEAS deployment codes matched database: ",
        basename(db_file),
        "\nMatches: ",
        paste(possible_matches, collapse = ", ")
      )
    }
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  event_tables <- DBI::dbListTables(con) |>
    grep(event_table_pattern, x = _, value = TRUE)
  
  if (length(event_tables) == 0) {
    warning("No event table found in: ", basename(db_file))
    return(NULL)
  }
  
  purrr::map_dfr(event_tables, function(tbl) {
    
    events <- DBI::dbReadTable(con, tbl)
    
    events |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character),
        deployment_code = deployment_code,
        db_file = basename(db_file),
        table = tbl,
        .before = 1
      )
  })
}


###SLE DATA

# Function to fetch and pivot SLE data
fetch_and_pivot_sle <- function(con, table_name, result_ids) {
  query <- glue::glue("
    SELECT [participantidentifier], [resultidentifier], [answers]
    FROM [dbo].[{table_name}]
    WHERE [participantidentifier] IN (
      SELECT [participantidentifier]
      FROM [dbo].[complete_phq]
    )
    AND [resultidentifier] IN ({paste0(\"'\", result_ids, \"'\", collapse = ', ')})
    AND [answers] IS NOT NULL;
  ")
  
  result <- dbSendQuery(con, query)
  data <- dbFetch(result)
  dbClearResult(result)  
  
  pivoted <- data %>%
    pivot_wider(names_from = resultidentifier, values_from = answers)
  
  return(pivoted)
}

baseline_ids <- c("Events1", "Event3", "Event4", "Events")
q1_ids <- paste0(c("Events1", "Event3", "Event4", "Events"), "_1")
q2_ids <- paste0(c("Events1", "Event3", "Event4", "Events"), "_2")
q3_ids <- paste0(c("Events1", "Event3", "Event4", "Events"), "_3")
q4_ids <- paste0(c("Events1", "Event3", "Event4", "Events"), "_4")

baseline_sle_pivoted_data <- fetch_and_pivot_sle(con, "DepressionAnalysis", baseline_ids)
q1_sle_pivoted_data <- fetch_and_pivot_sle(con, "Quarter1Survey", q1_ids)
q2_sle_pivoted_data <- fetch_and_pivot_sle(con, "Quarter2Survey", q2_ids)
q3_sle_pivoted_data <- fetch_and_pivot_sle(con, "Quarter3Survey", q3_ids)
q4_sle_pivoted_data <- fetch_and_pivot_sle(con, "Quarter4Survey", q4_ids)

calculate_sle <- function(data, suffix = "") {
  events1 <- paste0("Events1", suffix)
  event3 <- paste0("Event3", suffix)
  event4 <- paste0("Event4", suffix)
  events <- paste0("Events", suffix)
  
  data %>%
    mutate(SLE = case_when(
      .data[[events1]] == 10 ~ "Yes",
      .data[[events1]] %in% 1:9 ~ "Yes",
      !is.na(.data[[event3]]) & .data[[event3]] == 11 ~ "Yes",
      !is.na(.data[[event4]]) & .data[[event4]] == 12 ~ "No",
      !is.na(.data[[events]]) & sapply(strsplit(.data[[events]], "\\|"), function(x) any(as.numeric(x) %in% 1:9)) ~ "Yes",
      is.na(.data[[events1]]) & is.na(.data[[event3]]) & is.na(.data[[event4]]) & is.na(.data[[events]]) ~ NA_character_,
      TRUE ~ "No"
    ))
}

baseline_sle_analysis <- calculate_sle(baseline_sle_pivoted_data) %>% select(participantidentifier,SLE)
q1_sle_analysis <- calculate_sle(q1_sle_pivoted_data, "_1")
q2_sle_analysis <- calculate_sle(q2_sle_pivoted_data, "_2")
q3_sle_analysis <- calculate_sle(q3_sle_pivoted_data, "_3")
q4_sle_analysis <- calculate_sle(q4_sle_pivoted_data, "_4")


# Function to fetch data from SQL
fetch_data <- function(query) {
  result <- dbSendQuery(con, query)
  data <- dbFetch(result)
  dbClearResult(result)
  return(data)
}

# SQL Queries
queries <- list(
  enrollment = "
    SELECT [ParticipantIdentifier], CAST([EnrollmentDate] AS DATE) AS EnrollmentDate
    FROM [DW].[DimEnrolledParticipants]
    WHERE [participantidentifier] IN (SELECT [participantidentifier] FROM [dbo].[StudyParticipants])",
  baseline = "SELECT [participantidentifier], [resultidentifier], [answers] FROM [dbo].[DepressionAnalysis]",
  dob = "SELECT [participantidentifier], [resultidentifier], [answers] FROM [dbo].[DepressionAnalysis] WHERE [resultidentifier] = 'DOB'",
  quarter = function(q) sprintf("SELECT [participantidentifier], [resultidentifier], [answers] FROM [dbo].[Quarter%dSurvey] WHERE [participantidentifier] IN (SELECT [participantidentifier] FROM [dbo].[complete_phq])", q)
)

# Fetch Data
enrollment_data <- fetch_data(queries$enrollment) %>% rename_with(tolower)
baseline_data <- fetch_data(queries$baseline)
dob_data <- fetch_data(queries$dob)
quarter_data <- lapply(1:4, function(q) fetch_data(queries$quarter(q)))


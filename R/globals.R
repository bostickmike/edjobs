# Column names used inside dplyr verbs (non-standard evaluation), for R CMD check.
utils::globalVariables(c("N_Rows", ".data", "parts_sum", "total_value", "District", "job_id", "Posted_Date", "Seen", "Title", "Last_Seen",
                         # posting_spells.R
                         "Posting_ID", "Run_Date", "Posting_Source", "posted", "title", "t", "key", "run_idx", "chg", "stretch",
                         "From", "To", "Runs", "Postings", "Ongoing", "n", "position", "Role_Hint", "Posting_Identity_Method",
                         "Is_Feed", "date_posted", "Posted_Approx", "url", "Close_Date", "First_Seen", "Next_Run",
                         "Source_First_Run", "Start_Date", "End_Date", "Gap_Censored",
                         # k12_history.R
                         "Broad_Category", "Archive_Date", "Category", "Role_Hint", "position",
                         # app_helpers.R
                         "Current Postings", "Delta",
                         # k12_he_classification.R: posting_run_totals()
                         "Job_Type", "id", "unmarked", ".group",
                         # posting_spells.R: frozen_board_runs()
                         "Weeks",
                         # app_server_shared.R
                         ".Job_Type", ".Map_Name", ".Not_Marked", "Appointment", "Count", "County", "CurrentCount",
                         "DisplayCount", "Duration_Bin", "Enrollment", "Estimated_Days_Open", "Faculty_Avg_Salary",
                         "Faculty_Avg_Salary_Y1Ago", "Institution", "is_admin", "is_cte", "is_elementary", "is_health",
                         "is_secondary", "is_sped", "is_stem", "is_support_staff", "Link", "Location", "MatchingCount",
                         "MatchingSample", "Median_Days", "Name", "SampleTitles", "Status", "Type", "Value",
                         "WeeklyNew",
                         # app_server_configured.R
                         "Adjunct/part-time faculty", "Enrollment_Change_Pct", "Faculty_Avg_Salary_Professor",
                         "Faculty_Avg_Salary_Y2Ago", "Faculty_Count", "Faculty/instructor (non-adjunct)", "Latitude",
                         "Longitude", "Median_Gross_Rent", "Median_Gross_Rent_MOE", "Median_Household_Income",
                         "Median_Household_Income_MOE", "Mining_Employment_Share", "Mining_Employment_Share_MOE",
                         "Openings", "Pell_Recipient_Share", "Pell_Year", "Population_Change_Pct", "Salary_Note",
                         "Students_Per_Teacher", "Vacancy_Rate"))

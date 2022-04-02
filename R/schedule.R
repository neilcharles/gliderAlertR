library(taskscheduleR)

taskscheduler_create(taskname = "test_odb", rscript = glue::glue("{getwd()}/odb_live_get.R"), 
                     schedule = "MINUTE", starttime = format(Sys.time(), "%H:%M"), modifier = 10)

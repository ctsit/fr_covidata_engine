library(tidyverse)
library(dotenv)
library(REDCapR)
library(openxlsx)
library(sendmailR)

source("functions.R")
# set the timezone
Sys.setenv(TZ = Sys.getenv("TIME_ZONE"))

# echo details from the .env file we read
Sys.getenv("INSTANCE")

script_run_time <- strftime(Sys.time(), format = "%Y%m%d_%H%M") 

tokens <- c("SCHEDULED_TESTING_TOKEN")
project_titles <- c("SCHEDULED_TESTING_PROJECT")
project_pids   <- c("SCHEDULED_TESTING_PID")
folder_names <- c("scheduled_testing_")

# read data from result upload project, (prod pid 8270)
result_project_read <- redcap_read_oneshot(redcap_uri = Sys.getenv('URI'),
                                           token = Sys.getenv("RESULT_TOKEN"))$data %>% 
  rowwise() %>% 
  mutate(verified_id = TRUE)

for (pky_project in c(1)) {
  # NOTE: For production script the data will be read from pid 8270
  # to get the research_encounter_id which will then be written to pid 9761
  
  pky_project_read <- redcap_read_oneshot(redcap_uri = Sys.getenv('URI'),
                                          token = Sys.getenv(tokens[[pky_project]]))$data %>%
    filter(!is.na(research_encounter_id)) %>%
    select(record_id, redcap_event_name, research_encounter_id, covid_19_swab_result)
  
  # student records without swab data
  scheduled_testing_swab_data <- pky_project_read %>%
    filter(is.na(covid_19_swab_result)) %>%
    select(record_id, redcap_event_name, research_encounter_id)
  
  # Run these two tests on entire dataset so we'll know that the corrections have
  # been made
  
  # flag incorrect barcodes
  result_id_with_bad_checksum <- result_project_read %>% 
    filter(!verified_id) %>% 
    mutate(reason_not_imported = 'bad checksum in barcode id') %>% 
    select(record_id, covid_19_swab_result, verified_id, reason_not_imported)
  
  # on the low chance that the barcode passes checksum but does not match an id
  # in the scheduled_testing project
  # IMPORTANT: This becomes invalid as data from both projects are being uploaded to results
  # result_id_with_no_match_in_scheduled_testing <- result_project_read %>% 
  #   filter(verified_id) %>% 
  #   anti_join(pky_project_read, by = c("record_id" = "research_encounter_id")) %>% 
  #   mutate(reason_not_imported = 'no match in target project') %>% 
  #   select(record_id, covid_19_swab_result, verified_id, 
  #          reason_not_imported)
  
  # make result upload file for swabs
  swab_result <- result_project_read %>% 
    filter(!is.na(record_id)) %>% 
    select(research_encounter_id = record_id, covid_19_swab_result, verified_id) %>%
    filter(!is.na(covid_19_swab_result)) %>%  
    # join to get records in scheduled_testing project without swab results
    inner_join(scheduled_testing_swab_data, by=c("research_encounter_id")) %>%
    mutate(covid_19_swab_result = case_when(
      str_detect(str_to_lower(covid_19_swab_result), "pos") ~ "1",
      str_detect(str_to_lower(covid_19_swab_result), "neg") ~ "0",
      str_detect(str_to_lower(covid_19_swab_result), "inde") ~ "2",
      str_detect(str_to_lower(covid_19_swab_result), "inad") ~ "99",
      TRUE ~ covid_19_swab_result
    )) %>%
    select(record_id, redcap_event_name, covid_19_swab_result, research_encounter_id, verified_id) %>%
    arrange(record_id)
  
  # only send an email when there are new swab results
  if (nrow(swab_result) > 0){
    
    # create folder to store output
    output_dir <- paste0("pky_covid19_import_log_", folder_names[[pky_project]], script_run_time)
    dir.create(output_dir, recursive = T)
    
    # write data to scheduled_testing project
    swab_result_to_import <- swab_result %>% 
      filter(covid_19_swab_result %in% c("1", "0", "2", "99") & verified_id) %>% 
      select(-verified_id)
    
    bad_swab_result <- swab_result %>% 
      select(-redcap_event_name) %>% 
      filter(!covid_19_swab_result %in% c("1", "0", "2", "99") & verified_id) %>% 
      mutate(reason_not_imported = "improper value for swab result") %>%   
      mutate_at(vars(record_id), as.character) %>% 
      bind_rows(result_id_with_bad_checksum)
    # bind_rows(result_id_with_no_match_in_scheduled_testing)
    
    # only write to redcap when there are legit records
    if(nrow(swab_result_to_import) > 0){
    write_data <- redcap_write_oneshot(swab_result_to_import,
                         redcap_uri = Sys.getenv("URI"),
                         token = Sys.getenv(tokens[[pky_project]]))
    }
    
    if(write_data$success){
    all_output <- list("Swab Results Imported" = swab_result_to_import,
                       "Swab Results Not Imported" = bad_swab_result)
    
    write.xlsx(all_output, 
               paste0(output_dir, "/", folder_names[[pky_project]], 
                      "swab_result_log_", script_run_time, ".xlsx"), 
               na = "")
    
    # Zip the reports generated
    zipfile_name <-  paste0(output_dir, ".zip")
    zip(zipfile_name, output_dir)
    
    # attach the zip file and email it
    attachment_object <- mime_part(zipfile_name, zipfile_name)
    
    project_title <- Sys.getenv(project_titles[[pky_project]])
    project_pid <- Sys.getenv(project_pids[[pky_project]])
    result_project_pid <- Sys.getenv("RESULT_PID")
    
    email_body <- paste0("The attached file(s) includes a log of all results that were uploaded",
                         " to the REDCap project, ", project_title, " (PID ", project_pid ,")",
                         "\n\nNumber of records uploaded: ", write_data$records_affected_count,
                         "\nNumber of records not uploaded: ", nrow(bad_swab_result),
                         "\n\nIf there are records that were not uploaded, then there were",
                         " improper values in the swab result column or incorrect record_ids were used", 
                         " Please review the Swab Results Not Imported tab in the attached log file",
                         " then update these records at ",
                         "https://redcap.ctsi.ufl.edu/redcap/redcap_v9.3.5/index.php?pid=", result_project_pid,
                         "\n\nScript run time: ", script_run_time)
    
    email_subject <- paste("Results loaded for", project_title)
    
    send_upload_email(email_body, email_subject)
    
  }
}
}

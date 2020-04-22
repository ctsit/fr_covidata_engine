# REDCap First Responder COVID-19 ETL Engine

This project provides extract, transform, and load (ETL) tools in support of the First Responders COVID-19 Testing project at the University of Florida. The ETL tools are RScripts run by a Docker container.

## Prerequisites

This script use R and these R packages:

    tidyverse
    dotenv
    REDCapR
    openxlsx
    sendmailR

To build the Docker container, you will need only Docker.

This project is designed to move data between two REDCap projects that work together to collect and curate the data in a COVID-19 testing workflow. The source project, referred to as the _results_ project in the code and configuration files, is provided as a REDCap project XML file at [`./examples/First_Responder_COVID19_Results_Upload.xml`](./examples/First_Responder_COVID19_Results_Upload.xml).  The target project, referred to as the _survey_ project in the code and configuration files, is available at as [`First_Responder_COVID19.xml`](https://github.com/ctsit/fr_covidata/blob/master/example/First_Responder_COVID19.xml) in the [fr_covidata REDCap module](https://github.com/ctsit/fr_covidata).

This script uses the REDCap API to move data between the two projects. The API must be enabled on the REDCap project and the host where this script runs will need to have access to it.


## Setup and Configuration

This script is configured entirely via the environment. An example `.env` files are provided as `./example.env` and `./example_pky.env` To use one of these files, copy it to the name `.env` and customize according to your project needs. Follow these steps to build the required components and configure the script's `.env` file.

1. Create each of the REDCap projects from the project XML files. We will refer to these two projects as _survey_ and _results_ for the remainder of this document.
1. In both the survey and results projects, give a user User Rights of _Full Data Set_ for _Data Exports_
1. In each project, that user will need an API key in _each_ project.
1. Add the new API keys to the .env file taking care to not confuse the two keys.
1. Change the `*_PROJECT_TITLE` and `*_PROJECT_PID` fields to match the result and survey projects.
1. Set `TIME_ZONE` to assure that time stamps used in the file names and the email are accurate.
1. Revise the `EMAIL_*` and `SMTP_SERVER` settings to reflect your local needs.


## Running the ETL script

The primary ETL script is [`load_results_into_survey_project.R`](load_results_into_survey_project.R). It can be run at the command line, in RStudio, or by building and running the docker container. In each case the script will read its configuration from the `.env` file.

To build the image and run the report using docker within the project directory do:

`docker build -t fr_covidata_engine_all .`

and run the script using docker with a command something like this:

`docker run --rm --env-file <path_to_dir_full_of_env_files>/fr_dev.env fr_covidata_engine_all Rscript load_results_into_survey_project.R`


## Testing workflows

To test the the `load_results*` scripts, follow these steps:

1. Write the environment file as described above.
1. Create appointment records in the Survey project. 10-15 records make a good test. 
1. Use `load_fake_data_into_upload.R` to generate fake result data in the Results project. It will derive a swab result value from the the research_encounter_id. 
1. Run `load_results_into_pky_projects.R` or `load_results_into_survey_project.R` according to your need.
1. If you are testing the PKY projects with `load_results_into_pky_projects.R`, continue the test by adding appointment records on the next _follow-up_ event in the serial project. 
1. Create fake result data based on these records by running `load_fake_data_from_serial_into_upload.R`.
1. Test the complete workflow by rerunning `load_results_into_pky_projects.R`.

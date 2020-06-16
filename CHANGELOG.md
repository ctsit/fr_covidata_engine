# Change Log
All notable changes to the REDCap First Responder COVID-19 ETL Engine project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).


## [0.4.0] - 2020-06-16
### Added
- Add FR IgG load script (Philip Chase)
- Add VERSION file referenced in 'Release and Deployment' (Philip Chase)
- Add Release and Deployment section to README (Philip Chase)
- Add prod cron file for FR (Philip Chase)
- Add the URI parameter to example.env (Philip Chase)

### Changed
- Align Dockerfile with tt_engine work (Philip Chase)
- Use URI env var in load fake data (Philip Chase)


## [0.3.0] - 2020-04-24
### Added
- Set form_complete fields in PKY (Philip Chase)


## [0.2.0] - 2020-04-23
### Added
 - Verify the checksums of barcodes before save data to REDCap (Laurence James-Woodley)
 - Add scripts to load data nd test data for PKY (Philip Chase)
 - initialze consecutive negative swab results to zero (Philip Chase)

### Changed
 - Allow one docker image to run any of the scripts (Philip Chase)


## [0.1.0] - 2020-04-14
### Summary
 - First release of fr_covidata_engine
 - Supports swab results: pos, neg, and ina
 - Updates only blank results in survey project
 - Provides a log of upload results and non-loadable for swabs
 - Writes a custom email body using good/bad result count, project titles, PIDs, and URL to result project for address errors
 - Emails log file to configurable addressees

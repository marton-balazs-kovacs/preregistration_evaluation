
<!-- README.md is generated from README.Rmd. Please edit that file -->

# An Assessment of the Secondary Effects of Preregistration

This repository contains the analysis, the data files, and the figures
for the project titled “An Assessment of the Secondary Effects of
Preregistration”. The projects OSF repository can be found on the
following [link](https://osf.io/jcdvb/).

# Folder structure

The `data/` folder contains all the datafiles that are needed to
reproduce the results of the project. The datafiles that contains
personal information are not shared (e.g. sample database with email
addresses, source files with comments that contain information that
makes it possible to identify the respondent).

The `analysis/` folder contains all the analyses files in rmarkdowns
separately for the **Pilot** study and the **Main** study. Within this
folder you can find the following files:

-   `PreregistrationEvaluation_Pilot_Grouping.Rmd` contains the code and
    description for the grouping process of the responses that we
    gathered during the pilot study. Some of the grouping steps were
    carried out manually by the authors. These steps are indicated and
    described in this document.
-   `PreregistrationEvaluatio_Main_Sample.Rmd` contains the code that we
    used to sample contacts from our email address database. The email
    address database that we used to sample the email addresses cannot
    be shared due to privacy reasons.
-   `PreregistrationEvaluation_Main_Source_Raw.Rmd` file contains the
    code necessary for the transformation of the source data (the
    datafile downloaded directly from Qualtrics as is) to the raw
    datafile (datafile with standard name and .tsv format). We also made
    sure that the raw datafile do not contain any information that can
    be used to indetify any of the respondents.
-   `PreregistrationEvaluation_Main_Raw_Processed.Rmd` file contains the
    code that cleans the dataset and transforms is into a tidy format
    ready for the analysis.
-   `PreregistrationEvaluation_Main_Analysis.Rmd` file contains the code
    for the figure creation and the analysis.

The `doc/` folder contains the .html and .docx files rendered from the
rmarkdowns.

The `figures/` folder contains all the figures that are created in the
`PreregistrationEvaluation_Main_Analysis.Rmd` file.

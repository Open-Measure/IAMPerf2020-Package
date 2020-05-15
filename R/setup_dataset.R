#' Prepare an anonimized version of the IAM Performance Measurement 2020 Survey
#' for usage as part of the iamperf2020 R package.
#'
#' `setup_dataset()` Prepare the dataset and output the package RData file in the /data folder.
#'
#' @return nothing.
#'
#' @examples
#'
#' \dontrun{
#' setup_dataset();
#' }
#'
#' @export
setup_dataset <- function() {

  # Pre-requisite:
  # Open the SPSS file exported from QuestionPro with IBM SPSS.
  # And re-save it to SPSS format.
  # This is because the raw file exported from QuestionPro opens properly in IBM SPPS,
  # but fails to load in R. The problem solves when it is loaded once with IBM SPSS,
  # and the file is saved again.

  # Load the original SPSS file.
  survey_rawdata = spssfile_load();

  # Select the subset of dimensions that do not contain PII information.
  iamperf2020data = data.frame(
    # Q18 Org Size.
    Q18 = survey_rawdata$Q18,
    # Q20 Dedicated Team.
    Q20R1 = survey_rawdata$Q20R1,
    Q20R2 = survey_rawdata$Q20R2,
    Q20R3 = survey_rawdata$Q20R3,
    Q20R4 = survey_rawdata$Q20R4,
    Q20R5 = survey_rawdata$Q20R5,
    Q20R6 = survey_rawdata$Q20R6,
    # Q21 Organization Model.
    Q21R1 = survey_rawdata$Q21R1,
    Q21R2 = survey_rawdata$Q21R2,
    Q21R3 = survey_rawdata$Q21R3,
    Q21R4 = survey_rawdata$Q21R4,
    Q21R5 = survey_rawdata$Q21R5,
    Q21R6 = survey_rawdata$Q21R6,
    # Q22 IAM Manager Reporting Line.
    Q22A1 = survey_rawdata$Q22A1,
    Q22A2 = survey_rawdata$Q22A2,
    Q22A3 = survey_rawdata$Q22A3,
    Q22A4 = survey_rawdata$Q22A4,
    Q22A5 = survey_rawdata$Q22A5,
    Q22A6 = survey_rawdata$Q22A6,
    Q22A7 = survey_rawdata$Q22A7,
    # Q23 IAM Goals.
    Q23R1 = survey_rawdata$Q23R1,
    Q23R2 = survey_rawdata$Q23R2,
    Q23R3 = survey_rawdata$Q23R3,
    Q23R4 = survey_rawdata$Q23R4,
    Q23R5 = survey_rawdata$Q23R5,
    Q23R6 = survey_rawdata$Q23R6,
    Q23R7 = survey_rawdata$Q23R7,
    Q23R8 = survey_rawdata$Q23R8,
    Q23R9 = survey_rawdata$Q23R9,
    Q23R10 = survey_rawdata$Q23R10,
    Q23R11 = survey_rawdata$Q23R11,
    # Q24 Maturity Level.
    Q24R1 = survey_rawdata$Q24R1,
    Q24R2 = survey_rawdata$Q24R2,
    Q24R3 = survey_rawdata$Q24R3,
    Q24R4 = survey_rawdata$Q24R4,
    Q24R5 = survey_rawdata$Q24R5,
    Q24R6 = survey_rawdata$Q24R6,
    # Q27 Indicator Design.
    Q27R1 = survey_rawdata$Q27R1,
    Q27R2 = survey_rawdata$Q27R2,
    Q27R3 = survey_rawdata$Q27R3,
    Q27R4 = survey_rawdata$Q27R4,
    Q27R5 = survey_rawdata$Q27R5,
    Q27R6 = survey_rawdata$Q27R6,
    Q27R7 = survey_rawdata$Q27R7,
    Q27R8 = survey_rawdata$Q27R8,
    Q27R9 = survey_rawdata$Q27R9,
    # Q28 Indicator Framework.
    Q28R1 = survey_rawdata$Q28R1,
    Q28R2 = survey_rawdata$Q28R2,
    Q28R3 = survey_rawdata$Q28R3,
    Q28R4 = survey_rawdata$Q28R4,
    Q28R5 = survey_rawdata$Q28R5,
    Q28R6 = survey_rawdata$Q28R6,
    # Q29 Indicator Communication.
    Q29R1 = survey_rawdata$Q29R1,
    Q29R2 = survey_rawdata$Q29R2,
    Q29R3 = survey_rawdata$Q29R3,
    Q29R4 = survey_rawdata$Q29R4,
    Q29R5 = survey_rawdata$Q29R5,
    # Q30 Indicator Automation.
    Q30 = survey_rawdata$Q30,
    # Q23 IAM Goals.
    Q31R1 = survey_rawdata$Q31R1,
    Q31R2 = survey_rawdata$Q31R2,
    Q31R3 = survey_rawdata$Q31R3,
    Q31R4 = survey_rawdata$Q31R4,
    Q31R5 = survey_rawdata$Q31R5,
    Q31R6 = survey_rawdata$Q31R6,
    Q31R7 = survey_rawdata$Q31R7,
    Q31R8 = survey_rawdata$Q31R8,
    Q31R9 = survey_rawdata$Q31R9,
    Q31R10 = survey_rawdata$Q31R10
  );

  # Save the file in
  save(iamperf2020data, file="data/iamperf2020data.RData");
  write.csv(iamperf2020data,"data/iamperf2020data.csv");

}

#' Visualize question #31: indicator coverage
#' References:
#' - https://cran.r-project.org/web/packages/likert/likert.pdf
#' - https://rcompanion.org/handbook/E_03.html
#' - https://rdrr.io/github/decisionpatterns/stringr.tools/
#' - https://rdrr.io/github/decisionpatterns/stringr.tools/man/str_prefix.html
#'
#' `visualize_q31_indicator_coverage()` visualizes question data.
#'
#' @return plot output.
#'
#' @examples
#' visualize_q31_indicator_coverage();
#'
#' \dontrun{
#' visualize_q31_indicator_coverage();
#' }
#'
#' @export
visualize_q31_indicator_coverage <- function() {

  # Load the dataset.
  data("iamperf2020data");

  # Retrieve the applicable answers.
  question_number = 31;
  sub_question_count = 10;
  question_prefix = paste("Q", question_number, "R", sep = "");
  question_data = iamperf2020data[,paste(question_prefix, 1:sub_question_count, sep = "")];
  colnames(question_data) = paste("R", 1:sub_question_count, sep = "");

  # Retrieve the original option titles from the survey.
  # Remove the unecessary prefixes.
  series_titles = c(
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R1), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R2), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R3), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R4), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R5), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R6), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R7), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R8), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R9), "Dims - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R10), "Dims - ")
  );

  # Prepare the vector with the option codes.
  series_codes = stringr.tools::str_prefix(1:sub_question_count, "R");

  # Prepare a table with the option codes and titles.
  legend_block = data.frame(series_codes, series_titles);
  colnames(legend_block) = c("Item", "Statement");

  na_value = 4;

  # na_value = I don't know or not applicable.
  # Replace na_values with NA as NA will be recognized by the likert plotting function.
  question_data$R1 = ifelse(question_data$R1 == na_value, NA, question_data$R1);
  question_data$R2 = ifelse(question_data$R2 == na_value, NA, question_data$R2);
  question_data$R3 = ifelse(question_data$R3 == na_value, NA, question_data$R3);
  question_data$R4 = ifelse(question_data$R4 == na_value, NA, question_data$R4);
  question_data$R5 = ifelse(question_data$R5 == na_value, NA, question_data$R5);
  question_data$R6 = ifelse(question_data$R6 == na_value, NA, question_data$R6);
  question_data$R7 = ifelse(question_data$R7 == na_value, NA, question_data$R7);
  question_data$R8 = ifelse(question_data$R8 == na_value, NA, question_data$R8);
  question_data$R9 = ifelse(question_data$R9 == na_value, NA, question_data$R9);
  question_data$R10 = ifelse(question_data$R10 == na_value, NA, question_data$R10);

  value_levels = c("1", "2", "3");

  value_labels = c("Not covered",
                 "Partially covered",
                 "Satisfactorily covered");

  question_data$R1 = factor(question_data$R1, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R2 = factor(question_data$R2, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R3 = factor(question_data$R3, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R4 = factor(question_data$R4, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R5 = factor(question_data$R5, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R6 = factor(question_data$R6, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R7 = factor(question_data$R7, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R8 = factor(question_data$R8, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R9 = factor(question_data$R9, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R10 = factor(question_data$R10, levels = value_levels, labels = value_labels, ordered = TRUE);

  # headTail(question_data);
  # str(question_data);
  # summary(question_data)
  # likert(question_data);

  # Re-set the column names to the long titles to get proper labels on the graph.
  colnames(question_data) = series_titles;

  likert_data = likert::likert(question_data);

  likert_colors = c("#ff9955", "#dddddd", "#5599ff");

  plot(
    likert_data,
    type = "bar",
    col = likert_colors);

}

#' Visualize question #24: maturity level.
#' References:
#' - https://cran.r-project.org/web/packages/likert/likert.pdf
#' - https://rcompanion.org/handbook/E_03.html
#' - https://rdrr.io/github/decisionpatterns/stringr.tools/
#' - https://rdrr.io/github/decisionpatterns/stringr.tools/man/str_prefix.html
#'
#' `visualize_q24_maturity_level()` visualizes question data.
#'
#' @return plot output.
#'
#' @examples
#' visualize_q24_maturity_level();
#'
#' \dontrun{
#' visualize_q24_maturity_level();
#' }
#'
#' @export
visualize_q24_maturity_level <- function() {

  # Load the dataset.
  data("iamperf2020data");

  # Retrieve the applicable answers.
  question_number = 24;
  sub_question_count = 6;
  question_prefix = paste("Q", question_number, "R", sep = "");
  question_data = iamperf2020data[,paste(question_prefix, 1:sub_question_count, sep = "")];
  colnames(question_data) = paste("R", 1:sub_question_count, sep = "");

  # Retrieve the original option titles from the survey.
  # Remove the unecessary prefixes.
  series_titles = c(
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R1), "Maturity - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R2), "Maturity - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R3), "Maturity - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R4), "Maturity - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R5), "Maturity - "),
    stringr.tools::str_unprefix(sjlabelled::get_label(question_data$R6), "Maturity - ")
  );

  # Prepare the vector with the option codes.
  series_codes = stringr.tools::str_prefix(1:question_number, "R");

  # Prepare a table with the option codes and titles.
  legend_block = data.frame(series_codes, series_titles);
  colnames(legend_block) = c("Item", "Statement");

  # 4 = I don't know or not applicable.
  # Replace 6s with NA as NA will be recognized by the likert plotting function.
  question_data$R1 = ifelse(question_data$R1 == 6, NA, question_data$R1);
  question_data$R2 = ifelse(question_data$R2 == 6, NA, question_data$R2);
  question_data$R3 = ifelse(question_data$R3 == 6, NA, question_data$R3);
  question_data$R4 = ifelse(question_data$R4 == 6, NA, question_data$R4);
  question_data$R5 = ifelse(question_data$R5 == 6, NA, question_data$R5);
  question_data$R6 = ifelse(question_data$R6 == 6, NA, question_data$R6);

  value_levels = c("1", "2", "3", "4", "5");
  value_labels = c("Initial", "Repeatable", "Defined", "Managed", "Optimized");

  question_data$R1 = factor(question_data$R1, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R2 = factor(question_data$R2, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R3 = factor(question_data$R3, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R4 = factor(question_data$R4, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R5 = factor(question_data$R5, levels = value_levels, labels = value_labels, ordered = TRUE);
  question_data$R6 = factor(question_data$R6, levels = value_levels, labels = value_labels, ordered = TRUE);

  # headTail(question_data);
  # str(question_data);
  # summary(question_data)
  # likert(question_data);

  # Re-set the column names to the long titles to get proper labels on the graph.
  colnames(question_data) = series_titles;

  likert_data = likert::likert(question_data);

  likert_colors = c("#ff6600", "#ff9955", "#dddddd", "#5599ff", "#0066ff");

  plot(
    likert_data,
    type = "bar",
    col = likert_colors);

}

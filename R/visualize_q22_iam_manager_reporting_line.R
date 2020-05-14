#' Visualize question #22: IAM manager reporting line
#' References:
#' - https://cran.r-project.org/web/packages/eulerr/vignettes/visualization.html
#' - https://stackoverflow.com/questions/46044564/customizing-euler-diagram-colors-with-eulerr-r
#'
#' `visualize_q22_iam_manager_reporting_line()` visualizes question data.
#'
#' @return plot output.
#'
#' @examples
#' visualize_q22_iam_manager_reporting_line();
#'
#' \dontrun{
#' visualize_q22_iam_manager_reporting_line();
#' }
#'
#' @export
visualize_q22_iam_manager_reporting_line <- function() {

  # Load the dataset.
  data("iamperf2020data");

  # Retrieve the applicable answers.
  question_number = 22;
  sub_question_count = 7;
  question_prefix = paste("Q", question_number, "A", sep = "");
  question_data = iamperf2020data[,paste(question_prefix, 1:sub_question_count, sep = "")];
  colnames(question_data) = paste("R", 1:sub_question_count, sep = "");

  # For troubleshooting purposes to corss-check column headers.
  # sjlabelled::get_label(question_data);

  # Minor bug in the original questionnaire: the value "CFO" appeared twice in the options list.
  # In consequence, merge columns R1 and R5.
  question_data$R1 = ifelse(is.na(question_data$R1), question_data$R5, question_data$R1);

  # And remove column R5 (duplicate CFO option).
  question_data = subset(question_data, select = -c(R5));

  # Remove the N/A column.
  question_data = subset(question_data, select = -c(R7));

  value_columns = c("CFO", "CISO", "CIO", "COO", "Other");
  colnames(question_data) = value_columns;

  # Flatten the list by combinations because answers are non-exclusive but keep the counts.
  question_flat = plyr::count(question_data);

  # Count the number of selected options in the answer.
  question_flat$option_count =
    tidyr::replace_na(question_flat$CFO, 0) +
    tidyr::replace_na(question_flat$CISO, 0) +
    tidyr::replace_na(question_flat$CIO, 0) +
    tidyr::replace_na(question_flat$COO, 0) +
    tidyr::replace_na(question_flat$Other, 0);

  # In the flattened data, remove single options.
  # Single valued answered will be counted "vertically" to account for
  # both multi-valued answers and single-valued answers.
  # This is linked to the data structure required for the Euler plot.
  question_flat = question_flat[question_flat$option_count > 1,];

  # Remove the N/A column.
  question_flat = subset(question_flat, select = -c(option_count));
  #question_flat[is.na(question_flat)] = ""

  # Declare a utility function to perform the paste operation
  # on a vector while discarding NAs in the process.
  paste_without_na = function(some_vector, sep = "") {
    some_vector = some_vector[!is.na(some_vector)];
    some_vector = base::paste(some_vector, collapse = sep);
    return(some_vector);
  };

  # Create a data.frame with string instead of integers,
  # this will facilitate later concatenation.
  question_strings = data.frame(
    CFO = ifelse(question_flat$CFO == 1, "CFO", NA),
    CISO = ifelse(question_flat$CISO == 1, "CISO", NA),
    CIO = ifelse(question_flat$CIO == 1, "CIO", NA),
    COO = ifelse(question_flat$COO == 1, "COO", NA),
    Other = ifelse(question_flat$Other == 1, "Other", NA)
  );

  # Multi-value concatenations.
  # The euler uses a naming convention
  # with bubble names are listed and separated by "&", e.g. "A&B",
  # to designate set intersections.
  intersection_names = apply(question_strings, 1, paste_without_na, "&");

  intersection_frequencies = question_flat$freq;
  names(intersection_frequencies) = intersection_names;

  single_values = c(
    CFO = sum(!is.na(question_data$CFO)),
    CISO = sum(!is.na(question_data$CISO)),
    CIO = sum(!is.na(question_data$CIO)),
    COO = sum(!is.na(question_data$COO)),
    Other = sum(!is.na(question_data$Other))
    );

  euler_data = c(single_values, intersection_frequencies);

  palette = c("#ff6600", "#ff9955", "#5599ff", "#0066ff", "#dddddd");

  plot(
    eulerr::euler(euler_data),
    labels = value_columns,
    fills = palette,
    edges = TRUE,
    quantities = list(fontsize = 24, cex = .5, type = "percent"),
    alpha = 0.7
    );

}

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

  # Binds global variables locally to the function
  # Ref: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  iamperf2020data = NULL;

  # Load the dataset.
  utils::data("iamperf2020data");

  # Retrieve the applicable answers.
  question_number = 22;
  question_data = labelled::unlabelled(iamperf2020data);
  question_data = data.frame(
    CFO = question_data[,"Q22A1"],
    CISO = question_data[,"Q22A2"],
    CIO = question_data[,"Q22A3"],
    COO = question_data[,"Q22A4"],
    CEO = question_data[,"Q22A5"],
    Other = question_data[,"Q22A6"]
  );
  # Column Q22A7 is the "I don't know", we discard it as an NA.
  # Q22A7 = question_data[,"Q22A7"]

  # Convert string values to 1s and NAs to 0s.
  question_data = ifelse(is.na(question_data),0,1);

  # Flatten the list by combinations because answers are non-exclusive but keep the counts.
  question_flat = plyr::count(question_data);

  # Count the number of selected options in the answer.
  # We must sub
  question_flat[,"option_count"] = rowSums(question_flat) - question_flat[,"freq"];

  # In the flattened data, remove single options.
  # Single valued answered will be counted "vertically" to account for
  # both multi-valued answers and single-valued answers.
  # This is linked to the data structure required for the Euler plot.
  question_flat = question_flat[question_flat[,"option_count"] > 1,];

  # Remove the option_count column.
  column_index <- match("option_count", names(question_flat));
  question_flat = subset(question_flat, select = -c(column_index));

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
    CFO = ifelse(question_flat[,"x.CFO"] == 1, "CFO", NA),
    CISO = ifelse(question_flat[,"x.CISO"] == 1, "CISO", NA),
    CIO = ifelse(question_flat[,"x.CIO"] == 1, "CIO", NA),
    COO = ifelse(question_flat[,"x.COO"] == 1, "COO", NA),
    CEO = ifelse(question_flat[,"x.CEO"] == 1, "CEO", NA),
    Other = ifelse(question_flat[,"x.Other"] == 1, "Other", NA)
  );

  # Multi-value concatenations.
  # The euler uses a naming convention
  # with bubble names are listed and separated by "&", e.g. "A&B",
  # to designate set intersections.
  intersection_names = apply(question_strings, 1, paste_without_na, "&");

  intersection_frequencies = question_flat[,"freq"];
  names(intersection_frequencies) = intersection_names;

  single_values = c(
    CFO = sum(question_data[,"CFO"]),
    CISO = sum(question_data[,"CISO"]),
    CIO = sum(question_data[,"CIO"]),
    COO = sum(question_data[,"COO"]),
    CEO = sum(question_data[,"CEO"]),
    Other = sum(question_data[,"Other"])
    );

  euler_data = c(single_values, intersection_frequencies);

  palette = c("#5599ff", "#0066ff", "#dddddd", "#5599ff", "#0066ff", "#dddddd");

  graphics::plot(
    eulerr::euler(euler_data),
    labels = colnames(question_data),
    fills = palette,
    edges = TRUE,
    quantities = list(fontsize = 24, cex = .5, type = "percent"),
    alpha = 0.7
    );

}

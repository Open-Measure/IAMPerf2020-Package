#library(plotrix);
#library(sjlabelled);
#library(knitr);
#library(sjlabelled);
#library(plyr);
#library(formattable);

color_palette = c("#ff6600", "#ff9955", "#dddddd", "#5599ff", "#0066ff");
names(color_palette) = c("Dark orange", "Light orange", "Light grey", "Light blue", "Dark blue");

#' Return the color corresponding to an integer index value, with redish negative values and blueish positive values.
#'
#' `get_color_by_index(integer_index)` return the color corresponding to the index value.
#'
#' @param integer_index The index value of the value that is to be retrieved.
#'
#' @return The color in hex RGB format.
#'
#' @examples
#' get_color_by_index(-1);
#'
#' \dontrun{
#' get_color_by_index(-1);
#' }
#'
#' @export
get_color_by_index <- function(integer_index) {

  vector_index = integer_index + 3;
  return(color_palette[vector_index]);

}

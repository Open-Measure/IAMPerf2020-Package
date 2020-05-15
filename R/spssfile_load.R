#' Wrapper function to load SPSS files.
#'
#' References:
#' - http://www.milanor.net/blog/how-to-open-an-spss-file-into-r/
#' - https://stat.ethz.ch/R-manual/R-devel/library/foreign/html/read.spss.html
#'
#' `spssfile_load(spssfile_path)` load the raw survey data in SPSS format.
#'
#' @param spssfile_path The path of the SPSS file to be loaded.
#'
#' @return Output the SPSS file data frame.
#'
#' @examples
#'
#' \dontrun{
#' spssfile_load(spssfile_path);
#' }
#'
#' @export
spssfile_load <- function(spssfile_path=NULL){

  if(is.null(spssfile_path)) {
    spssfile_path = file.choose();
  }

  # Alternative approach using the foreign package:
  #dataset = read.spss(spssfile_path, to.data.frame=TRUE);

  spss_dataset <- haven::read_sav(spssfile_path);
  return(spss_dataset);

}

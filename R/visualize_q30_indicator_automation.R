#' Visualize question #30: indicator automation.
#'
#' `visualize_q29_indicator_communication()` visualizes question data.
#'
#' @param legend (boolean) Whether the graph legend should be plotted.
#' @param sub_title (boolean) Whether the graph sub title should be plotted.
#' @param title (boolean) Whether the graph title should be plotted.
#'
#' @return plot output.
#'
#' @examples
#' visualize_q30_indicator_automation();
#' visualize_q30_indicator_automation(legend = FALSE, sub_title = FALSE, title = FALSE);
#'
#' \dontrun{
#' visualize_q30_indicator_automation();
#' }
#'
#' @export
visualize_q30_indicator_automation <- function(
  legend = TRUE,
  sub_title = TRUE,
  title = TRUE) {

  # Binds global variables locally to the function
  # Ref: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  iamperf2020data = NULL;

  # Load the dataset.
  utils::data("iamperf2020data");

  # Remove dropouts.
  data_frequencies = iamperf2020data$Q30[!is.na(iamperf2020data$Q30)];
  # Count frequencies.
  data_frequencies = plyr::count(data_frequencies);

  data_labels = sjlabelled::get_labels(data_frequencies);
  data_labels$x[4] = "Not available or I don't know";
  data_frequencies$labels = c("Mostly\nautomated", "Partially\nautomated", "Mostly\nmanual", "N/A");
  data_frequencies$categories = c("Mostly automated", "Partially automated", "Mostly manual", "N/A");

  graph_legend = paste(data_frequencies$categories," (", data_labels$x, ")", sep="");
  graph_legend = paste(graph_legend, collapse = '\n');

  data_frequencies$colors = c(
    rep(get_color_by_index(1), 3),
    get_color_by_index(0));

  y_axis_max = ceiling(max(data_frequencies$freq) / 10) * 10;

  dropouts_count = sum(is.na(iamperf2020data$Q30));
  answers_count = length(iamperf2020data$Q30) - dropouts_count;
  dropouts_ratio = dropouts_count / (dropouts_count + answers_count);
  answers_ratio = answers_count / (dropouts_count + answers_count);

  graph_subtitle = paste0(
    "Answers: ", answers_count,
    " (", formattable::percent(answers_ratio, digits = 0), ")",
    ", dropouts: ", dropouts_count,
    " (", formattable::percent(dropouts_ratio, digits = 0), ")");

  data_frequencies$ratio = ifelse(is.na(data_frequencies$x),
                                  NA,
                                  data_frequencies$freq / answers_count);

  data_frequencies$bar_labels = ifelse(
    is.na(data_frequencies$x),
    paste(" ", data_frequencies$freq, " "),
    paste(
      data_frequencies$freq,
      "\n ",
      round(data_frequencies$ratio * 100),
      "% ",
      sep = ""
    )
  );

  # Plot graph.

  graphics::par(mar = c(8, 4, 4, 4));

  barplot_graph = graphics::barplot(
    data_frequencies$freq,
    names = data_frequencies$labels,
    ylab = "Frequency",
    ylim = c(0, y_axis_max),
    col = data_frequencies$colors,
  );

  if(title){
  graphics::title(
    main = "Degree of automation of IAM performance indicators",
    #sub = graph_subtitle,
    cex.sub = .8,
    outer = FALSE);
  };

  graphics::abline(
    v = 3.7,
    col = "red",
    lwd = 2,
    lty = 2
  );

  plotrix::barlabels(
    barplot_graph,
    data_frequencies$freq,
    labels = data_frequencies$bar_labels,
    cex = .8,
    prop = 1,
    miny = 0,
    offset = 0,
    nobox = FALSE
  );

  if(sub_title){
  graphics::mtext(graph_subtitle, side = 3, line = .4, cex = .8, outer = FALSE);
  };

  if(legend){
  graphics::mtext(graph_legend, side = 1, line = 6, cex = .7, outer = FALSE);
  };
}

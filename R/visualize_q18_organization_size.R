#' Visualize question #18: organization size.
#'
#' `visualize_organization_size()` visualizes question data.
#'
#' @return plot output.
#'
#' @examples
#' visualize_q18_organization_size();
#'
#' \dontrun{
#' visualize_q18_organization_size();
#' }
#'
#' @export
visualize_q18_organization_size <- function() {

  # Load the dataset.
  data("iamperf2020data");

  # Remove dropouts.
  data_frequencies = iamperf2020data$Q18[!is.na(iamperf2020data$Q18)];
  # Count frequencies.
  data_frequencies = plyr::count(data_frequencies);

  data_labels = sjlabelled::get_labels(data_frequencies);
  data_labels$x[5] = "N/A (Not available or I don't know)";

  graph_legend = paste(data_labels$x, collapse = '\n');

  data_frequencies$colors = c(
    rep(get_color_by_index(1), 4),
    get_color_by_index(0));

  data_frequencies$categories = c("Large", "Medium", "Small", "Micro", "N/A");

  y_axis_max = ceiling(max(data_frequencies$freq) / 10) * 10;

  dropouts_count = sum(is.na(iamperf2020data$Q18));
  answers_count = length(iamperf2020data$Q18) - dropouts_count;
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
    names = data_frequencies$categories,
    ylab = "Frequency",
    ylim = c(0, y_axis_max),
    col = data_frequencies$colors,
  );

  graphics::title(
    main = "Organization size",
    #sub = graph_subtitle,
    cex.sub = .8,
    outer = FALSE);

  graphics::abline(
    v = 4.9,
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

  graphics::mtext(graph_subtitle, side = 3, line = .4, cex = .8, outer = FALSE);

  graphics::mtext(graph_legend, side = 1, line = 6, cex = .8, outer = FALSE);

}

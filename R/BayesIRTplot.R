#' Generate plots for Bayesian item response theory models
#'
#' Generate plots for Bayesian item response theory (IRT) models fitted in `blavaan`. `BayesIRTplot()` reads in a fitted IRT model from `blavaan`.
#' The current version supports unidimensional two-parameter IRT models. Available plots include Item-Person Map (IPM), Item Characteristic Curve (ICC), Item Information Curve (IIC),
#' Test Characteristic Curve (TCC), Test Information Curve (TIC), and Reliability Plot (REL).
#'
#' @param blavaanobj A fitted IRT model from `blavaan`.
#' @param IRTplot Type of IRT plot to generate. Can be one or more of the following:
#'
#'   * IPM: Item-Person Map
#'   * ICC: Item Characteristic Curve
#'   * IIC: Item Information Curve
#'   * TCC: Test Characteristic Curve
#'   * TIC: Test Information Curve
#'   * REL: Reliability Plot
#'   * ALL (default): All plots listed above
#' @param DrawsK Controls the number of posterior draws being plotted as separate lines in ICC, IIC, TCC and TIC.
#'   Default to 50. Note that in those plots, the mean probabilities or the mean information are calculated using all posterior draws from `blavaan` fitted object
#'   regardless of how many posterior draws are plotted as separate lines. This option does not affect IPM and REL plots which always use all posterior draws.
#' @param items_IPM A list of items which will be included in IPM. By default, all items will be plotted.
#' @param items_ICC_IIC A list of items for which ICC and IIC will be generated. By default, ICC and IIC of all items will be generated.
#' @param item_names_ICC_IIC A list of item names to be displayed on ICC and IIC (optional). Should be of the same length as `items_ICC_IIC`.
#' @param plot_option_ICC_IIC Types of ICC and IIC plots to generate. Can be one or more of the following:
#'
#'   * separate (default): each item in a separate plot
#'   * panel: several items plotted in a panel
#'   * overlay: several items overlaid in one plot
#'
#'   Note that in overlaid plots, only the mean probabilities of a correct response or the mean item information across all posterior draws will be plotted (no separate lines for each posterior draw).
#' @param max_plots_panel In panel ICC and IIC plots, controls the maximum number of items to be plotted in one panel. Default to 9.
#' @param ncol_panel In panel ICC and IIC plots, controls the number of columns in one panel. Default to 3.
#' @param overlay_style In overlaid ICC and IIC plots, controls the way to distinguish different items:
#'
#'   * color (default): different items are plotted in different colors
#'   * line: different items are plotted in different line types
#'
#'   Note that if `overlay_style = "line"`, a maximum of 13 items can be plotted.
#' @param print_plots Should plots be printed? Logical; default to TRUE.
#' @param save_plots Should plots be saved? Logical; default to FALSE.
#' @param save_dir Directory where plots will be saved when `save_plots = TRUE`. Defaults to the current working directory returned by `getwd()`.
#'   Absolute path or relative path within the current working directory may be provided.
#' @param dpi If `save_plots = TRUE`, specifies the resolution (dots per inch) used when saving the plot. Default to 300.
#' @returns A list of ggplot objects corresponding to the requested IRT plots.
#'
#'   Depending on the requested plots and plotting options, the list may contain:
#'   * IPMplot: Item-Person Map
#'   * ICCplot_item1, ICCplot_item2, ...: Item Characteristic Curves by item
#'   * ICCplot_panel1, ICCplot_panel2, ...: Item Characteristic Curves by panel
#'   * ICCplot_overlay: Item Characteristic Curves overlaid
#'   * IICplot_item1, IICplot_item2, ...: Item Information Curves by item
#'   * IICplot_panel1, IICplot_panel2, ...: Item Information Curves by panel
#'   * IICplot_overlay: Item Information Curves overlaid
#'   * TCCplot: Test Characteristic Curve
#'   * TICplot: Test Information Curve
#'   * RELplot: Reliability Plot
#'
#' @examples
#' BayesIRTplot(tma.IRT.fitted.model)
#'
#' # access individual plot
#' plots <- BayesIRTplot(tma.IRT.fitted.model, print_plots = FALSE)
#' plots$IPMplot
#'
#' @export
BayesIRTplot <- function(blavaanobj,
                         IRTplot = "ALL",
                         DrawsK = 50,
                         items_IPM = "ALL",
                         items_ICC_IIC = "ALL",
                         item_names_ICC_IIC = NULL,
                         plot_option_ICC_IIC = "separate",
                         max_plots_panel = 9,
                         ncol_panel = 3,
                         overlay_style = "color",
                         print_plots = TRUE,
                         save_plots = FALSE,
                         save_dir = getwd(),
                         dpi = 300)
{

  # data for plotting
  data_list <- BayesIRTdata(blavaanobj)

  draws_by_item_theta <- data_list$draws_by_item_theta
  summary_by_item <- data_list$summary_by_item
  summary_by_item_theta <- data_list$summary_by_item_theta
  summary_by_draw_theta <- data_list$summary_by_draw_theta
  summary_by_theta <- data_list$summary_by_theta
  summary_by_draw <- data_list$summary_by_draw
  mean_reliability <- data_list$mean_reliability

  # number of draws
  ndraws <- nrow(summary_by_draw)
  if (DrawsK > ndraws) {
    warning (paste0(
      "DrawsK (", DrawsK,
      ") exceeds the number of available posterior draws (", ndraws,
      "). All available draws will be used."
    ))
  }

  # number of items
  nitems <- data_list$nitems

  ###############################################
  # * IRT Plot Options and Data Preparation -----
  ###############################################
  IRTplot <- toupper(IRTplot)

  # set up dummy variables to indicate which plot is requested
  # set dummy variable to 1 if the plot is requested
  if ("ALL" %in% IRTplot) {
    IPM <- 1
    ICC <- 1
    IIC <- 1
    TCC <- 1
    TIC <- 1
    REL <- 1
  } else {
    IPM <- as.integer("IPM" %in% IRTplot)
    ICC <- as.integer("ICC" %in% IRTplot)
    IIC <- as.integer("IIC" %in% IRTplot)
    TCC <- as.integer("TCC" %in% IRTplot)
    TIC <- as.integer("TIC" %in% IRTplot)
    REL <- as.integer("REL" %in% IRTplot)
  }

  # initialize a list for plots
  plots <- list()

  # show two digits on axis, remove leading zeros
  scaleFUN <- function(x) {
    labels <- sprintf("%.2f", x)
    labels[which(x == 0)] <- "0"
    gsub("(-?)0\\.", "\\1.", labels)
  }


  ################################
  ### * Item-Person Map Plot -----
  ################################
  if (IPM == 1){

    # user can supply a list of items to plot
    # if user did not supply, then default to plot ALL items
    item_list_IPM <-
      if (is.character(items_IPM) && toupper(items_IPM) == "ALL") {
        1:nitems
      } else if (is.numeric(items_IPM)) {
        unique(items_IPM)
      }

    sum_by_item_IPM <- summary_by_item |>
      dplyr::filter(itemnum %in% item_list_IPM) |>
      dplyr::mutate(
        itemnum = factor(itemnum, levels = item_list_IPM)
      )

    # Box plot of item thresholds
    IPMplot <- ggplot(data = sum_by_item_IPM) +
      geom_boxplot(aes(
        x = itemnum,
        lower = q1_tau,
        middle = q2_tau,
        upper = q3_tau,
        ymin = min_tau,
        ymax = max_tau),
        stat = "identity") +
      theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none", panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(x = "Item Number", y = "Item Threshold") +
      scale_y_continuous(labels = scaleFUN, position = "right", expand = expansion(mult = 0.05),
                         breaks = scales::breaks_pretty(n = 4)) +
      theme(text = element_text(family = "serif", size = 12))


    if (print_plots==TRUE){
      print(IPMplot)
    }
    plots$IPMplot <- IPMplot
  }

  ##########################
  ### * Construct item list -----
  # for ICC and IIC
  ##########################
  # user can supply a list of items to plot
  # if user did not supply, then default to plot ALL items
  item_list_ICC_IIC <-
    if (is.character(items_ICC_IIC) && toupper(items_ICC_IIC) == "ALL") {
      1:nitems
    } else if (is.numeric(items_ICC_IIC)) {
      unique(items_ICC_IIC)
    }

  name_map <- NULL
  # check item names, if provided, then must match with the number of items listed for plotting
  if (!is.null(item_names_ICC_IIC)) {
    if (length(item_names_ICC_IIC) != length(item_list_ICC_IIC)) {
      stop("number of item names must equal number of items to be plotted")
    }
    # mapping between item numbers and item names
    name_map <- stats::setNames(
      item_names_ICC_IIC,
      item_list_ICC_IIC)
  }

  ##########################################
  ### * Item Characteristic Curve Plot -----
  ##########################################
  if (ICC == 1){

    ## options to plot 1) each item separately 2) each item separately in a panel 3) overlay in the same plot
    plot_option_ICC_IIC <- toupper(plot_option_ICC_IIC)

    # set up dummy variables to indicate which type of plot is requested
    # set dummy variable to 1 if the plot is requested
    if ("ALL" %in% plot_option_ICC_IIC) {
      ICC_separate <- 1
      ICC_panel <- 1
      ICC_overlay <- 1
    } else {
      ICC_separate <- as.integer("SEPARATE" %in% plot_option_ICC_IIC)
      ICC_panel <- as.integer("PANEL" %in% plot_option_ICC_IIC)
      ICC_overlay <- as.integer("OVERLAY" %in% plot_option_ICC_IIC)
    }

    # plot each item separately
    if (ICC_separate == 1){
      # loop through each item
      for (i in 1:length(item_list_ICC_IIC)){

        item_plot = item_list_ICC_IIC[i]
        title_text <- ifelse(!is.null(item_names_ICC_IIC),
                             paste("Item Characteristic Curve for", name_map[as.character(item_plot)]),
                             paste("Item Characteristic Curve for Item", item_plot))

        ICCplot <- ggplot(subset(draws_by_item_theta, itemnum == item_plot & draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
                          aes(x = theta, y = p, colour = as.factor(draw))) +
          geom_line(alpha = 0.4) +
          scale_colour_grey() +
          labs(title = title_text,
               x = expression(theta),
               y = expression(italic(p)(y == 1))) +
          geom_line(data = subset(summary_by_item_theta, itemnum == item_plot),
                    aes(x = theta, y = p), colour = "black", linetype = "solid", linewidth = 2) +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "serif", size = 12)) +
          scale_y_continuous(labels = scaleFUN)

        if (print_plots==TRUE){
          print(ICCplot)
        }
        plots[[paste0("ICCplot_item", item_plot)]] <- ICCplot
      }}


    # plot items in a panel
    if (ICC_panel == 1){

      # split item list into groups
      item_groups <- split(
        item_list_ICC_IIC,
        ceiling(seq_along(item_list_ICC_IIC) / max_plots_panel)
      )

      # plot items in each group separately
      for (g in seq_along(item_groups)) {

        current_items <- item_groups[[g]]

        # subset to current items and needed draws
        plot_data <- subset(
          draws_by_item_theta,
          itemnum %in% current_items & draw %in% 1:as.integer(DrawsK)
        )
        # subset to current items
        sum_data <- subset(
          summary_by_item_theta,
          itemnum %in% current_items
        )

        # item names
        if (!is.null(item_names_ICC_IIC)) {
          current_names <- name_map[as.character(current_items)]
        }else {
          current_names <- paste("Item", current_items)
        }

        plot_data$item_label <- factor(plot_data$itemnum, levels = current_items, labels = current_names)
        sum_data$item_label <- factor(sum_data$itemnum, levels = current_items, labels = current_names)

        ICCplot_panel <- ggplot(plot_data, aes(x = theta, y = p, colour = as.factor(draw))) +
          geom_line(alpha = 0.4) +
          scale_colour_grey() +
          geom_line(data = sum_data, aes(x = theta, y = p), colour = "black", linewidth = 1.2) +
          facet_wrap(~ item_label, ncol = ncol_panel) +
          labs(
            title = "Item Characteristic Curve",
            x = expression(theta),
            y = expression(italic(p)(y == 1))
          ) +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "serif", size = 12)) +
          scale_y_continuous(labels = scaleFUN)

        if (print_plots==TRUE){
          print(ICCplot_panel)
        }
        plots[[paste0("ICCplot_panel", g)]] <- ICCplot_panel
      }}

    # overlay items in a plot
    if (ICC_overlay == 1){

      if (toupper(overlay_style) == "LINE" &&
          length(item_list_ICC_IIC) > 13) {
        stop("A maximum of 13 items can be plotted in an overlaid plot. Consider plot_option_ICC_IIC = 'panel' or 'separate', or use overlay_style = 'color'. Advanced users may also modify the function source code available on GitHub to support additional line types.")
      }

      # item names
      summary_by_item_theta$item_label <-
        if (!is.null(item_names_ICC_IIC)) {
          factor(summary_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = name_map[as.character(item_list_ICC_IIC)])
        } else {
          factor(summary_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = item_list_ICC_IIC)
        }

      # distinguish items by different colors
      if (toupper(overlay_style) == "COLOR") {

        ICCplot_overlay <- ggplot() +
          geom_line(
            data = summary_by_item_theta |> dplyr::filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = p,
                group = item_label,
                colour = item_label),
            linewidth = 1, alpha = 0.8) +
          labs(title = "Item Characteristic Curve",
               colour = "Item",
               x = expression(theta),
               y = expression(italic(p)(y==1))) +
          theme_classic() +
          theme(text = element_text(family = "serif", size = 12)) +
          scale_color_hue(l = 70, c = 60) +
          scale_y_continuous(labels = scaleFUN)
      }

      # distinguish items by different line types
      else if (toupper(overlay_style) == "LINE") {

        ICCplot_overlay <- ggplot() +
          geom_line(
            data = summary_by_item_theta |> dplyr::filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = p,
                group = item_label,
                linetype = item_label),
            linewidth = 1) +
          labs(title = "Item Characteristic Curve",
               linetype = "Item",
               x = expression(theta),
               y = expression(italic(p)(y==1))) +
          theme_classic() +
          theme(text = element_text(family = "serif", size = 12)) +
          scale_y_continuous(labels = scaleFUN)
      }
      if (print_plots==TRUE){
        print(ICCplot_overlay)
      }
      plots$ICCplot_overlay <- ICCplot_overlay

    }
  }

  #######################################
  ### * Item Information Curve Plot -----
  #######################################
  if (IIC == 1){

    ## options to plot 1) each item separately 2) each item separately in a panel 3) overlay in the same plot
    plot_option_ICC_IIC <- toupper(plot_option_ICC_IIC)

    # set up dummy variables to indicate which type of plot is requested
    # set dummy variable to 1 if the plot is requested
    if ("ALL" %in% plot_option_ICC_IIC) {
      IIC_separate <- 1
      IIC_panel <- 1
      IIC_overlay <- 1
    } else {
      IIC_separate <- as.integer("SEPARATE" %in% plot_option_ICC_IIC)
      IIC_panel <- as.integer("PANEL" %in% plot_option_ICC_IIC)
      IIC_overlay <- as.integer("OVERLAY" %in% plot_option_ICC_IIC)
    }

    # plot each item separately
    if (IIC_separate == 1){
      # loop through each item
      for (i in 1:length(item_list_ICC_IIC)){

        item_plot = item_list_ICC_IIC[i]
        title_text <- ifelse(!is.null(item_names_ICC_IIC),
                             paste("Item Information Curve for", name_map[as.character(item_plot)]),
                             paste("Item Information Curve for Item", item_plot))

        IICplot <-
          ggplot(subset(draws_by_item_theta, itemnum == item_plot & draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
                 aes(x = theta, y = i, color = as.factor(draw))) +
          geom_line(alpha = 0.4) +
          scale_colour_grey() +
          labs(title = title_text,
               x = expression(theta),
               y = "Information") +
          geom_line(data = subset(summary_by_item_theta, itemnum == item_plot),
                    aes(x = theta, y = i), colour = "black", linetype = "solid", linewidth = 2) +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "serif", size = 12)) +
          scale_y_continuous(labels = scaleFUN)

        if (print_plots==TRUE){
          print(IICplot)
        }
        plots[[paste0("IICplot_item", item_plot)]] <- IICplot
      }}


    # plot items in a panel
    if (IIC_panel == 1){

      # split item list into groups (by default, a maximum of 9 plots will be plotted in a panel)
      item_groups <- split(
        item_list_ICC_IIC,
        ceiling(seq_along(item_list_ICC_IIC) / max_plots_panel)
      )

      # plot items in each group separately
      for (g in seq_along(item_groups)) {

        current_items <- item_groups[[g]]

        # subset to current items and needed draws
        plot_data <- subset(
          draws_by_item_theta,
          itemnum %in% current_items & draw %in% 1:as.integer(DrawsK)
        )
        # subset to current items
        sum_data <- subset(
          summary_by_item_theta,
          itemnum %in% current_items
        )

        # item names
        if (!is.null(item_names_ICC_IIC)) {
          current_names <- name_map[as.character(current_items)]
        }else {
          current_names <- paste("Item", current_items)
        }

        plot_data$item_label <- factor(plot_data$itemnum, levels = current_items, labels = current_names)
        sum_data$item_label <- factor(sum_data$itemnum, levels = current_items, labels = current_names)

        IICplot_panel <- ggplot(plot_data, aes(x = theta, y = i, colour = as.factor(draw))) +
          geom_line(alpha = 0.4) +
          scale_colour_grey() +
          geom_line(data = sum_data, aes(x = theta, y = i), colour = "black", linewidth = 1.2) +
          facet_wrap(~ item_label, ncol = ncol_panel) +
          labs(
            title = "Item Information Curve",
            x = expression(theta),
            y = "Information") +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "serif", size = 12)) +
          scale_y_continuous(labels = scaleFUN)

        if (print_plots==TRUE){
          print(IICplot_panel)
        }
        plots[[paste0("IICplot_panel", g)]] <- IICplot_panel
      }}

    # overlay items in a plot
    if (IIC_overlay == 1){

      if (toupper(overlay_style) == "LINE" &&
          length(item_list_ICC_IIC) > 13) {
        stop("A maximum of 13 items can be plotted in an overlaid plot. Consider plot_option_ICC_IIC = 'panel' or 'separate', or use overlay_style = 'color'. Advanced users may also modify the function source code available on GitHub to support additional line types.")
      }

      # item names
      summary_by_item_theta$item_label <-
        if (!is.null(item_names_ICC_IIC)) {
          factor(summary_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = name_map[as.character(item_list_ICC_IIC)])
        } else {
          factor(summary_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = item_list_ICC_IIC)
        }

      # distinguish items by different colors
      if (toupper(overlay_style) == "COLOR") {

        IICplot_overlay <- ggplot() +
          geom_line(
            data = summary_by_item_theta |> dplyr::filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = i,
                group = item_label,
                colour = item_label),
            linewidth = 1, alpha = 0.8) +
          labs(title = "Item Information Curve",
               colour = "Item",
               x = expression(theta),
               y = "Information") +
          theme_classic() +
          theme(text = element_text(family = "serif", size = 12)) +
          scale_color_hue(l = 70, c = 60) +
          scale_y_continuous(labels = scaleFUN)
      }

      # distinguish items by different line types
      else if (toupper(overlay_style) == "LINE") {

        IICplot_overlay <- ggplot() +
          geom_line(
            data = summary_by_item_theta |> dplyr::filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = i,
                group = item_label,
                linetype = item_label),
            linewidth = 1) +
          labs(title = "Item Information Curve",
               linetype = "Item",
               x = expression(theta),
               y = "Information") +
          theme_classic() +
          theme(text = element_text(family = "serif", size = 12)) +
          scale_y_continuous(labels = scaleFUN)
      }

      if (print_plots==TRUE){
        print(IICplot_overlay)
      }
      plots$IICplot_overlay <- IICplot_overlay
    }

  }


  #######################################
  ### * Test Characteristic Curve Plot -----
  #######################################
  if (TCC == 1){

    TCCplot <-
      ggplot(subset(summary_by_draw_theta, draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
             aes(x = theta, y = test_p, color = as.factor(draw))) +
      geom_line(alpha = 0.4)  +
      scale_colour_grey() +
      labs(title = "Test Characteristic Curve",
           x = expression(theta),
           y = "Expected Score") +
      geom_line(data = summary_by_theta, aes(x = theta, y = test_p), colour = "black", linetype = "solid", linewidth = 2) +
      theme_classic() +
      theme(legend.position = "none", text = element_text(family = "serif", size = 12))

    if (print_plots==TRUE){
      print(TCCplot)
    }
    plots$TCCplot <- TCCplot
  }

  #######################################
  ### * Test Information Curve Plot -----
  #######################################
  if (TIC == 1){

    TICplot <-
      ggplot(subset(summary_by_draw_theta, draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
             aes(x = theta, y = test_i, color = as.factor(draw))) +
      geom_line(alpha = 0.4)  +
      scale_colour_grey() +
      labs(title = "Test Information Curve",
           x = expression(theta),
           y = "Information") +
      geom_line(data = summary_by_theta, aes(x = theta, y = test_i), colour = "black", linetype = "solid", linewidth = 2) +
      theme_classic() +
      theme(legend.position = "none", text = element_text(family = "serif", size = 12))

    if (print_plots==TRUE){
      print(TICplot)
    }
    plots$TICplot <- TICplot
  }

  ############################
  ### * Reliability Plot -----
  ############################
  if (REL == 1){

    RELplot <- ggplot(summary_by_draw, aes(x = rel)) +
      geom_histogram(
        binwidth = 0.01,
        fill = "grey40",
        color = "white"
      ) +
      scale_x_continuous(breaks = seq(0, 1, by = .01), labels = scaleFUN) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(legend.position = "none") +
      scale_colour_grey() +
      labs(title = "Marginal Reliability Plot",
           x = "Reliability") +
      geom_vline(xintercept = mean_reliability, color = "black", linetype = "longdash", linewidth = 1) +
      theme(text = element_text(family = "serif", size = 12),
            axis.text.y = element_blank(),      #numbers
            axis.ticks.y = element_blank(),     #ticks
            axis.title.y = element_blank())     #axis title

    if (print_plots==TRUE){
      print(RELplot)
    }
    plots$RELplot <- RELplot
  }

  #### Save plots to local directory
  if (save_plots) {
    for (nm in names(plots)) {
      if (!is.null(plots[[nm]])) {
        ggsave(
          filename = file.path(save_dir, paste0(nm, ".png")),
          plot = plots[[nm]],
          width = 8,
          height = 6,
          dpi = dpi
        )
      }
    }
  }

  return(invisible(plots))
}

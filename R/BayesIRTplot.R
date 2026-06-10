BayesIRTplot <- function(blavaanobj,
                         IRTplot = "ALL",
                         DrawsK = 50,
                         items_IPM = "ALL",
                         items_ICC_IIC = "ALL",
                         item_names_ICC_IIC = NULL,
                         ICCplot_option = "separate",
                         IICplot_option = "separate",
                         max_plots_panel = 9,
                         ncol_panel = 3,
                         overlay_style = "color",
                         save_plots = FALSE,
                         save_dir = getwd(),
                         dpi = 300)
{

  # option for suppressing scientific notation
  options(scipen=999)

  library(blavaan)
  library(lavaan)
  library(MCMCvis)
  library(ggplot2)
  library(coda)
  library(dplyr)
  library(tidyr)

  #####################################
  # * Data Pre-processing -----
  #####################################
  # extract the stanfit object
  fitted.model.sim.stanfit <- blavInspect(blavaanobj, what = "mcobj")
  # convert draws to mcmc.list
  draws.sim.to.analyze <- MCMCchains(fitted.model.sim.stanfit, mcmc.list = TRUE)
  # extract number of items
  nitems = length(lavNames(blavaanobj, type = "ov"))

  # rename parameters
  a_start <- 1 # starting column number for a parameters
  a_end <- nitems  # ending column number for a parameters
  tau_start <- nitems+1 # starting column number for tau parameters
  tau_end <- nitems*2  # ending column number for tau parameters

  # rename a parameters
  varnames(draws.sim.to.analyze)[a_start:a_end] <- paste0("a_", seq(from = 1, to = nitems))
  # rename tau parameters
  varnames(draws.sim.to.analyze)[tau_start:tau_end] <- paste0("tau_", seq(from = 1, to = nitems))

  # combine chains row-wise (if have multiple chains)
  draws_df_sim <- as.data.frame(do.call(rbind, draws.sim.to.analyze))

  # extract number of draws
  ndraws <- nrow(draws_df_sim)
  if (DrawsK > ndraws) {
    stop("number of draws to be plotted should be lower than the total number of draws")
  }

  # keep only parameters used in the analysis
  draws_df_sim<-draws_df_sim[, 1:tau_end]
  # create a column for draws, from 1 to total number of draws
  draws_df_sim$draw <- 1:nrow(draws_df_sim)

  #### reformatting data frame to long format (number of items * number of draws = total rows)
  # long format: for a parameters
  draws_long_a <- draws_df_sim %>%
    select(draw, starts_with("a_")) %>%
    pivot_longer(starts_with("a_"), names_to = "label_a", values_to = "a")
  # long format: for tau parameters
  draws_long_tau <- draws_df_sim %>%
    select(draw, starts_with("tau_")) %>%
    pivot_longer(starts_with("tau_"), names_to = "label_tau", values_to = "tau")
  # merge
  drawsfinalsim <- cbind(draws_long_a, draws_long_tau)
  # add item number
  drawsfinalsim$itemnum <- seq(from = 1, to = nitems)
  # keep columns that are needed
  drawsfinalsim <- drawsfinalsim[ , c("draw", "a", "tau", "itemnum")]

  ##############################
  # calculate the probability of correct for theta in [-6,6]
  p.from.blavaan.for.theta.values <- function(pars){

    # define the range for theta
    theta = seq(-6, 6, by = .2)

    prob_correct <- NULL
    lambda <- pars$a
    tau <- pars$tau

    for(i in 1:nrow(pars)){
      prob_correct[[i]] <- pnorm(tau[i], mean = lambda[i]*theta, sd = 1, lower.tail = FALSE) # calculate probability
    }
    # convert to dataframe
    prob_correct_df <- data.frame(t(sapply(prob_correct, c)))
    # add columns for draw number and item number
    prob_correct_df <- cbind(prob_correct_df, draw = pars$draw, itemnum = pars$itemnum)
    return (prob_correct_df)
  }

  prob_correct_all <- p.from.blavaan.for.theta.values(drawsfinalsim)

  # convert to long format
  # each row is for one item, one draw, one theta value and the corresponding probability of correct
  draws_theta <- prob_correct_all %>% group_by(draw) %>%
    select(draw, itemnum, starts_with("X")) %>%
    pivot_longer(starts_with("X"), names_to = "Theta_label", values_to = "p")
  # theta values
  draws_theta$theta <- rep(seq(-6, 6, 0.2), length.out = nrow(draws_theta))
  # drop column
  draws_theta$Theta_label <- NULL

  #################
  # merge with draws of parameter values
  draws_theta_final <- merge(draws_theta, drawsfinalsim, by = c("draw","itemnum"))
  # transformation of a parameter (from probit to logit)
  draws_theta_final$a <- draws_theta_final$a*1.702
  # item information
  draws_theta_final$i <- with(draws_theta_final, a^2 * p * (1 - p))
  # reorder it by draw and itemnum
  draws_theta_final <- draws_theta_final[order(draws_theta_final$draw,
                                               draws_theta_final$itemnum), ]


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

  #####################
  # summary data for plot

  # summary by item
  # mean, IQR of tau parameter across draws
  sum_by_item <- draws_theta_final %>%
    expand(nesting(draw, itemnum, tau)) %>% # remove duplicated rows
    group_by(itemnum) %>%
    summarize(mean_tau = mean(tau),
              q1 = quantile(tau, probs = 0.25), #Q1
              q2 = quantile(tau, probs = 0.5), #Median
              q3 = quantile(tau, probs = 0.75), #Q3
              ymin   = q1 - 1.5 * IQR(tau),
              ymax   = q3 + 1.5 * IQR(tau)
    ) %>% ungroup()

  # summary by item and theta
  # mean probability of correct across draws
  # mean item information across draws
  sum_by_item_theta <- draws_theta_final %>%
    expand(nesting(draw, itemnum, theta, p, i)) %>%
    group_by(itemnum, theta) %>%
    summarise(p = mean(p),
              i = mean(i), .groups = "drop")

  # summary by theta and draw
  # calculate test information and expected total score
  sum_by_draw_theta <- draws_theta_final %>%
    expand(nesting(draw, itemnum, theta, i, p)) %>%
    group_by(theta, draw) %>%
    summarise(test_i = sum(i),
              test_p = sum(p), .groups = "drop")

  # summary by theta
  # mean test information across draws
  sum_by_theta <- sum_by_draw_theta %>%
    group_by(theta) %>%
    summarise(test_i = mean(test_i),
              test_p = mean(test_p)) %>% ungroup()

  # summary by draw
  # reliability for each draw
  sum_by_draw <- sum_by_draw_theta %>%
    mutate(rel_by_theta = test_i/(test_i+1)) %>%
    group_by(draw) %>%
    mutate(weight = dnorm(theta),
           rel_with_weight = (rel_by_theta*weight/sum(weight, na.rm = TRUE)),
           rel = sum(rel_with_weight, na.rm = TRUE)) %>% slice(1) %>%
    select(draw, rel) %>% ungroup()

  # reliability across draws
  # for each theta, first calculate mean test information across all draws, then calculate reliability
  # then add up across theta based on weights
  final_rel <- sum_by_theta %>%
    mutate(rel_by_theta = test_i/(test_i+1)) %>%
    mutate(weight = dnorm(theta))  %>%
    mutate(rel_with_weight = (rel_by_theta*weight/sum(weight, na.rm = TRUE)))%>%
    mutate(rel = sum(rel_with_weight, na.rm = TRUE)) %>% slice(1) %>%
    pull(rel)


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

    sum_by_item_IPM <- sum_by_item %>%
      filter(itemnum %in% item_list_IPM)

    # Yaxis two digits
    scaleFUN <- function(x) sprintf("%.2f", x)

    # Box plot of item thresholds
    IPMplot <- ggplot(data = sum_by_item_IPM, aes(x = factor(itemnum), y = mean_tau)) +
      geom_point(size = 2, colour = "black") +
      geom_boxplot(aes(
        lower = q1,
        middle = q2,
        upper = q3,
        ymin = ymin,
        ymax = ymax),
        stat = "identity") + theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none", panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(x = "Item Number", y = "Item Threshold") +
      scale_y_continuous(labels = scaleFUN, position = "right", expand = expansion(mult = 0.05),
                         breaks = scales::breaks_pretty(n = 4)) +
      scale_x_discrete(limits = as.character(sort(unique(sum_by_item_IPM$itemnum)))) +
      theme(text = element_text(family = "Times New Roman", size = 12))

    print(IPMplot)
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

  # check item names, if provided, then must match with the number of items listed for plotting
  if (!is.null(item_names_ICC_IIC)) {
    if (length(item_names_ICC_IIC) != length(item_list_ICC_IIC)) {
      stop("number of item names must equal number of items to be plotted")
    }}


  ##########################################
  ### * Item Characteristic Curve Plot -----
  ##########################################
  if (ICC == 1){

    ## options to plot 1) each item separately 2) each item separately in a panel 3) overlay in the same plot
    ICCplot_option <- toupper(ICCplot_option)

    # set up dummy variables to indicate which type of plot is requested
    # set dummy variable to 1 if the plot is requested
    if ("ALL" %in% ICCplot_option) {
      ICC_separate <- 1
      ICC_panel <- 1
      ICC_overlay <- 1
    } else {
      ICC_separate <- as.integer("SEPARATE" %in% ICCplot_option)
      ICC_panel <- as.integer("PANEL" %in% ICCplot_option)
      ICC_overlay <- as.integer("OVERLAY" %in% ICCplot_option)
    }

    # plot each item separately
    if (ICC_separate == 1){
      # loop through each item
      for (i in 1:length(item_list_ICC_IIC)){

        item_plot = item_list_ICC_IIC[i]
        title_text <- ifelse(!is.null(item_names_ICC_IIC),
                             paste("2PL Item Characteristic Curve for", item_names_ICC_IIC[i]),
                             paste("2PL Item Characteristic Curve for Item", item_plot))

        ICCplot <- ggplot(subset(draws_theta_final, itemnum == item_plot & draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
                          aes(x = theta, y = p, colour = as.factor(draw))) +
          geom_line(alpha = 0.4) +
          scale_colour_grey() +
          labs(title = title_text,
               subtitle = paste(DrawsK, "draws are plotted"),
               x = expression(theta),
               y = expression(italic(p)(y == 1))) +
          geom_line(data = subset(sum_by_item_theta, itemnum == item_plot),
                    aes(x = theta, y = p), colour = "black", linetype = "solid", linewidth = 2) +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12))

        print(ICCplot)
        plots[[paste0("ICCplot_item", item_plot)]] <- ICCplot
      }}


    # plot items in a panel
    if (ICC_panel == 1){

      # split item list into groups (by default, a maximum of 9 plots will be plotted in a panel)
      item_groups <- split(
        item_list_ICC_IIC,
        ceiling(seq_along(item_list_ICC_IIC) / max_plots_panel)
      )

      # plot items in each group separately
      for (g in seq_along(item_groups)) {

        current_items <- item_groups[[g]]
        current_items <- sort(current_items)

        # subset to current items and needed draws
        plot_data <- subset(
          draws_theta_final,
          itemnum %in% current_items & draw %in% 1:as.integer(DrawsK)
        )
        # subset to current items
        sum_data <- subset(
          sum_by_item_theta,
          itemnum %in% current_items
        )

        # item names
        if (!is.null(item_names_ICC_IIC)) {
          current_names <- item_names_ICC_IIC[
            match(current_items, item_list_ICC_IIC)
          ]
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
            title = "2PL Item Characteristic Curve",
            subtitle = paste(DrawsK, "draws are plotted"),
            x = expression(theta),
            y = expression(italic(p)(y == 1))
          ) +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12))

        print(ICCplot_panel)
        plots[[paste0("ICCplot_panel", g)]] <- ICCplot_panel
      }}

    # overlay items in a panel
    if (ICC_overlay == 1){

      if (toupper(overlay_style) == "LINE" &&
          length(item_list_ICC_IIC) > 13) {
        stop("A maximum of 13 line types can be plotted in an overlaid plot. Consider ICCplot_option = 'panel' or 'separate', or use overlay_style == 'color'")
      }

      # item names
      sum_by_item_theta$item_label <-
        if (!is.null(item_names_ICC_IIC)) {
          factor(sum_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = item_names_ICC_IIC)
        } else {
          factor(sum_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = item_list_ICC_IIC)
        }

      # distinguish items by different colors
      if (toupper(overlay_style) == "COLOR") {

        ICCplot_overlay <- ggplot() +
          geom_line(
            data = sum_by_item_theta %>% filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = p,
                group = item_label,
                colour = item_label),
            linewidth = 1, alpha = 0.8) +
          labs(title = "2PL Item Characteristic Curve",
               colour = "Item",
               x = expression(theta),
               y = expression(italic(p)(y==1))) +
          theme_classic() +
          theme(text = element_text(family = "Times New Roman", size = 12)) +
          scale_color_hue(l = 70, c = 60)
      }

      # distinguish items by different line types
      else if (toupper(overlay_style) == "LINE") {

        ICCplot_overlay <- ggplot() +
          geom_line(
            data = sum_by_item_theta %>% filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = p,
                group = item_label,
                linetype = item_label),
            linewidth = 1) +
          labs(title = "2PL Item Characteristic Curve",
               linetype = "Item",
               x = expression(theta),
               y = expression(italic(p)(y==1))) +
          theme_classic() +
          theme(text = element_text(family = "Times New Roman", size = 12))
      }
      plots$ICCplot_overlay <- ICCplot_overlay
      print(ICCplot_overlay)
    }
  }

  #######################################
  ### * Item Information Curve Plot -----
  #######################################
  if (IIC == 1){

    ## options to plot 1) each item separately 2) each item separately in a panel 3) overlay in the same plot
    IICplot_option <- toupper(IICplot_option)

    # set up dummy variables to indicate which type of plot is requested
    # set dummy variable to 1 if the plot is requested
    if ("ALL" %in% IICplot_option) {
      IIC_separate <- 1
      IIC_panel <- 1
      IIC_overlay <- 1
    } else {
      IIC_separate <- as.integer("SEPARATE" %in% IICplot_option)
      IIC_panel <- as.integer("PANEL" %in% IICplot_option)
      IIC_overlay <- as.integer("OVERLAY" %in% IICplot_option)
    }

    # plot each item separately
    if (IIC_separate == 1){
      # loop through each item
      for (i in 1:length(item_list_ICC_IIC)){

        item_plot = item_list_ICC_IIC[i]
        title_text <- ifelse(!is.null(item_names_ICC_IIC),
                             paste("2PL Item Information Curve for", item_names_ICC_IIC[i]),
                             paste("2PL Item Information Curve for Item", item_plot))

        IICplot <-
          ggplot(subset(draws_theta_final, itemnum == item_plot & draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
                 aes(x = theta, y = i, color = as.factor(draw))) +
          geom_line(alpha = 0.4) +
          scale_colour_grey() +
          labs(title = title_text,
               subtitle = paste(DrawsK, "draws are plotted"),
               x = expression(theta),
               y = "Information") +
          geom_line(data = subset(sum_by_item_theta, itemnum == item_plot),
                    aes(x = theta, y = i), colour = "black", linetype = "solid", linewidth = 2) +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12))

        print(IICplot)
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
        current_items <- sort(current_items)

        # subset to current items and needed draws
        plot_data <- subset(
          draws_theta_final,
          itemnum %in% current_items & draw %in% 1:as.integer(DrawsK)
        )
        # subset to current items
        sum_data <- subset(
          sum_by_item_theta,
          itemnum %in% current_items
        )

        # item names
        if (!is.null(item_names_ICC_IIC)) {
          current_names <- item_names_ICC_IIC[
            match(current_items, item_list_ICC_IIC)
          ]
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
            title = "2PL Item Information Curve",
            subtitle = paste(DrawsK, "draws are plotted"),
            x = expression(theta),
            y = "Information") +
          theme_classic() +
          theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12))

        print(IICplot_panel)
        plots[[paste0("IICplot_panel", g)]] <- IICplot_panel
      }}

    # overlay items in a panel
    if (IIC_overlay == 1){

      if (toupper(overlay_style) == "LINE" &&
          length(item_list_ICC_IIC) > 13) {
        stop("A maximum of 13 line types can be plotted in an overlaid plot. Consider IICplot_option = 'panel' or 'separate', or use overlay_style == 'color'")
      }

      # item names
      sum_by_item_theta$item_label <-
        if (!is.null(item_names_ICC_IIC)) {
          factor(sum_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = item_names_ICC_IIC)
        } else {
          factor(sum_by_item_theta$itemnum, levels = item_list_ICC_IIC, labels = item_list_ICC_IIC)
        }

      # distinguish items by different colors
      if (toupper(overlay_style) == "COLOR") {

        IICplot_overlay <- ggplot() +
          geom_line(
            data = sum_by_item_theta %>% filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = i,
                group = item_label,
                colour = item_label),
            linewidth = 1, alpha = 0.8) +
          labs(title = "2PL Item Information Curve",
               colour = "Item",
               x = expression(theta),
               y = "Information") +
          theme_classic() +
          theme(text = element_text(family = "Times New Roman", size = 12)) +
          scale_color_hue(l = 70, c = 60)
      }

      # distinguish items by different line types
      else if (toupper(overlay_style) == "LINE") {

        IICplot_overlay <- ggplot() +
          geom_line(
            data = sum_by_item_theta %>% filter(itemnum %in% item_list_ICC_IIC),
            aes(x = theta, y = i,
                group = item_label,
                linetype = item_label),
            linewidth = 1) +
          labs(title = "2PL Item Information Curve",
               linetype = "Item",
               x = expression(theta),
               y = "Information") +
          theme_classic() +
          theme(text = element_text(family = "Times New Roman", size = 12))
      }
      plots$IICplot_overlay <- IICplot_overlay
      print(IICplot_overlay)
    }

  }


  #######################################
  ### * Test Characteristic Curve Plot -----
  #######################################
  if (TCC == 1){

    TCCplot <-
      ggplot(subset(sum_by_draw_theta, draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
             aes(x = theta, y = test_p, color = as.factor(draw))) +
      geom_line(alpha = 0.4)  +
      scale_colour_grey() +
      labs(title = "2PL Test Characteristic Curve",
           subtitle = paste(DrawsK, "draws are plotted"),
           x = expression(theta),
           y = "Expected Score") +
      geom_line(data = sum_by_theta, aes(x = theta, y = test_p), colour = "black", linetype = "solid", linewidth = 2) +
      theme_classic() +
      theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12))

    print(TCCplot)
    plots$TCCplot <- TCCplot
  }

  #######################################
  ### * Test Information Curve Plot -----
  #######################################
  if (TIC == 1){

    TICplot <-
      ggplot(subset(sum_by_draw_theta, draw %in% c(1:as.integer(DrawsK))), # subset to DrawsK number of draws
             aes(x = theta, y = test_i, color = as.factor(draw))) +
      geom_line(alpha = 0.4)  +
      scale_colour_grey() +
      labs(title = "2PL Test Information Curve",
           subtitle = paste(DrawsK, "draws are plotted"),
           x = expression(theta),
           y = "Information") +
      geom_line(data = sum_by_theta, aes(x = theta, y = test_i), colour = "black", linetype = "solid", linewidth = 2) +
      theme_classic() +
      theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12))

    print(TICplot)
    plots$TICplot <- TICplot
  }

  ############################
  ### * Reliability Plot -----
  ############################
  if (REL == 1){

    RELplot <- ggplot(sum_by_draw, aes(x = rel, color = as.factor(draw))) +
      geom_histogram() +
      scale_x_continuous(breaks = seq(0, 1, by = .01)) +
      theme_classic() + theme(legend.position = "none") +
      scale_colour_grey() +
      labs(title = "Marginal Reliability Plot",
           x = "Reliability") +
      geom_vline(xintercept = final_rel, color = "black", linetype = "longdash", linewidth = 1) +
      theme(text = element_text(family = "Times New Roman", size = 12),
            axis.text.y = element_blank(),      #numbers
            axis.ticks.y = element_blank(),     #ticks
            axis.title.y = element_blank())     #axis title

    print(RELplot)
    plots$RELplot <- RELplot

    # mean reliability value
    print(paste0("Reliability: ", sprintf("%.3f", final_rel)))
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


}

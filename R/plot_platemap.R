#' Platemap for a given data
#'
#' Generic function for plotting a map of the given data and plateformat.
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param plot_data A dataframe with three columns such as first column
#' representing the text characters used to label the y-axis, second column
#' representing the character used to label the x-axis of platemap, third column
#' representing the data subtypes.
#' @param legend_txt_bg_col Named vector representing the data subtypes as legend
#' text and colors as their names
#' @param las2 numeric in {0,1,2,3}; the style of axis labels. for more info,
#' refer las in \code{help(par)}
#' @param las3 numeric in {0,1,2,3}; the style of axis labels. for more info
#' refer las in \code{help(par)}
#' @param cex_axis size of text in the axis
#' @param legend_pch plotting character or symbol for legend label. for more
#' info, refer \code{help(pch)}
#' @param symbol_size A numeric vector representing the size of symbols plotted
#' at any coordinates of the plotting region
#' @param legend_text_width the width of the legend text in x ("user") coordinates.
#' (Should be positive even for a reversed x axis.) Defaults to the proper value computed by strwidth(legend).
#' @param legend_ncol the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param legend_posX the x co-ordinates to be used to position the legend.
#' @param legend_posY the y co-ordinates to be used to position the legend.
#' @param legend_symbol_size size of symbols used in the legend label.
#' @param legend_text_size size of text used in the legend label.
#' @param ... Arguments to be passed to methods, such as graphical parameters (see \code{help(par)}).
#' Many methods will accept the following arguments:
#'
#' @return A figure with a map of the plate with data subtypes color coded as
#' specified in \code{legend_txt_bg_col}
#' @export
#'
#' @examples
#' legend_txt_bg_col <- c('Empty'                     = 'red',
#'                        'Pinning Error'             = 'black',
#'                        'Morethan Plate Median'     = '#660066',
#'                        'Lessthan Plate Median'     = 'green',
#'                        'Morethan 90% Plate Median' = 'cyan',
#'                        'Lessthan 25% Plate Median' = 'yellow',
#'                        'Excluded Colonies'         = 'blue')
#' # 384 plate format
#' plateformat <- 384
#' base_data <- colonyarea$data_subtypes
#' across_384 <- convert_down_across(plateformat = plateformat,
#'                                   data_from = base_data,
#'                                   is_plate_coords = TRUE,
#'                                   out_data_flow = 'across')
#'
#' down_384 <- convert_down_across(plateformat = plateformat,
#'                                 data_from = across_384[,3],
#'                                 is_plate_coords = TRUE,
#'                                 out_data_flow = 'down')
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = across_384,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = down_384,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
#' # 1536 plate format
#' plateformat <- 1536
#' across_1536 <- convert_small_to_large(plate_from = 384,
#'                                       plate_to = plateformat,
#'                                       data_from = across_384[,3],
#'                                       out_data_flow = 'across',
#'                                       is_plate_coords = TRUE)
#'
#' down_1536 <- convert_down_across(plateformat = plateformat,
#'                                  data_from = across_1536[,3],
#'                                  is_plate_coords = TRUE,
#'                                  out_data_flow = 'down')
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = across_1536,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = down_1536,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
#' # 6144 plate format
#' plateformat <- 6144
#' across_6144 <- convert_small_to_large(plate_from = 1536,
#'                                       plate_to = plateformat,
#'                                       data_from = across_1536[,3],
#'                                       out_data_flow = 'across',
#'                                       is_plate_coords = TRUE)
#'
#' down_6144 <- convert_down_across(plateformat = plateformat,
#'                                  data_from = across_6144[,3],
#'                                  is_plate_coords = TRUE,
#'                                  out_data_flow = 'down')
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = across_6144,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = down_6144,
#'               legend_txt_bg_col = legend_txt_bg_col)
plot_platemap <- function(plateformat,
                          plot_data,
                          legend_txt_bg_col,
                          las2 = 2,
                          las3 = 0,
                          cex_axis = 1.2,
                          legend_pch = 21,
                          symbol_size = rep(0.3, plateformat),
                          legend_text_width = NULL,
                          legend_ncol = 4,
                          legend_posX = 0,
                          legend_posY = -1,
                          legend_symbol_size = 1.8,
                          legend_text_size = 1.2,
                          ...)
{
  # get user graphical parameters and set it before exiting from this function
  op <- par(c('xpd', 'ann', 'cex.axis', 'las', 'bg', 'pch', 'cex', 'bty'))
  on.exit(par(op))

  y  <- plot_data[, 1]
  x <- plot_data[, 2]
  z <- plot_data[, 3]
  uniq_y <- unique(y)
  uniq_x <- unique(x)
  uniq_z <- unique(z)

  # convert the data subtypes into z with numeric data type
  data_categs_counter <- 1
  for(data_categ in uniq_z){
    z[which(z %in% data_categ)] <- data_categs_counter
    data_categs_counter <- (data_categs_counter + 1)
  }
  z <- as.numeric(z)

  # draw a dummy plot and set plot region limits
  plot(x    = NA,
       y    = NA,
       xlim = c(0.5, (length(uniq_x) + 0.5)),
       ylim = c(0.5, (length(uniq_y) + 0.5)),
       ann  = FALSE,
       axes = FALSE)

  # draw box
  box()

  # add axis labels
  axis(side     = 2,
       at       = seq_along(uniq_y),
       labels   = rev(uniq_y),
       las      = las2,
       cex.axis = cex_axis)

  axis(side     = 3,
       at       = seq_along(uniq_x),
       labels   = uniq_x,
       las      = las3,
       cex.axis = cex_axis)

  #setup background colors for plot region symbols
  symbol_bg_colors <- legend_txt_bg_col[uniq_z]
  colfunc <- colorRampPalette(symbol_bg_colors)
  colgrp <- findInterval(z, unique(z))
  collist <- colfunc(length(unique(colgrp)))

  # draw plot region symbols
  symbols(x       = x,
          y       = factor(y, levels = rev(uniq_y)),
          circles = symbol_size,
          add     = TRUE,
          inches  = FALSE,
          bg      = collist[colgrp])

  # draw legend
  legend_text <- names(symbol_bg_colors)

  legend2(x          = legend_posX,
          y          = legend_posY,
          legend     = legend_text,
          ncol       = legend_ncol,
          pch        = legend_pch,
          pt.bg      = collist,
          xpd        = TRUE,
          cex        = legend_text_size,
          bty        = 'n',
          pt.cex     = legend_symbol_size,
          text.width = legend_text_width)

  invisible(TRUE)
}


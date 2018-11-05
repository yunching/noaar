GeomTimeline <- ggplot2::ggproto(
  `_class` = "GeomTimeline", `_inherit` = ggplot2::Geom,
  setup_data = function(data, params) {
    # str(data)
    if (is.null(data$y)) {
      data$y <- 0 
    }
    if (is.null(data$xmin)) {
      data$xmin <- min(data$x, na.rm = TRUE)
    }
    if (is.null(data$xmax)) {
      data$xmax <- max(data$x, na.rm = TRUE)
    }
    # str(data)
    data
  },
  required_aes = "x",
  default_aes = ggplot2::aes(
    shape = 19, colour = "grey", fill = NA, alpha = NA, stroke = 1, size = 1
  ), 
  draw_key = draw_key_point,
  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    # str(data)
    # str(coords)
    # Resize point size to something reasonable
    coords$size <-coords$size / max(coords$size) * 1
    dates_grob <- grid::pointsGrob(
      coords$x,
      coords$y,
      pch = coords$shape,
      # size = unit(coords$size, "char"),
      gp = grid::gpar(
        col = coords$colour,
        cex = coords$size,
        fill = scales::alpha(coords$fill, alpha = coords$alpha)
      )
    )
    axis_grob <-
      grid::polylineGrob(x = c(coords$xmin, coords$xmax), c(coords$y, coords$y), id = c(coords$group, coords$group))
    bottom_axis_grob <- grid::linesGrob(y = c(0, 0))
    
    grid::gList(dates_grob, axis_grob, bottom_axis_grob)
  }
)

geom_timeline <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
}

GeomTimelineLabel <- ggplot2::ggproto(
  `_class` = "GeomTimelineLabel", 
  `_inherit` = ggplot2::Geom, 
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(n_max = NA), 
  # draw_key = draw_key_point,
  setup_data = function(data, params) {
    if (is.numeric(data$n_max[1])) {
      data %>% 
        group_by("group") %>% 
        top_n(n, size)
    } else {
      data
    }
  },
  
  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    
    segments_grob <- grid::segmentsGrob(
        x0 = coords$x,
        y0 = coords$y,
        x1 = coords$x,
        y1 = coords$y + 0.1,
        gp = grid::gpar()
      )
    
      labels_grob <- grid::textGrob(
        x = coords$x,
        y = coords$y + 0.1,
        label = coords$label,
        rot = 45,
        hjust = -0.1,
        vjust = -0.1,
        gp = grid::gpar()
      )
    
    grid::gList(segments_grob, labels_grob)
  }
)
    
geom_timelinelabel <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimelineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

cleaned_data %>%
  # filter(country %in% c("CHINA", "MEXICO")) %>% 
  filter(date >= lubridate::ymd("2000-01-01")) %>%
  filter(date < lubridate::ymd("2017-01-01")) %>%
  # sample_n(10) %>% 
  filter(!is.na(intensity)) %>%
  filter(!is.na(deaths)) %>%
  ggplot(mapping = aes(x = date)) +
  geom_timeline(aes(size = intensity, color = deaths)) + theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x = element_line(),
    axis.line.x = element_line(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + geom_timelinelabel(aes(label = location_name))


GeomTimeline <- ggproto(
  `_class` = "GeomTimeline", `_inherit` = ggplot2::Geom,
  setup_data = function(data, params) {
    str(data)
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
     str(coords)
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

cleaned_data %>%
  filter(country %in% c("CHINA", "MEXICO")) %>% 
  filter(date >= lubridate::ymd("2000-01-01")) %>%
  filter(date < lubridate::ymd("2017-01-01")) %>%
  # filter(!is.na(intensity)) %>%
  # filter(!is.na(deaths)) %>%
  ggplot(mapping = aes(x = date, y = country, size = intensity, color = deaths)) +
  geom_timeline() + theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background =element_blank(),
    axis.ticks.x = element_line(),
    axis.line.x = element_line()
  )

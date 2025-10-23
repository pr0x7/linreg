#' Visualize Airport Delays
#' @importFrom dplyr group_by summarise inner_join filter
#' @importFrom magrittr %>%
#' @export
visualize_airport_delays <- function() {
  flights_data <- nycflights13::flights
  airports_data <- nycflights13::airports

  delay_summary <- flights_data %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(mean_delay = mean(arr_delay, na.rm = TRUE),
                     .groups = 'drop')

  airport_delays <- airports_data %>%
    dplyr::inner_join(delay_summary, by = c("faa" = "dest")) %>%
    dplyr::filter(!is.na(lon), !is.na(lat), !is.na(mean_delay))

  plot <- ggplot2::ggplot(airport_delays, ggplot2::aes(x = lon, y = lat, color = mean_delay)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::scale_color_gradient2(low = "green", mid = "yellow", high = "red",
                                   midpoint = 0, name = "Mean Delay (min)") +
    ggplot2::labs(title = "Mean Arrival Delays by Airport",
                  x = "Longitude",
                  y = "Latitude") +
    ggplot2::theme_minimal()

  return(plot)
}



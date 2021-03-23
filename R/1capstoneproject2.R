#' Reading the NOAA earthquake data file
#'
#' @param The filename of the NOAA earthquake data file
#' @return tbl_df object (earthquake data)
#' @note Stop if the filename does not exist (error message)
#' @import dplyr
#' @import tibble
#' @importFrom readr read_delim
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes.tsv",package="mycapstone") #xx
#' eq_data_read(filename)
#' }
#'
#' @export
eq_data_read <- function(filename) {
    if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(filename, delim='\t',progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' cleans the LOCATION_NAME column by stripping out the country name (including the colon) and
#' converts names to title case (as opposed to all caps)
#' @param mydf that contains location names written in upper case
#' @return a dataframe filtered required for mapping in a timeline the data
#' @importFrom tidyr unite drop_na
#' @importFrom stringi stri_trans_totitle
#' @import %>%
#' @import lubridate
#' @import dplyr
#' @examples
#'\dontrun{
#' filename<-system.file("data","earthquakes.tsv",package="mycapstone") ##XX
#' eq_clean_data(eq_data_read(filename))
#' }
#'
#' @export
  eq_clean_data<-function(mydf){
    clean_data <- mydf%>%
      dplyr::rename(COUNTRY=Country,LOCATION_NAME='Location Name',LATITUDE=Latitude, LONGITUDE=Longitude,YEAR=Year, MONTH=Mo, DAY=Dy, HOUR=Hr, EQ_MAG_ML='MMI Int',DEATHS=Deaths)%>%
      dplyr::select(COUNTRY,LOCATION_NAME, LATITUDE, LONGITUDE,YEAR, MONTH, DAY, HOUR, EQ_MAG_ML,DEATHS) %>%
      dplyr::mutate(LOCATION_NAME=gsub(".*:", "", LOCATION_NAME))%>%
      dplyr::mutate(LATITUDE= as.numeric(LATITUDE)) %>%
      dplyr::mutate(LONGITUDE= as.numeric(LONGITUDE))%>%
      tidyr::unite(datetime, YEAR, MONTH, DAY, HOUR) %>%
      dplyr::mutate(datetime = lubridate::ymd_h(datetime))%>%
      dplyr::mutate(DEATHS=as.numeric(DEATHS))

      eq_location_clean(clean_data)

  }

  #' title case the Earthquake's Location Data-Name
  #' @param mydf contains location names written in Uper case
  #' @return contains the Eathquake data filtered required for mapping in a timeline the data and the Tittle Case Location
  #' @importFrom stringi stri_trans_totitle
  #'@examples
  #'\dontrun{
  #' filename<-system.file("data","earthquakes.tsv",package="mycapstone")
  #' eq_location_clean(eq_clean_data(eq_data_read(filename)))
  #' }
  #'
  #' @export
  eq_location_clean<-function(mydf){
    LOCATION_NAME<-NULL
    mydf = mydf%>%
      dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
    mydf
  }

# use the GeomTimeLine Prototype Function required to Plot a Timeline with the Earthquakes of a given country
#' @param mapping aesthetic mappings created by aes
#' @param data is the dataframe that contains the Earthquake's data
#' @param na.rm  will hepls to remove the NA values from the data frame
#' @param position position adjustment functio
#' @param stat The Layer's statistical transformation
#' @param show.legend layer's legend
#' @param inherit.aes will indicate the default aesthetics overridng
#' @param ... layer's other arguments
#' @return plot of an Earthquakes timeline which contains all Earthquakes of a Given Country or List of Countries between a set of dates
#' @import ggplot2
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes.tsv",package="mycapstone")
#' eq_clean_data(eq_data_read(filename)) %>%
#' dplyr::filter(datetime >= "1990-01-01" & datetime <="2018-01-01" & COUNTRY %in% c("MEXICO","USA", "JORDAN"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          na.rm = TRUE,
                          position = "identity",
                          stat = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    Geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}


#'Ploting an Earthquake's Location timeline
#'building a GEOM Function from scratch
#'use a Dataframe compiled using the function eq_clean_data.
#'a prototype function as foundation for geom_timeline function.
#'use the ggplot2's geom_point.
#'use the Earthquakes' dates as X-axis main values
#'The geom_point's size and colour defined by the Earthquake's magnitude
#'The GeomTimeLine was build using the Function Prototype provided in the Course's Material 4.7.1 Building a New Geom
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 #<character vector of required aesthetics>
                                 required_aes = c("x"),
                                 #aes(<default values for certain aesthetics>)
                                 default_aes = ggplot2::aes(y = 0.1,
                                                            shape = 21,
                                                            size = 1,
                                                            colour = "blue",
                                                            alpha = 0.8,
                                                            stroke = 1,
                                                            fill = NA),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord) {
                                   coords <- coord$transform(data, panel_scales)
                                   Timeline_line_grobs <- grid::polylineGrob(x = grid::unit(rep(c(0, 1),
                                                                                                length(coords$y)),
                                                                                            "npc"),
                                                                             y = rep(coords$y, each = 2),
                                                                             id.length = rep(2,length(coords$y)),
                                                                             gp = grid::gpar(col = "black", lwd = 0.3, lty = 1))

                                   Earthquakes_points_grobs <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(col = alpha(coords$colour, coords$alpha), fill = alpha(coords$fill, coords$alpha),
                                                     lwd = coords$stroke * .stroke / 2),
                                     fontsize = coords$size * .pt + coords$stroke * .stroke / 2
                                    )
                                   grid::gTree(children = grid::gList(Timeline_line, Earthquakes_points_grobs))
                                 })


#' Funcion for adding the Eartquakes's Location labels to an Earthquake's timeline
#' @param mapping aesthetic mappings created by aes
#' @param data is the dataframe that contains the Earthquake's data
#' @param na.rm  will hepls to remove the NA values from the data frame
#' @param show.legend layer's legend
#' @param stat The Layer's statistical transformation
#' @param position position adjustment functio
#' @param inherit.aes will indicate the default aesthetics overridng
#' @param ... layer's other arguments
#' @return the Earthquake's labels
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes.tsv",package="mycapstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(datetime >= "1980-01-01" & datetime <="2018-01-01" & COUNTRY %in% c("MEXICO","USA", "JORDAN"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
#' geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))
#'}
#'

#' @export
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                na.rm = TRUE,
                                show.legend = NA,
                                stat = "identity",
                                position = "identity",
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeLineAnnotation,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomTimeLineAnnotation <- ggplot2::ggproto("GeomTimeLineAnnotation", ggplot2::Geom,
                                           required_aes = c("x", "tags"),
                                           default_aes = ggplot2::aes(y = 0.5,
                                                                      number = NULL,
                                                                      max_aes = NULL),

                                           draw_panel = function(data, panel_scales, coord) {

                                             coords <- coord$transform(data, panel_scales)
                                             Timeline_seg_grobs <- grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"),
                                                                                      y0 = grid::unit(coords$y, "npc"),
                                                                                      x1 = grid::unit(coords$x, "npc"),
                                                                                      y1 = grid::unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                      default.units = "npc",
                                                                                      arrow = NULL,
                                                                                      name = NULL,
                                                                                      gp = grid::gpar(),
                                                                                      vp = NULL)

                                             Earthquake_text_grobs <- grid::textGrob(label = coords$tags,
                                                                                     x = unit(coords$x, "npc"),
                                                                                     y = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                     rot = 60,
                                                                                     just = "left",
                                                                                     gp = grid::gpar(fontsize = 8))

                                             grid::gTree(children = grid::gList(Timeline_seg_grobs, Earthquake_text_grobs))
                                           }
)


#' Earthquakes Data in an Interactive Map.
#'
#' mapped centered with their latitude and longitude "epicenter" which is annotated based on an annot_col which the user can specify.
#' plus specifies "popup_text" by a call to eq_create_label generates the appropriate text..
#'
#' @references \url{http://rstudio.github.io/leaflet/}
#'
#' @param eq_clean The clean earthquake data in a tbl_df object.
#' @param annot_col Column in the tbl_df object to be used for annotation.
#'
#' @return  returns an interactive map.
#'
#' @note  warning an invalid column name & uses the LOCATION_NAME column as annotation column.
#'
#' @import leaflet
#' @import %>%
#'
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes.tsv",package="mycapstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 1980) %>%
#' eq_map(annot_col = "datetime")
#' }
#'
#' @export
eq_map <- function(eq_clean=NULL, annot_col="datetime"){

  all_columns <- colnames(eq_clean)
  stopifnot(any('datetime' %in% all_columns),any('LATITUDE' %in% all_columns),
            any('LONGITUDE' %in% all_columns),any('EQ_MAG_ML' %in% all_columns))
  if(!(any(annot_col %in% all_columns))) {
    warning("Invalid Column - DATE Displayed")
    annot_col = "datetime"
  }
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = eq_clean, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_MAG_ML,
                              weight=1, fillOpacity = 0.2, popup =~ paste(get(annot_col)))
  }

#' Creates pop up text for markers.
#' generates HTML formatted text for popups for map markers.
#'
#' @param eq_clean The clean earthquake data in a tbl_df object.
#' @return returns a character vector containing popup text to be used in a leaflet visualization.
#' @import dplyr
#' @import %>%
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes.tsv",package="mycapstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 1980) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(eq_clean=NULL) {
  all_columns <- colnames(eq_clean)
  stopifnot(any('LOCATION_NAME' %in% all_columns),any('EQ_MAG_ML' %in% all_columns),
            any('DEATHS' %in% all_columns))
  data2<- eq_clean %>% dplyr::select_(.dots=c('LOCATION_NAME','EQ_MAG_ML','DEATHS')) %>%
    dplyr::mutate(new_LOCATION_NAME =  ifelse(is.na(LOCATION_NAME), LOCATION_NAME, paste0("<b>Location:</b> ", LOCATION_NAME,"<br />"))) %>%
    dplyr::mutate(new_EQ_PRIMARY = ifelse(is.na(EQ_MAG_ML), EQ_MAG_ML, paste0("<b>Magnitude:</b> ", EQ_MAG_ML,"<br />"))) %>%
    dplyr::mutate(new_DEATHS =  ifelse(is.na(DEATHS), DEATHS, paste0("<b>Total Deaths:</b> ", DEATHS))) %>%
    tidyr::unite('popup_values',c('new_LOCATION_NAME','new_EQ_PRIMARY','new_DEATHS'),sep ='') %>%
    dplyr::mutate(popup_values =  stringr::str_replace_all(popup_values,"[,]*NA[,]*","")) %>%
    dplyr::mutate(popup_values =  ifelse(popup_values=="","All Values are NA",popup_values))

  popup_values <- dplyr::collect(dplyr::select(data2,.dots=c('popup_values')))[[1]]

  return(popup_values)

}


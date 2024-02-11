#' Shiny GUI for GeoWeightedModel package
#'@import shiny
#'@import beepr
#'@importFrom DT DTOutput
#'@importFrom DT renderDT
#'@import GWmodel
#'@import cartography
#'@import dplyr
#'@importFrom raster shapefile
#'@import readxl
#'@importFrom shinyBS bsModal
#'@import shinyWidgets
#'@importFrom shinyalert useShinyalert
#'@import shinybusy
#'@importFrom shinyjs useShinyjs
#'@import sp
#'@import spdep
#'@import shinydashboard
#'@description runGeoWeightedModel() loads interactive user interface built using R 'shiny'.
#'@details The interactive user interface to provide an easy way for to perform techniques from a subset of
#'spatial statistics known as geographically weighted models.
#' @param launch.browser If true,
#' the system's default web browser will be launched automatically
#' after the app is started. Defaults to true in interactive sessions
#' only. This value ofthis parameter can also be a function to call with
#' the application's URL.
#' @param port is the TCP port that the application should listen on.
#' If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)),
#' then that port will be used.
#' Otherwise, use a random port.
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#' @return No return value
#' @examples
#' if(interactive()){
#' runGeoWeightedModel()
#' }
#' @export


runGeoWeightedModel <- function(host = "127.0.0.1",
                                port = NULL,launch.browser = TRUE) {
  appDir <- system.file("GeoweightedModelApp", package = "GeoWeightedModel")
  if (appDir == "") {
    stop("Could not find GeoWeightedModel.
         Try re-installing `GeoWeightedModel`.", call. = FALSE)
  }

  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = launch.browser,
                port = port,
                host = getOption("shiny.host", host)
  )
}

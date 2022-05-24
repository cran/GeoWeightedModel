data(USelect)
shiny::shinyServer(function(input,output,session) {
  options(shiny.maxRequestSize=2000*1024^2)
  values <- reactiveValues()

  #--------------# Upload example #---------------#
  output$example <- DT::renderDT({
    if(input$example == FALSE){return()}
      data(USelect)
      data <- USelect2004@data
      DT::datatable(data, options = list(
      pageLength = 5,
      scrollCollapse = T,
      scrollX = TRUE,
      fixedColumns = TRUE,
      lengthMenu = c(10, 50, 100, 500)))
    })

  #--------------# Upload data #---------------#
  observe({
    inFile <- input$file1
    if (is.null(inFile)) {
      return (NULL)
    }
    else {#Set up the selection for the worksheet names within the selected file
      filePath <<- inFile$datapath
      selectionWorksheet <- sort( unique(readxl::excel_sheets(inFile$datapath)))
      updateSelectInput(session, "worksheet", choices = selectionWorksheet)
    }
  })

  observe({
    if(is.null(input$file1)) {
      shinyjs::disable("getData")
    }
    else if(input$example == TRUE) {
      shinyjs::disable("getData")
    }
    else {shinyjs::enable("getData")
    }
  })

  z <- reactiveValues(dat = NULL,
                      map = NULL,
                      SPDF = NULL,
                      uploaddirectory = NULL,
                      shpdf=NULL )

  observeEvent(input$getData, {# Get the data from the spreadsheet worksheets
    dat <- readxl::read_excel (filePath, sheet=input$worksheet)
   z$dat <- as.data.frame(unclass(dat), stringsAsFactors = TRUE)
   z$dat
  })

  output$table1 <- DT::renderDT({
     req(z$dat)
    DT::datatable(z$dat, options = list(
      pageLength = 5,
      scrollCollapse = T,
      scrollX = TRUE,
      fixedColumns = TRUE,
      lengthMenu = c(10, 50, 100, 500)))
    })
  #--------------# Upload shapefile #---------------#
  observe({
    if(is.null(input$filemap)) {
      shinyjs::disable("getshape")
    }
    else if(input$example == TRUE) {
      shinyjs::disable("getshape")
    }
    else {shinyjs::enable("getshape")
    }
  })

  observe({
    z$shpdf <- input$filemap
    if(is.null(z$shpdf)){
      return()
    }
    previouswd <- getwd()
    z$uploaddirectory <- dirname(z$shpdf$datapath[1])
    setwd(z$uploaddirectory)
    for(i in 1:nrow(z$shpdf)){
      file.rename(z$shpdf$datapath[i], z$shpdf$name[i])
    }
    setwd(previouswd)
    })

  observeEvent(input$getshape,{
      shinybusy::show_modal_spinner(
        spin = "atom",
        color = "#428ded",
        text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
      )
    map <- raster::shapefile(paste(z$uploaddirectory,
                                   z$shpdf$name[grep(pattern="*.shp$",
                                                     z$shpdf$name)], sep="/"))
    map <- spTransform(map,
                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    z$map <- map
    shinybusy::remove_modal_spinner()
    beepr:: beep(2)
  })


  observe({
    req(z$dat)
    values$ovarsID <- names(z$dat)
    updateSelectizeInput(session,
                         'colID', 'Select ID for merge data',
                         choices = values$ovarsID,
                         #multiple = TRUE,
                         server = TRUE)
    })
  # select for merge data
  datasel <- reactive({
    req(z$dat)
    datasel <- merge(z$map,z$dat, by = input$colID)
    datasel
    })

  output$shpi <- renderPrint({
    if (is.null(z$map)) {
      return (NULL)
    }
    names(z$map)
  })

#--------------# Distance matrix #---------------#

    observe({
      if(input$example== TRUE || !is.null(z$map)) {
        shinyjs::enable("Run1")
      }
      else {shinyjs::disable("Run1")}
    })


    observeEvent(input$Run1,{

     shinybusy::show_modal_spinner(
      spin = "atom",
      color = "blue",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while")

    if( input$example == TRUE){
      values$dMat <- gw.distGW(dp.locat = sp::coordinates(USelect2004),
                               focus = input$focus,
                               theta = input$theta*pi,
                               p = input$power ,
                               longlat = input$longlat)
    }
    else{
       values$dMat <- gw.distGW(dp.locat = sp::coordinates(datasel()),
                                    focus = input$focus,
                                    theta = input$theta*pi,
                                    p = input$power ,
                                    longlat = input$longlat)
      }
    shinybusy::remove_modal_spinner()
    beepr:: beep(2)

  })

  output$distmatrix <-  DT::renderDT({
    req(values$dMat)
    datmat <- data.frame(values$dMat)%>%
      dplyr::mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data = datmat, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })
  ############ Variable selection for bw (bandwidth)
  #--------- bw.gwr------#
  observe({
    if(input$example== TRUE){
    updateSelectizeInput(session,
                         'dependientgwr', 'Dependent',
                         choices =  USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE
    )
    updateSelectizeInput(session,
                         'independientgwr',
                         'Independient(s)',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
  })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientgwr', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )
    updateSelectizeInput(session,
                         'independientgwr',
                         'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientggwr', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )

    updateSelectizeInput(session,
                         'independientggwr',
                         'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })
  #--------- bw.gwda------#
  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                         'depbwgda', 'Dependent',
                         choices =  USelect2004@data%>%
                           dplyr::select_if(is.factor) %>% names,
                         #multiple = TRUE,
                         server = TRUE)}
  })
  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.factor) %>% names
    updateSelectizeInput(session,
                         'depbwgda', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    if(input$example == TRUE){
    updateSelectizeInput(session,
                         'indepbwgda', 'Independient(s)',
                         choices = USelect2004@data%>%
                           dplyr::select_if(is.numeric) %>% names,
                         #multiple = TRUE,
                         server = TRUE)}
  })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'indepbwgda', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })
  #---------bw.gwpca------#
  observe({
    if(input$example == TRUE){
     updateSelectizeInput(session,
                        'varpca', 'Variables',
                         choices = USelect2004@data%>%
                          dplyr::select_if(is.numeric) %>% names,
                         #multiple = TRUE,
                         server = TRUE
   )}
  })
  observe({
    if(input$example == FALSE){
      req(z$dat)
      ovarsbw <- z$dat %>% dplyr::select_if(is.numeric) %>% names
      updateSelectizeInput(session,
                           'varpca', 'Variables',
                           choices = ovarsbw,
                           #multiple = TRUE,
                           server = TRUE)}
    })

    observe({
      updateNumericInput(session,'kpca',
                       'k',
                       2,
                       min = 2,
                       max = length(input$varpca)-1)
  })

  datapca <- reactive({
    if(input$example ==FALSE){
      mf <- datasel()[, input$varpca]
      data.scaled <- scale(as.matrix(mf@data[, input$varpca]))
      coords <- sp::coordinates(datasel())
      datapca <- SpatialPointsDataFrame(coords, as.data.frame(data.scaled))
      datapca}
 })

# datapca <- reactive({
   # if(input$example == TRUE){
   # mf <- USelect2004[, input$varpca]
   # data.scaled <- scale(as.matrix(mf@data[, input$varpca]))
    #coords <- sp::coordinates(USelect2004)
   # datapca <- SpatialPointsDataFrame(coords, as.data.frame(data.scaled))
   # datapca}
 # })

  ## bw calculation

  observe({
    if (isTRUE(!is.null(input$independientgwr))) {
      shinyjs::enable("Runbw")
    }
    else if(isTRUE(!is.null(input$indepbwgda))) {
      shinyjs::enable("Runbw")
    }

    else if(isTRUE(!is.null(input$independientggwr))) {
      shinyjs::enable("Runbw")
    }
    else if(isTRUE(!is.null(input$varpca))) {
      shinyjs::enable("Runbw")
    }
    else {shinyjs::disable("Runbw")
    }
  })


observeEvent(input$Runbw, {
  req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "#428ded",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example== FALSE){
    if(input$bandSelect == "bw.gwr"){
      formu <- as.formula(paste(input$dependientgwr," ~ ",
                                paste(input$independientgwr,collapse="+")))
    values$bw <-  GWmodel::bw.gwr(formula = formu,
                                  data = datasel(),
                                  approach = input$approachgwr,
                                  kernel = input$kernelgwr,
                                  adaptive = input$adaptativegwr,
                                  p = input$powergwr,
                                  theta = input$thetagwr*pi,
                                  longlat= input$longlatgwr,
                                  dMat = values$dMat)}

    else if(input$bandSelect=="bw.ggwr"){
      formu <- as.formula(paste(input$dependientggwr," ~ ",
                                paste(input$independientggwr,collapse="+")))
    values$bw <-  GWmodel::bw.ggwr(formula = formu,
                                   data = datasel(),
                                   family = input$familyggwr,
                                   approach = input$approachggwr,
                                   kernel = input$kernelggwr,
                                   adaptive = input$adaptativeggwr,
                                   p = input$powerggwr,
                                   theta = input$thetaggwr*pi,
                                   longlat= input$longlatggwr,
                                   dMat = values$dMat)}
    else if(input$bandSelect == "bw.gwda"){
      formu <- as.formula(paste(input$depbwgda," ~ ",
                                paste(input$indepbwgda,collapse="+")))
    values$bw <-  GWmodel::bw.gwda(formula = formu,
                                   data = datasel(),
                                   COV.gw = input$COVbwgda,
                                   prior.gw = input$priorbwgda,
                                   mean.gw = input$meanbwgda,
                                   prior = NULL,
                                   wqda = input$wqdabwgda,
                                   kernel = input$kernelbwgda,
                                   adaptive = input$adaptativebwgda,
                                   p = input$powerbwgda,
                                   theta = input$thetabwgda*pi,
                                   longlat= input$longlatbwgda,
                                   dMat = values$dMat)}
    else if(input$bandSelect == "bw.gwpca"){
      values$bw <- GWmodel::bw.gwpca(data = datapca(),
                                     vars = colnames(datapca()@data),
                                     k = input$kpca,
                                     robust = input$robustpca,
                                     kernel = input$kernelpca,
                                     adaptive = input$adaptativepca,
                                     p = input$powerpca,
                                     theta = input$thetapca*pi,
                                     longlat= input$longlatpca,
                                     dMat = values$dMat)}
    }
    else if(input$example== TRUE){

      if(input$bandSelect == "bw.gwr"){
        formu <- as.formula(paste(input$dependientgwr," ~ ",
                                  paste(input$independientgwr,collapse="+")))
      values$bw <-  GWmodel::bw.gwr(formula = formu,
                                    data = USelect2004,
                                    approach = input$approachgwr,
                                    kernel = input$kernelgwr,
                                    adaptive = input$adaptativegwr,
                                    p = input$powergwr,
                                    theta = input$thetagwr*pi,
                                    longlat= input$longlatgwr,
                                    dMat = values$dMat)}

      else if(input$bandSelect=="bw.ggwr"){
        formu <- as.formula(paste(input$dependientggwr," ~ ",
                                  paste(input$independientggwr,collapse="+")))
        values$bw <-  GWmodel::bw.ggwr(formula = formu,
                                       data = USelect2004,
                                       family = input$familyggwr,
                                       approach = input$approachggwr,
                                       kernel = input$kernelggwr,
                                       adaptive = input$adaptativeggwr,
                                       p = input$powerggwr,
                                       theta = input$thetaggwr*pi,
                                       longlat= input$longlatggwr,
                                       dMat = values$dMat)}
      else if(input$bandSelect == "bw.gwda"){
        formu <- as.formula(paste(input$depbwgda," ~ ",
                                  paste(input$indepbwgda,collapse="+")))
      values$bw <-  GWmodel::bw.gwda(formula = formu,
                                     data = USelect2004,
                                     COV.gw = input$COVbwgda,
                                     prior.gw = input$priorbwgda,
                                     mean.gw = input$meanbwgda,
                                     prior = NULL,
                                     wqda = input$wqdabwgda,
                                     kernel = input$kernelbwgda,
                                     adaptive = input$adaptativebwgda,
                                     p = input$powerbwgda,
                                     theta = input$thetabwgda*pi,
                                     longlat= input$longlatbwgda,
                                     dMat = values$dMat)}
      else if(input$bandSelect == "bw.gwpca"){
        values$bw <- GWmodel::bw.gwpca(data = USelect2004,
                                       vars = input$varpca,
                                       k = input$kpca,
                                       robust = input$robustpca,
                                       kernel = input$kernelpca,
                                       adaptive = input$adaptativepca,
                                       p = input$powerpca,
                                       theta = input$thetapca*pi,
                                       longlat= input$longlatpca,
                                       dMat = values$dMat)}
    }

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })


  output$selbw <- renderPrint({
    req(values$bw)
    values$bw
  })
  #--------------# gwss #---------------#
  observe({
    if(input$example == TRUE){
    updateSelectizeInput(session,
                         'vargwss', 'Variables',
                         choices = USelect2004@data%>%
                           dplyr::select_if(is.numeric) %>% names,
                         #multiple = TRUE,
                         server = TRUE)}
    })

  observe({
    req(z$dat)
    ovars <- names(z$dat)
    updateSelectizeInput(session,
                         'vargwss', 'Variables',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    if (isTRUE(is.null(input$vargwss))) {shinyjs::disable("Rungwss") }
    else {shinyjs::enable("Rungwss")}
  })

  observeEvent(input$Rungwss, {
    req(input$vargwss)
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "blue",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example == FALSE){
    if (input$selbw == "Manual"){
      values$gwss <- gwss(data = datasel(),
                                   vars = input$vargwss,
                                   kernel = input$kernelgwss,
                                   adaptive = input$adaptativegwss,
                                   p = input$powergwss,
                                   theta = input$thetagwss*pi,
                                   longlat= input$longlatgwss,
                                   bw = input$bwgwss,
                                   quantile = input$quantilegwss,
                                   dMat = values$dMat)}
    else {values$gwss <- gwss(data = datasel(),
                                       vars = input$vargwss,
                                       kernel = input$kernelgwss,
                                       adaptive = input$adaptativegwss,
                                       p = input$powergwss,
                                       theta = input$thetagwss*pi,
                                       longlat= input$longlatgwss,
                                       bw = values$bw,
                                       quantile = input$quantilegwss,
                                       dMat = values$dMat)}
      }
else if (input$example == TRUE){
  if (input$selbw == "Manual"){
    values$gwss <- gwss(data = USelect2004,
                        vars = input$vargwss,
                        kernel = input$kernelgwss,
                        adaptive = input$adaptativegwss,
                        p = input$powergwss,
                        theta = input$thetagwss*pi,
                        longlat= input$longlatgwss,
                        bw = input$bwgwss,
                        quantile = input$quantilegwss,
                        dMat = values$dMat)}
  else {values$gwss <- gwss(data = USelect2004,
                            vars = input$vargwss,
                            kernel = input$kernelgwss,
                            adaptive = input$adaptativegwss,
                            p = input$powergwss,
                            theta = input$thetagwss*pi,
                            longlat= input$longlatgwss,
                            bw = values$bw,
                            quantile = input$quantilegwss,
                            dMat = values$dMat)}
  }

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$gwss <- renderPrint({
    req(values$gwss)
    values$gwss
  })

  output$DFgwss <-  DT::renderDT({
    req(values$gwss)
    datgwss <- data.frame(values$gwss$SDF)%>%
      dplyr::mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwss, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  observe({
    req(values$gwss)
    vargwss <- names(data.frame(values$gwss$SDF))
    updateSelectInput(session,'plotgwss',
                      'Select', choices = vargwss)
  })

  observeEvent(input$Runplotgwss, {
    req(values$gwss$SDF)
    if(input$example == FALSE){
      polys <- list("sp.lines", as(z$map, "SpatialLines"),
                    col="lightgrey", lwd=.5,lty=0.1)
      col.palette <- cartography::carto.pal(input$colorgwss, 20)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwss,input$latgwss),
                    scale = input$scalegwss, col=1)
      values$plotgwss <- sp::spplot(values$gwss$SDF,input$plotgwss,
                               main = input$maingwss,
                               sp.layout=list(polys,map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotgwss}
    else if (input$example == TRUE){
      col.palette <- cartography::carto.pal(input$colorgwss,20)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwss,input$latgwss),
                    scale = input$scalegwss, col=1)
      values$plotgwss <- sp::spplot(values$gwss$SDF,input$plotgwss,
                               main = input$maingwss,
                               sp.layout=list(map.na),
                               scales = list(cex = 1, col="black"),
                               col = "transparent",
                               col.regions = col.palette)
      values$plotgwss
    }

  })


  output$mapgwss <- renderPlot({
    values$plotgwss
  },height = 700, width = 700)

  ########download plot

  # output$down <-downloadHandler()
  observe({
    if (isTRUE(is.null(values$plotgwss))) {
      shinyjs::disable("downgwss")
    }
    else {shinyjs::enable("downgwss")
    }
  })

output$downgwss <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdowngwss, sep=".")
    },

    content = function(file) {
      if(input$butdowngwss == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotgwss) # draw the plot
      dev.off()  # turn the device off

    }
  )

#-----------------------------gwr Collin---------------------------------------
observe({
  if(input$example == TRUE){
    updateSelectizeInput(session,
                         'dependientbgwrd', 'Dependent',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE
    )}
})
observe({
  if(input$example == TRUE){
    updateSelectizeInput(session,
                         'independientbgwrd', 'Independient(s)',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
})

observe({
  req(z$dat)
  ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
  updateSelectizeInput(session,
                       'dependientbgwrd', 'Dependent',
                       choices = ovars,
                       #multiple = TRUE,
                       server = TRUE
  )
})

observe({
  req(z$dat)
  ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
  updateSelectizeInput(session,
                       'independientbgwrd', 'Independient(s)',
                       choices = ovars,
                       #multiple = TRUE,
                       server = TRUE)
})


observe({
  if (isTRUE(is.null(input$independientbgwrd))) {
    shinyjs::disable("Runbgwrd")
  }
  else {shinyjs::enable("Runbgwrd")
  }
})

observeEvent(input$Runbgwrd, {
  req(values$dMat)
  req(input$independientbgwrd)
  shinybusy::show_modal_spinner(
    spin = "atom",
    color = "red",
    text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
  )
  if(input$example == FALSE){
    formu <- as.formula(paste(input$dependientbgwrd,"~",
                              paste(input$independientbgwrd,collapse="+")))
    if( input$selbgwrd == "Automatic"){
      values$gwr.collin <- GWmodel::gwr.collin.diagno(formula = formu,
                                                      data = datasel(),
                                                      bw = values$bw,
                                                      kernel = input$kernelbgwrd,
                                                      adaptive = input$adaptativebgwrd,
                                                      p = input$powerbgwrd,
                                                      theta = input$thetabgwrd*pi,
                                                      longlat = input$longlatbgwrd,
                                                      dMat = values$dMat
                                                      # parallel.method = "cluster"
      )
    }
    else { values$gwr.collin <- GWmodel::gwr.collin.diagno(
      formula = formu,data = datasel(),
      bw = input$bwbgwrd,
      kernel = input$kernelbgwrd,
      adaptive = input$adaptativebgwrd,
      p = input$powerbgwrd,
      theta = input$thetabgwrd*pi,
      longlat = input$longlatbgwrd,
      dMat = values$dMat
      # parallel.method = "cluster"
    )
    }}
  else if(input$example == TRUE){
    formu <- as.formula(paste(input$dependientbgwrd,"~",
                              paste(input$independientbgwrd,collapse="+")))
    if( input$selbgwrd == "Automatic"){
      values$gwr.collin <- GWmodel::gwr.collin.diagno(formula = formu,
                                                      data = USelect2004,
                                                      bw = values$bw,
                                                      kernel = input$kernelbgwrd,
                                                      adaptive = input$adaptativebgwrd,
                                                      p = input$powerbgwrd,
                                                      theta = input$thetabgwrd*pi,
                                                      longlat = input$longlatbgwrd,
                                                      dMat = values$dMat
                                                      # parallel.method = "cluster"
      )
    }
    else if (input$selbgwrd == "Manual"){
      values$gwr.collin <- GWmodel::gwr.collin.diagno(
      formula = formu,
      data = USelect2004,
      bw = input$bwbgwrd,
      kernel = input$kernelbgwrd,
      adaptive = input$adaptativebgwrd,
      p = input$powerbgwrd,
      theta = input$thetabgwrd*pi,
      longlat = input$longlatbgwrd,
      dMat = values$dMat
      )}
    }

  shinyWidgets::closeSweetAlert(session = session)
  shinyWidgets::sendSweetAlert(
    session = session,
    title =" Calculation completed !",
    type = "success"
  )
  beepr::beep(2)
  shinybusy::remove_modal_spinner()
})


#DT::DTOutput("gwrbasicdlocalCN"),
#DT::DTOutput("gwrbasicdlocalVDP"),
#DT::DTOutput("gwrbasicdlocalcorr.mat")

output$gwrbasicdVIF <- DT::renderDT({
  req(values$gwr.collin)

  datgwr <- data.frame(values$gwr.collin$SDF)
  datgwr1 <- datgwr %>% dplyr:: select(!starts_with("Corr_Intercept"))
  #datgwr2 <- datgwr %>% dplyr:: select(ends_with("_VDP") | starts_with("SDF.Corr_"))
  #datgwrd <- merge(datgwr1,datgwr2)
  datgwrd <- datgwr1 %>% mutate_if(is.numeric,round, digits = 6)

  DT::datatable(data =  datgwrd, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               scrollX = TRUE,
                               fixedColumns = TRUE,
                               buttons = c('pageLength',
                                           'copy',
                                           'csv',
                                           'excel',
                                           'pdf',
                                           'print'),
                               pagelength = 10,
                               lengthMenu = list(c(10, 25, 100, -1),
                                                 c('10', '25', '100','All'))))
})


observe({
  req(values$gwr.collin)
  datgwr <- data.frame(values$gwr.collin$SDF)
  datgwr1 <- datgwr %>% dplyr:: select(!starts_with("Corr_Intercept"))
  #datgwr2 <- datgwr %>% dplyr:: select(ends_with("_VDP") | starts_with("SDF.Corr_"))
  #datgwrd <- merge(datgwr1,datgwr2)
  datgwrd <- datgwr1 %>% mutate_if(is.numeric,
              round,
              digits = 6)

  vargwsd <- names(datgwrd)

  updateSelectInput(session,'plotgwrd',
                    'Select', choices = vargwsd)
})

observeEvent(input$Runplotgwrd, {
  if(input$example == FALSE){
    req(values$gwr.collin$SDF)
    polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                  lwd=.5,lty=0.1)
    map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                   offset = c(input$longwrd,input$latgwrd),
                   scale = input$scalegwrd, col=1)
    col.palette <- cartography::carto.pal(input$colorgwrd, 20)
    values$plotgwrd <-sp::spplot(values$gwr.collin$SDF,input$plotgwrd,
                                 main = input$maingwrd,
                                 sp.layout=list(polys,map.na),
                                 scales=list(cex = 1, col="black"),
                                 col="transparent",
                                 col.regions = col.palette)
    values$plotgwrd}
  else if(input$example == TRUE){
    map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                   offset = c(input$longwrd,input$latgwrd),
                   scale = input$scalegwrd, col=1)
    col.palette <- cartography::carto.pal(input$colorgwrd, 20)
    values$plotgwrd <-sp::spplot(values$gwr.collin$SDF,input$plotgwrd,
                                 main = input$maingwrd,
                                 sp.layout=list(map.na),
                                 scales=list(cex = 1, col="black"),
                                 col="transparent",
                                 col.regions = col.palette)
    values$plotgwrd
  }

})

observe({
  if (isTRUE(is.null(values$plotgwrd))) {
    shinyjs::disable("downgwrd")
  }
  else {shinyjs::enable("downgwrd")
  }
})

output$mapgwrbasicd <- renderPlot({
  values$plotgwrd
},height = 700, width = 700)


output$downgwrd <- downloadHandler(
  filename =  function() {
    paste("GeoWeithedModel",input$butdowngwrd, sep=".")
  },

  content = function(file) {
    if(input$butdowngwrd == "png")
      png(file) # open the png device
    else
      pdf(file) # open the pdf device
    print(values$plotgwrd) # draw the plot
    dev.off()  # turn the device off

  }
)



  #-----------------------------gwr.basic---------------------------------------

observe({
 if(input$example == TRUE){
  updateSelectizeInput(session,
                       'dependientbgwr', 'Dependent',
                       choices = USelect2004@data %>%
                         dplyr::select_if(is.numeric)%>% names,
                       #multiple = TRUE,
                       server = TRUE
  )}
  })
observe({
  if(input$example == TRUE){
  updateSelectizeInput(session,
                       'independientbgwr', 'Independient(s)',
                       choices = USelect2004@data %>%
                         dplyr::select_if(is.numeric)%>% names,
                       #multiple = TRUE,
                       server = TRUE)}
})

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientbgwr', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )
  })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'independientbgwr', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    if (isTRUE(is.null(input$independientbgwr))) {
      shinyjs::disable("Runbgwr")
    }
    else {shinyjs::enable("Runbgwr")
    }
  })
  observeEvent(input$Runbgwr, {
    req(values$dMat)
    req(input$independientbgwr)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example == FALSE){
      formu <- as.formula(paste(input$dependientbgwr,"~",
                                paste(input$independientbgwr,collapse="+")))
      if( input$selbgwr == "Automatic"){
      values$gwr.basic <- GWmodel::gwr.basic(formula = formu,
                                             data = datasel(),
                                             bw = values$bw,
                                             kernel = input$kernelbgwr,
                                             adaptive = input$adaptativebgwr,
                                             cv = input$cvbgwr,
                                             p = input$powerbgwr,
                                             theta = input$thetabgwr*pi,
                                             longlat = input$longlatbgwr,
                                             dMat = values$dMat
                                             # parallel.method = "cluster"
      )
    }
    else { values$gwr.basic <- GWmodel::gwr.basic(
      formula = formu,data = datasel(),
      bw = input$bwbgwr,
      kernel = input$kernelbgwr,
      adaptive = input$adaptativebgwr,
      cv = input$cvbgwr,
      p = input$powerbgwr,
      theta = input$thetabgwr*pi,
      longlat = input$longlatbgwr,
      dMat = values$dMat
      # parallel.method = "cluster"
    )
    }}
    else if(input$example == TRUE){
      formu <- as.formula(paste(input$dependientbgwr,"~",
                                paste(input$independientbgwr,collapse="+")))
      if( input$selbgwr == "Automatic"){
        values$gwr.basic <- GWmodel::gwr.basic(formula = formu,
                                               data = USelect2004,
                                               bw = values$bw,
                                               kernel = input$kernelbgwr,
                                               adaptive = input$adaptativebgwr,
                                               cv = input$cvbgwr,
                                               p = input$powerbgwr,
                                               theta = input$thetabgwr*pi,
                                               longlat = input$longlatbgwr,
                                               dMat = values$dMat
                                               # parallel.method = "cluster"
        )
      }
      else { values$gwr.basic <- GWmodel::gwr.basic(
        formula = formu,
        data = USelect2004,
        bw = input$bwbgwr,
        kernel = input$kernelbgwr,
        adaptive = input$adaptativebgwr,
        cv = input$cvbgwr,
        p = input$powerbgwr,
        theta = input$thetabgwr*pi,
        longlat = input$longlatbgwr,
        dMat = values$dMat
        # parallel.method = "cluster"
      )
      }
    }

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$gwrbasic <- renderPrint({
    req(values$gwr.basic)
    values$gwr.basic
  })

  output$SDFgwrbasic <- DT::renderDT({
    #req(values$gwr.basic)
    datgwr <- data.frame(values$gwr.basic$SDF)%>%
      mutate_if(is.numeric,
                round,
                digits = 6)
    DT::datatable(data =  datgwr, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  observe({
    req(values$gwr.basic)
    vargws <- names(data.frame(values$gwr.basic$SDF))

    updateSelectInput(session,'plotgwr',
                      'Select', choices = vargws)
  })


  observeEvent(input$Runplotgwr, {
    if(input$example == FALSE){
      req(values$gwr.basic$SDF)
      polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                    lwd=.5,lty=0.1)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwr,input$latgwr),
                    scale = input$scalegwr, col=1)
      col.palette <- cartography::carto.pal(input$colorgwr, 20)
      values$plotgwr <-sp::spplot(values$gwr.basic$SDF,input$plotgwr,
                              main = input$maingwr,
                              sp.layout=list(polys,map.na),
                              scales=list(cex = 1, col="black"),
                              col="transparent",
                              col.regions = col.palette)
      values$plotgwr}
    else if(input$example == TRUE){
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwr,input$latgwr),
                    scale = input$scalegwr, col=1)
      col.palette <- cartography::carto.pal(input$colorgwr, 20)
      values$plotgwr <-sp::spplot(values$gwr.basic$SDF,input$plotgwr,
                              main = input$maingwr,
                              sp.layout=list(map.na),
                              scales=list(cex = 1, col="black"),
                              col="transparent",
                              col.regions = col.palette)
      values$plotgwr
    }

  })

  observe({
    if (isTRUE(is.null(values$plotgwr))) {
      shinyjs::disable("downgwr")
    }
    else {shinyjs::enable("downgwr")
    }
  })

  output$mapgwrbasic <- renderPlot({
    values$plotgwr
  },height = 700, width = 700)


  output$downgwr <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdowngwr, sep=".")
    },

    content = function(file) {
      if(input$butdowngwr == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotgwr) # draw the plot
      dev.off()  # turn the device off

    }
  )
  #-----------------------------generalized ggwr.basic--------------------------

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientGGWR', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )
  })
  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'independientGGWR', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    if (isTRUE(is.null(input$independientGGWR))) {
      shinyjs::disable("RunGGWR")
    }
    else {shinyjs::enable("RunGGWR")
    }
  })
  observeEvent(input$RunGGWR, {
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    formu <- as.formula(paste(input$dependientGGWR," ~ ",
                              paste(input$independientGGWR,collapse="+")))

    if( input$selGGWR == "Automatic"){
      values$ggwr.basic <- GWmodel::ggwr.basic(formula = formu,
                                               data = datasel(),
                                               bw = values$bw,
                                               kernel = input$kernelGGWR,
                                               adaptive = input$adaptativeGGWR,
                                               cv = input$cvGGWR,
                                               tol = 1e-05,
                                               maxiter = input$maxiterGGWR,
                                               p = input$powerGGWR,
                                               theta = input$thetaGGWR*pi,
                                               longlat = input$longlatGGWR,
                                               dMat = values$dMat
                                               # parallel.method = "cluster"
      )
    }
    else { values$ggwr.basic <- GWmodel::ggwr.basic(
      formula = formu,
      data = datasel(),
      bw = input$bwGGWR,
      family = input$familyGGWR,
      kernel = input$kernelGGWR,
      adaptive = input$adaptativeGGWR,
      cv = input$cvGGWR,
      tol = 1e-05,
      maxiter = input$maxiterGGWR,
      p = input$powerGGWR,
      theta = input$thetaGGWR*pi,
      longlat = input$longlatGGWR,
      dMat = values$dMat
      # parallel.method = "cluster"
    )
    }
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$GGWRbasic <- renderPrint({
    req(values$ggwr.basic)
    values$ggwr.basic
  })
  output$SDFGGWRbasic <- DT::renderDT({
    req(values$ggwr.basic)
    datgwr <- data.frame(values$ggwr.basic$SDF)%>%
      dplyr::mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwr, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  #####plot ggwr.basic
  observe({
    req(values$ggwr.basic)
    vargws <- names(data.frame(values$ggwr.basic$SDF))

    updateSelectInput(session,'plotggwr',
                      'Select', choices = vargws)
  })


  observeEvent(input$Runplotggwr, {
    req(values$ggwr.basic$SDF)
    polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                  lwd=.5,lty=0.1)
    col.palette <- cartography::carto.pal(input$colorggwr, 20)
    map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                  offset = c(input$longgwr,input$latggwr), scale = 1, col=1)
    values$plotggwr <-sp::spplot(values$ggwr.basic$SDF,input$plotggwr,
                             main = input$mainggwr,
                             sp.layout=list(polys,map.na),
                             scales=list(cex = 1, col="black"),
                             col="transparent",
                             col.regions = col.palette)
    values$plotggwr

  })
  output$mapggwr <- renderPlot({
    values$plotggwr
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotggwr))) {
      shinyjs::disable("downggwr")
    }
    else {shinyjs::enable("downggwr")
    }
  })


  output$downggwr <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownggwr, sep=".")
    },
    #
    content = function(file) {
      if(input$butdownggwr == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotggwr) # draw the plot
      dev.off()  # turn the device off

    }
  )

  #-----------------------------gwr.robust--------------------------------------
  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                         'dependientRobust', 'Dependent',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
    })

  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                           'independientRobust', 'Independient(s)',
                           choices = USelect2004@data %>%
                             dplyr::select_if(is.numeric)%>% names,
                           #multiple = TRUE,
                           server = TRUE)
      }
  })
  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientRobust', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )
  })
  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'independientRobust', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    if (isTRUE(is.null(input$independientRobust))) {
      shinyjs::disable("RunRobust")
    }
    else {shinyjs::enable("RunRobust")
    }
  })

  observeEvent(input$RunRobust, {
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if (input$example == FALSE){formu <- as.formula(paste(input$dependientRobust," ~ ",
                              paste(input$independientRobust,collapse="+")))
    if( input$selRobust == "Automatic"){values$gwr.robust <- GWmodel::gwr.robust(formula = formu,
                                                                                 data = datasel(),
                                      bw = values$bw,
                                      kernel = input$kernelRobust,
                                      adaptive = input$adaptativeRobust,
                                      delta  = 1e-05,
                                      filtered = input$filtered,
                                      maxiter = input$maxiterRobust,
                                      #F123.test = input$f123test,
                                      p = input$powerRobust,
                                      theta = input$thetaRobust*pi,
                                      longlat = input$longlatRobust,
                                      dMat = values$dMat)}
    else { values$gwr.robust <- GWmodel::gwr.robust(formula = formu,
                                           data = datasel(),
                                           bw = input$bwRobust,
                                           kernel = input$kernelRobust,
                                           adaptive = input$adaptativeRobust,
                                           #F123.test = input$f123test,
                                           delta  = 1e-05,
                                           filtered = input$filtered,
                                           maxiter = input$maxiterRobust,
                                           p = input$powerRobust,
                                           theta = input$thetaRobust*pi,
                                           longlat = input$longlatRobust,
                                           dMat = values$dMat )}
    }
    else if(input$example == TRUE){
     formu <- as.formula(paste(input$dependientRobust," ~ ",
                               paste(input$independientRobust,collapse="+")))
     if(input$selRobust == "Automatic"){
       values$gwr.robust <- GWmodel::gwr.robust(
         formula = formu,
         data = USelect2004,
         bw = values$bw,
         kernel = input$kernelRobust,
         adaptive = input$adaptativeRobust,
         delta  = 1e-05,
         #F123.test = input$f123test,
         filtered = input$filtered,
         maxiter = input$maxiterRobust,
         p = input$powerRobust,
         theta = input$thetaRobust*pi,
         longlat = input$longlatRobust,
         dMat = values$dMat

        )}
     else { values$gwr.robust <- GWmodel::gwr.robust(
       formula = formu,
       data = USelect2004,
       bw = input$bwRobust,
       kernel = input$kernelRobust,
       adaptive = input$adaptativeRobust,
       delta  = 1e-05,
       filtered = input$filtered,
       maxiter = input$maxiterRobust,
       #F123.test = input$f123test,
       p = input$powerRobust,
       theta = input$thetaRobust*pi,
       longlat = input$longlatRobust,
       dMat = values$dMat
      )}
    }

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$GGWRobust <- renderPrint({
    req(values$gwr.robust)
    values$gwr.robust
  })
  output$SDFGWRRobust <- DT::renderDT({
    req(values$gwr.robust)
    datgwr <- data.frame(values$gwr.robust$SDF)%>%
      mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwr, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  ## plot gwr robust
  observe({
    req(values$gwr.robust)
    vargws <- names(data.frame(values$gwr.robust$SDF))

    updateSelectInput(session,'plotRgwr',
                      'Select', choices = vargws)
  })


  observeEvent(input$RunplotRgwr, {
    if(input$example == FALSE){
    req(values$gwr.robust$SDF)
    polys <- list("sp.lines", as(z$map, "SpatialLines"),
                  col="lightgrey", lwd=.5,lty=0.1)
    col.palette <- cartography::carto.pal(input$colorRgwr, 20)
    map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                  offset = c(input$lonRgwr,input$latRgwr),
                  scale = input$scaleRgwr, col=1)
    values$plotRgwr <-sp::spplot(values$gwr.robust$SDF,input$plotRgwr,
                             main = input$mainRgwr,
                             sp.layout=list(polys, map.na),
                             scales=list(cex = 1, col="black"),
                             col="transparent",
                             col.regions = col.palette)
    values$plotRgwr}
    else if(input$example == TRUE){
      col.palette <- cartography::carto.pal(input$colorRgwr, 20)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonRgwr,input$latRgwr),
                    scale = input$scaleRgwr, col=1)
      values$plotRgwr <-sp::spplot(values$gwr.robust$SDF,input$plotRgwr,
                               main = input$mainRgwr,
                               sp.layout=list(map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotRgwr
    }
    })

  output$mapRgwr <- renderPlot({
    values$plotRgwr
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotRgwr))) {
      shinyjs::disable("downRgwr")
    }
    else {shinyjs::enable("downRgwr")
    }
  })

  output$downRgwr <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownRgwr, sep=".")
    },# content is a function with argument file. content writes the plot to
    #the device
    content = function(file) {
      if(input$butdownRgwr == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotRgwr) # draw the plot
      dev.off()  # turn the device off
      }
  )
  #-----------------------------gwr.hetero--------------------------------------
  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                         'dependienthetero', 'Dependent',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE
    )}
  })
  observe({
    if(input$example == TRUE){
    updateSelectizeInput(session,
                         'independienthetero', 'Independient(s)',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
  })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependienthetero', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )
  })
  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'independienthetero', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    if (isTRUE(is.null(input$independienthetero))) {
      shinyjs::disable("Runhetero")
    }
    else {shinyjs::enable("Runhetero")
    }
  })
  observeEvent(input$Runhetero, {
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example == FALSE){
    formu <- as.formula(paste(input$dependienthetero," ~ ",
                              paste(input$independienthetero,collapse="+")))

    if( input$selhetero == "Automatic"){
      values$gwr.hetero <- GWmodel::gwr.hetero(
        formula = formu,
        data = datasel(),
        bw = values$bw,
        kernel = input$kernelhetero,
        adaptive = input$adaptativehetero,
        tol=0.0001,
        maxiter = input$maxiterhetero,
        p = input$powerhetero,
        theta = input$thetahetero*pi,
        longlat = input$longlathetero,
        dMat = values$dMat

      )
    }
    else { values$gwr.hetero <- GWmodel::gwr.hetero(
      formula = formu,
      data = datasel(),
      bw = input$bwhetero,
      kernel = input$kernelhetero,
      adaptive = input$adaptativehetero,
      tol= 0.0001,
      maxiter = input$maxiterhetero,
      p = input$powerhetero,
      theta = input$thetahetero*pi,
      longlat = input$longlathetero,
      dMat = values$dMat
    )
    }
    }
    else if(input$example == TRUE){
      formu <- as.formula(paste(input$dependienthetero," ~ ",
                                paste(input$independienthetero,collapse="+")))
      if( input$selhetero == "Automatic"){
        values$gwr.hetero <- GWmodel::gwr.hetero(
          formula = formu,
          data = USelect2004,
          bw = values$bw,
          kernel = input$kernelhetero,
          adaptive = input$adaptativehetero,
          tol=0.0001,
          maxiter = input$maxiterhetero,
          p = input$powerhetero,
          theta = input$thetahetero*pi,
          longlat = input$longlathetero,
          dMat = values$dMat

        )
      }
      else { values$gwr.hetero <- GWmodel::gwr.hetero(
        formula = formu,
        data = USelect2004,
        bw = input$bwhetero,
        kernel = input$kernelhetero,
        adaptive = input$adaptativehetero,
        tol= 0.0001,
        maxiter = input$maxiterhetero,
        p = input$powerhetero,
        theta = input$thetahetero*pi,
        longlat = input$longlathetero,
        dMat = values$dMat
      )
      }


    }

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$GWRhetero <- renderPrint({
    req(values$gwr.hetero)
    values$gwr.hetero
  })
  output$SDFGWRhetero <- DT::renderDT({
    req(values$gwr.hetero)
    datgwr <- data.frame(values$gwr.hetero)%>%
      mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwr, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  ## plot gwr hetero
  observe({
    req(values$gwr.hetero)
    vargws <- names(data.frame(values$gwr.hetero))

    updateSelectInput(session,'plotHgwr',
                      'Select', choices = vargws)
  })

  observeEvent(input$RunplotHgwr, {
    if(input$example== FALSE){
    req(values$gwr.hetero)
    polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                  lwd=.5,lty=0.1)
    map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                  offset = c(input$lonHgwr,input$latHgwr), scale = 1, col=1)
    col.palette <- cartography::carto.pal(input$colorHgwr, 20)
    values$plotHgwr <-sp::spplot(values$gwr.hetero,input$plotHgwr,
                             main = input$mainHgwr,
                             sp.layout=list(polys,map.na),
                             scales=list(cex = 1, col="black"),
                             col="transparent",
                             col.regions = col.palette)
    values$plotHgwr}
    else if(input$example == TRUE){
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonHgwr,input$latHgwr),
                    scale = input$scaleHgwr, col=1)
      col.palette <- cartography::carto.pal(input$colorHgwr, 20)
      values$plotHgwr <-sp::spplot(values$gwr.hetero,input$plotHgwr,
                               main = input$mainHgwr,
                               sp.layout=list(map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotHgwr

    }


  })

  output$mapHgwr <- renderPlot({
    values$plotHgwr
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotHgwr))) {
      shinyjs::disable("downHgwr")
    }
    else {shinyjs::enable("downHgwr")
    }
  })
  output$downHgwr <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownHgwr, sep=".")
    },
    content = function(file) {
      if(input$butdownHgwr == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotHgwr) # draw the plot
      dev.off()  # turn the device off

    }
  )

  #-----------------------------gwr.mixed---------------------------------------
  observe({
    if(input$example == TRUE){
    updateSelectizeInput(session,
                         'dependientmixed', 'Dependent',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE
    )}
  })

  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                         'independientmixed', 'Independient(s)',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
  })
  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                         'fixedvars', 'fixed var(s)',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
  })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientmixed', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE

    )
  })
  observe({
    req(z$dat)
    ovars <- names(z$dat)
    updateSelectizeInput(session,
                         'independientmixed', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })

  observe({
    req(z$dat)
    ovars <- c(input$independientmixed)
    updateSelectizeInput(session,
                         'fixedvars', 'fixed var',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE,
                         options = list(maxItems = 1))
  })

  observe({
    if (isTRUE(is.null(input$independientmixed))) {
      shinyjs::disable("Runmixed")
    }
    else {shinyjs::enable("Runmixed")
    }
  })

  observeEvent(input$Runmixed, {
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example == FALSE){
    formu <- as.formula(paste(input$dependientmixed," ~ ",
                              paste(input$independientmixed,collapse="+")))

    if( input$selmixed == "Automatic"){
      values$gwr.mixed <- GWmodel::gwr.mixed(
        formula = formu,
        data = datasel(),
        bw = values$bw,
        fixed.vars = c(input$fixedvars),
        intercept.fixed= input$intercepfixed,
        diagnostic = input$diagnostic,
        kernel = input$kernelmixed,
        adaptive = input$adaptativemixed,
        p = input$powermixed,
        theta = input$thetamixed*pi,
        longlat = input$longlatmixed,
        dMat = values$dMat

      )
    }
    else { values$gwr.mixed <- GWmodel::gwr.mixed(
      formula = formu,
      data = datasel(),
      bw = input$bwmixed,
      fixed.vars = c(input$fixedvars),
      intercept.fixed= input$intercepfixed,
      diagnostic = input$diagnostic,
      kernel = input$kernelmixed,
      adaptive = input$adaptativemixed,
      p = input$powermixed,
      theta = input$thetamixed*pi,
      longlat = input$longlatmixed,
      dMat = values$dMat
    )
    }}
    else if(input$example == TRUE){
      formu <- as.formula(paste(input$dependientmixed," ~ ",
                                paste(input$independientmixed,collapse="+")))

      if( input$selmixed == "Automatic"){
        values$gwr.mixed <- GWmodel::gwr.mixed(
          formula = formu,
          data = USelect2004,
          bw = values$bw,
          fixed.vars = c(input$fixedvars),
          intercept.fixed= input$intercepfixed,
          diagnostic = input$diagnostic,
          kernel = input$kernelmixed,
          adaptive = input$adaptativemixed,
          p = input$powermixed,
          theta = input$thetamixed*pi,
          longlat = input$longlatmixed,
          dMat = values$dMat

        )
      }
      else { values$gwr.mixed <- GWmodel::gwr.mixed(
        formula = formu,
        data = USelect2004,
        bw = input$bwmixed,
        fixed.vars = c(input$fixedvars),
        intercept.fixed= input$intercepfixed,
        diagnostic = input$diagnostic,
        kernel = input$kernelmixed,
        adaptive = input$adaptativemixed,
        p = input$powermixed,
        theta = input$thetamixed*pi,
        longlat = input$longlatmixed,
        dMat = values$dMat
      )
      }}
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$GWRmixed <- renderPrint({
    req(values$gwr.mixed)
    values$gwr.mixed
  })
  output$SDFGWRmixed <- DT::renderDT({
    req(values$gwr.mixed)
    datgwr <- data.frame(values$gwr.mixed$SDF)%>%
      mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwr, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  ## plot gwr mixed
  observe({
    req(values$gwr.mixed)
    vargws <- names(data.frame(values$gwr.mixed$SDF))

    updateSelectInput(session,'plotMgwr',
                      'Select', choices = vargws)
  })

  observeEvent(input$RunplotMgwr, {
    if (input$example == FALSE){
      req(values$gwr.mixed$SDF)
      polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                    lwd=.5,lty=0.1)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonMgwr,input$latMgwr), scale = 1, col=1)
      col.palette <- cartography::carto.pal(input$colorMgwr, 20)
      values$plotMgwr <-sp::spplot(values$gwr.mixed$SDF,input$plotMgwr,
                               main = input$mainMgwr,
                               sp.layout=list(polys,map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotMgwr}
    else if(input$example == TRUE){
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonMgwr,input$latMgwr),
                    scale = input$scaleMgwr, col=1)
      col.palette <- cartography::carto.pal(input$colorMgwr, 20)
      values$plotMgwr <-sp::spplot(values$gwr.mixed$SDF,input$plotMgwr,
                               main = input$mainMgwr,
                               sp.layout=list(map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotMgwr
    }


  })

  output$mapMgwr <- renderPlot({
    values$plotMgwr
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotMgwr))) {
      shinyjs::disable("downMgwr")
    }
    else {shinyjs::enable("downMgwr")
    }
  })
  output$downMgwr <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownMgwr, sep=".")
    },

    content = function(file) {
      if(input$butdownMgwr == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotMgwr) # draw the plot
      dev.off()  # turn the device off
    }
  )

  #-----------------------------gwr.scalable------------------------------------
  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                         'dependientscal', 'Dependent',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE
    )}
  })

  observe({
    if(input$example == TRUE){
    updateSelectizeInput(session,
                         'independientscal', 'Independient(s)',
                         choices = USelect2004@data %>%
                           dplyr::select_if(is.numeric)%>% names,
                         #multiple = TRUE,
                         server = TRUE)}
    })

  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'dependientscal', 'Dependent',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE
    )
  })
  observe({
    req(z$dat)
    ovars <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'independientscal', 'Independient(s)',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
  })


  observe({
    if (isTRUE(is.null(input$independientscal))) {
      shinyjs::disable("Runscal")
    }
    else {shinyjs::enable("Runscal")
    }
  })
  observeEvent(input$Runscal, {
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example == FALSE){
      formu <- as.formula(
        paste(input$dependientscal,
              " ~ ",paste(input$independientscal,collapse="+")))
      values$gwr.scalable <- GWmodel::gwr.scalable(formula = formu,
                                                 data = datasel(),
                                                 bw.adapt = input$bwscal,
                                                 polynomial = input$polinom,
                                                 kernel = input$kernelscal,
                                                 p = input$powerscal,
                                                 theta = input$thetascal*pi,
                                                 longlat = input$longlatscal,
                                                 dMat = values$dMat
                                                 # parallel.method = "cluster"
      )}
    else if (input$example == TRUE){

      formu <- as.formula(paste(input$dependientscal," ~ ",
                                paste(input$independientscal,collapse="+")))
      values$gwr.scalable <- GWmodel::gwr.scalable(formula = formu,
                                                   data = USelect2004,
                                                   bw.adapt = input$bwscal,
                                                   polynomial = input$polinom,
                                                   kernel = input$kernelscal,
                                                   p = input$powerscal,
                                                   theta = input$thetascal*pi,
                                                   longlat = input$longlatscal,
                                                   dMat = values$dMat
                                                   # parallel.method = "cluster"
      )
    }
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })

  output$GWRscalable <- renderPrint({
    req(values$gwr.scalable)
    values$gwr.scalable
  })
  output$SDFGWRscalable <- DT::renderDT({
    req(values$gwr.scalable)
    datgwr <- data.frame(values$gwr.scalable$SDF)%>%
      mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwr, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })
  ## plot gwr scala
  observe({
    req(values$gwr.scalable)
    vargws <- names(data.frame(values$gwr.scalable$SDF))

    updateSelectInput(session,'plotSgwr',
                      'Select', choices = vargws)
  })

  observeEvent(input$RunplotSgwr, {
    if(input$example == FALSE){
      req(values$gwr.scalable$SDF)
      polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                    lwd=.5,lty=0.1)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonSgwr,input$latSgwr),
                    scale = input$scaleSgwr, col=1)
      col.palette <- cartography::carto.pal(input$colorSgwr, 20)
      values$plotSgwr <-sp::spplot(values$gwr.scalable$SDF,
                               input$plotSgwr, main = input$mainSgwr,
                               sp.layout=list(polys,map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotSgwr}
    else if(input$example == TRUE){
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonSgwr,input$latSgwr),
                    scale = input$scaleSgwr, col=1)
      col.palette <- cartography::carto.pal(input$colorSgwr, 20)
      values$plotSgwr <-sp::spplot(values$gwr.scalable$SDF,
                               input$plotSgwr, main = input$mainSgwr,
                               sp.layout=list(map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotSgwr
    }


  })
  output$mapSgwr <- renderPlot({
    values$plotSgwr
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotSgwr))) {
      shinyjs::disable("downSgwr")
    }
    else {shinyjs::enable("downSgwr")
    }
  })
  output$downSgwr <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownSgwr, sep=".")
    },
# is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$butdownSgwr == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotSgwr) # draw the plot
      dev.off()  # turn the device off
    }
  )
  #------------------------------------gwpca------------------------------------
  observe({
    if(input$example == TRUE){
      updateSelectizeInput(session,
                           'vargwpca', 'Variables',
                           choices = USelect2004@data%>%
                             dplyr::select_if(is.numeric)
                           %>% names,
                         #multiple = TRUE,
                         server = TRUE)}
    })

  observe({
    req(z$dat)
    ovarspca <- z$dat %>% dplyr::select_if(is.numeric) %>% names
    updateSelectizeInput(session,
                         'vargwpca', 'Variables',
                         choices = ovarspca,
                         #multiple = TRUE,
                         server = TRUE
    )
  })
  observe({
  updateNumericInput(session,'kgwpca',
                       'k',
                       2,
                       min = 1,
                       max = length(input$vargwpca))
  })

  datagwpca <- reactive({
    if (input$example == FALSE){
    mf <- datasel()[, input$vargwpca]
    data.scaled <- scale(as.matrix(mf@data[, input$vargwpca]))
    coords <- sp::coordinates(datasel())
    datagwpca <- SpatialPointsDataFrame(coords, as.data.frame(data.scaled ))
    datagwpca}
  })
  #datagwpca <- reactive({
   # if(input$example== TRUE){
     # mf <- USelect2004[, input$vargwpca]
     # data.scaled <- scale(as.matrix(mf@data[, input$vargwpca]))
     # coords <- sp::coordinates(USelect2004)
    #datagwpca <- SpatialPointsDataFrame(coords, as.data.frame(data.scaled ))
   # datagwpca}
 # })

  observe({
    if (isTRUE(is.null(input$vargwpca))) {
      shinyjs::disable("Rungwpca")
    }
    else {shinyjs::enable("Rungwpca")
    }
  })
  observeEvent(input$Rungwpca, {
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "#428ded",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )
    if(input$example == FALSE){

      if( input$selgwpca == "Automatic"){
      values$gwpca <- GWmodel::gwpca(data = datagwpca(),
                                     vars = colnames(datagwpca()@data),
                                     k = input$kgwpca,
                                     robust = input$robustgwpca,
                                     kernel = input$kernelgwpca,
                                     adaptive = input$adaptativegwpca,
                                     bw = values$bw,
                                     p = input$powergwpca,
                                     theta = input$thetagwpca*pi,
                                     longlat= input$longlatgwpca,
                                     dMat = values$dMat)
    }
    else { values$gwpca <- GWmodel::gwpca(data = datagwpca(),
                                          vars = colnames(datagwpca()@data),
                                          k = input$kgwpca,
                                          robust = input$robustgwpca,
                                          kernel = input$kernelgwpca,
                                          adaptive = input$adaptativegwpca,
                                          bw = input$bwgwpca,
                                          p = input$powergwpca,
                                          theta = input$thetagwpca*pi,
                                          longlat= input$longlatgwpca,
                                          dMat = values$dMat)
    }}
    else if(input$example == TRUE){

      if( input$selgwpca == "Automatic"){
        values$gwpca <- GWmodel::gwpca(data = USelect2004,
                                       vars = input$vargwpca,
                                       k = input$kgwpca,
                                       robust = input$robustgwpca,
                                       kernel = input$kernelgwpca,
                                       adaptive = input$adaptativegwpca,
                                       bw = values$bw,
                                       p = input$powergwpca,
                                       theta = input$thetagwpca*pi,
                                       longlat= input$longlatgwpca,
                                       dMat = values$dMat)
      }
      else { values$gwpca <- GWmodel::gwpca(data = USelect2004,
                                            vars = input$vargwpca,
                                            k = input$kgwpca,
                                            robust = input$robustgwpca,
                                            kernel = input$kernelgwpca,
                                            adaptive = input$adaptativegwpca,
                                            bw = input$bwgwpca,
                                            p = input$powergwpca,
                                            theta = input$thetagwpca*pi,
                                            longlat= input$longlatgwpca,
                                            dMat = values$dMat)
      }

    }
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )
    shinybusy::remove_modal_spinner()
  })
  output$DFgwpca <- DT::renderDT({
    req(values$gwpca)
    values$gwpca$SDF
    datgwpca <- data.frame(values$gwpca$SDF)%>%
      dplyr::mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwpca, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  output$gwpca <- renderPrint({
    req(values$gwpca)
    values$gwpca
  })
  observe({
    updateNumericInput(session,'loading',
                       'Component',
                       1,
                       min = 1,
                       max = input$kgwpca)
  })

  output$loadinggwpca <- DT::renderDT({
    req(values$gwpca)
    loadinggwpca <- data.frame(values$gwpca$loadings[, , input$loading])%>%
      mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  loadinggwpca, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  observe({
    updateNumericInput(session,'loadingVgwpca',
                       'Number of component',
                       2,
                       min = 2,
                       max = input$kgwpca)
  })



  observeEvent(input$RunplotVgwr, {
    if(input$example == FALSE){
      req(values$gwpca)
      var.gwpca <- prop.var(values$gwpca, input$loadingVgwpca)
      mf <- datasel()[, input$vargwpca]
      mf$var.gwpca <- var.gwpca
      polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                    lwd=.5,lty=0.1)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwpca,input$latgwpca),
                    scale = input$scalegwpca, col=1)
      col.palette <- cartography::carto.pal(input$colorVgwpca, 20)
      values$plotVgwpca <- sp::spplot(mf,"var.gwpca", main = input$mainVgwpca,
                                 sp.layout=list(polys,map.na),
                                 scales=list(cex = 1, col="black"),
                                 col="transparent",
                                 col.regions = col.palette)
      values$plotVgwpca}
    else if(input$example == TRUE){
      req(values$gwpca)
      var.gwpca <- prop.var(values$gwpca, input$loadingVgwpca)
      mf <- USelect2004[, input$vargwpca]
      mf$var.gwpca <- var.gwpca
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwpca,input$latgwpca),
                    scale = input$scalegwpca, col=1)
      col.palette <- cartography::carto.pal(input$colorVgwpca, 20)
      values$plotVgwpca <- sp::spplot(mf,"var.gwpca", main = input$mainVgwpca,
                                 sp.layout=list(map.na),
                                 scales=list(cex = 1, col="black"),
                                 col="transparent",
                                 col.regions = col.palette)
      values$plotVgwpca
    }




  })
  output$Vargwpca <- renderPlot({
    values$plotVgwpca
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotVgwpca))) {
      shinyjs::disable("downVgwpca")
    }
    else {shinyjs::enable("downVgwpca")
    }
  })

  output$downVgwpca <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownVgwpca, sep=".")
    },
    # is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$butdownVgwpca == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotVgwpca) # draw the plot
      dev.off()  # turn the device off

    }
  )

  observe({
    updateNumericInput(session,'loadingwingwpca',
                       'Component',
                       1,
                       min = 1,
                       max = input$kgwpca)
  })

  observeEvent(input$Runplotwingwpca, {
    if(input$example== FALSE){
      mf <- datasel()[, input$vargwpca]
      loadings.pc1 <- values$gwpca$loadings[, , input$loadingwingwpca]
      win.item <- max.col(abs(loadings.pc1))
      mf$win.item <- win.item
      polys <- list("sp.lines", as(z$map, "SpatialLines"), col="lightgrey",
                    lwd=.5,lty=0.1)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonwingwpca,input$latwingwpca),
                    scale =input$scalewingwpca, col=1)
      mypalette.4 <- cartography::carto.pal("multi.pal",
                                            n1 = length(input$vargwpca))
      values$plotwingwpca <- sp::spplot(mf,"win.item",
                                        main = input$mainwingwpca,
                                   sp.layout=list(polys,map.na),
                                   scales=list(cex = 1, col="black"),
                                   #legendEntries = c(input$vargwpca),
                                   col="transparent",
                                   col.regions = mypalette.4,
                                   at = seq(from = 1,
                                            to = length(input$vargwpca)+1,
                                            by =1 ))
      values$plotwingwpca}
    else if(input$example == TRUE){
      mf <- USelect2004[, input$vargwpca]
      loadings.pc1 <- values$gwpca$loadings[, , input$loadingwingwpca]
      win.item <- max.col(abs(loadings.pc1))
      mf$win.item <- win.item
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$lonwingwpca,input$latwingwpca),
                    scale = input$scalewingwpca, col=1)
      mypalette.4 <- cartography::carto.pal("multi.pal",
                                            n1 = length(input$vargwpca))
      values$plotwingwpca <- sp:: spplot(mf,"win.item", main = input$mainwingwpca,
                                   sp.layout=list(map.na),
                                   scales=list(cex = 1, col="black"),
                                   #legendEntries = c(input$vargwpca),
                                   col="transparent",
                                   col.regions = mypalette.4,
                                   at = seq(from = 1,
                                            to = length(input$vargwpca)+1,
                                            by =1 ))
      values$plotwingwpca
    }



  })

  output$wingwpca <- renderPlot({
    values$plotwingwpca
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotwingwpca))) {
      shinyjs::disable("downwingwpca")
    }
    else {shinyjs::enable("downwingwpca")
    }
  })
  output$downwingwpca <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownwingwpca, sep=".")
    },
    content = function(file) {
      if(input$butdownwingwpca == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotwingwpca) # draw the plot
      dev.off()  # turn the device off

    }
  )

  #------------------------------------gwda------------------------------------
    observe({
      #req(z$dat)
        #if (input$example == FALSE | is.null(z$dat)){return()}
       if (input$example == TRUE){
      updateSelectizeInput(session,
                           'dependgwda', 'Grouping factor',
                          choices = USelect2004@data %>% dplyr::select_if(is.factor)%>%
                            names,
                           #multiple = TRUE,
                           server = TRUE,
                           options = list(maxItems = 1)
                          )}
      })

  observe({
    req(z$dat)
    #if (input$example == FALSE | is.null(z$dat)){return()}
      updateSelectizeInput(session,
                           'dependgwda', 'Grouping factor',
                           choices = z$dat %>% dplyr::select_if(is.factor)%>% names,
                           #multiple = TRUE,
                           server = TRUE,
                           options = list(maxItems = 1))
      })
  observe({
    req(z$dat)
   ovars <- z$dat %>% dplyr::select_if(is.numeric)%>% names
   updateSelectizeInput(session,
                         'indepentgwda', 'Discriminators',
                         choices = ovars,
                         #multiple = TRUE,
                         server = TRUE)
   })
  observe({
   #req(z$dat)
   if (input$example == TRUE ){
     updateSelectizeInput(session,
                          'indepentgwda', 'Discriminators',
                          choices =  USelect2004@data %>%
                            dplyr::select_if(is.numeric)%>% names,
                          #multiple = TRUE,
                          server = TRUE)
     }
    })
  observe({
    if (isTRUE(is.null(input$indepentgwda))) {
      shinyjs::disable("Rungwda")
      }
    else {shinyjs::enable("Rungwda")
    }
    })

  observeEvent(input$Rungwda,{
    req(values$dMat)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "red",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while")

    if( input$example == TRUE ){
      formu <- as.formula(paste(input$dependgwda," ~ ",
                                paste(input$indepentgwda,collapse="+")))
       if(input$selgwda == "Automatic"){
        values$gwda <-  gwda(formula = formu,
                             data = USelect2004,
                             COV.gw = input$COVgwda,
                             predict.data = USelect2004,
                             prior.gw = input$priorgwda,
                             mean.gw = input$meangwda,
                             bw = values$bw,
                             prior = NULL,
                             wqda = input$wqdagwda,
                             kernel = input$kernelgwda,
                             adaptive = input$adaptativegwda,
                             p = input$powergwda,
                             theta = input$thetagwda*pi,
                             longlat= input$longlatgwda,
                             dMat = values$dMat)
      }
      else if(input$selgwda == "Manual"){
        values$gwda <-  gwda(formula = formu,
                             data = USelect2004,
                             COV.gw = input$COVgwda,
                             predict.data = USelect2004,
                             prior.gw = input$priorgwda,
                             mean.gw = input$meangwda,
                             bw = input$bwgwda,
                             prior = NULL,
                             wqda = input$wqdagwda,
                             kernel = input$kernelgwda,
                             adaptive = input$adaptativegwda,
                             p = input$powergwda,
                             theta = input$thetagwda*pi,
                             longlat= input$longlatgwda,
                             dMat = values$dMat)
      }

    }
    else if (input$example == FALSE) {
      formu <- as.formula(paste(input$dependgwda," ~ ",
                                paste(input$indepentgwda,collapse="+")))
      if(input$selgwda == "Manual"){
        values$gwda <-  gwda(formula = formu,
                             data = datasel(),
                             COV.gw = input$COVgwda,
                             predict.data = datasel(),
                             prior.gw = input$priorgwda,
                             mean.gw = input$meangwda,
                             bw = input$bwgwda,
                             prior = NULL,
                             wqda = input$wqdagwda,
                             kernel = input$kernelgwda,
                             adaptive = input$adaptativegwda,
                             p = input$powergwda,
                             theta = input$thetagwda*pi,
                             longlat= input$longlatgwda,
                             dMat = values$dMat)
      }
      else if(input$selgwda == "Automatic"){
        values$gwda <-  gwda(formula = formu,
                             data = datasel(),
                             COV.gw = input$COVgwda,
                             predict.data = datasel(),
                             prior.gw = input$priorgwda,
                             mean.gw = input$meangwda,
                             bw = values$bw,
                             prior = NULL,
                             wqda = input$wqdagwda,
                             kernel = input$kernelgwda,
                             adaptive = input$adaptativegwda,
                             p = input$powergwda,
                             theta = input$thetagwda*pi,
                             longlat= input$longlatgwda,
                             dMat = values$dMat)
      }

    }

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success")
    beepr::beep(2)
    shinybusy::remove_modal_spinner()
  })


  output$gwda <- renderPrint({
    req(values$gwda)
    values$gwda
    })

  output$DFgwda <-  DT::renderDT({
    req(values$gwda$SDF)
    datgwda <- data.frame(values$gwda$SDF)%>%
      mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data =  datgwda, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

  output$predict <-  renderPrint({
    req(values$gwda$SDF)
    table(USelect2004$winner,values$gwda$SDF$group.predicted)
  })

  # plot gwda
    observe({
    req(values$gwda$SDF)
    vargws <- names(data.frame(values$gwda$SDF))

    updateSelectInput(session,'plotgwda',
                      'Select', choices = c("Original",
                                            "group.predicted",vargws))
    })

  observeEvent(input$Runplotgwda, {

    req(values$gwda$SDF)
    if(input$example == TRUE){
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwda,input$latgwda),
                    scale = input$scalegwda, col=1)
      col.palette <- cartography::carto.pal(input$colorgwda, 20)

      if(input$plotgwda == "Original"){
        USelect2004$winner <- factor(USelect2004$winner)
        values$plotgwda <- sp::spplot(USelect2004,"winner",
                               main = input$maingwda,
                               sp.layout=list(map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotgwda}
      else if(input$plotgwda == "group.predicted"){
        data <- values$gwda$SDF
        data$group.predicted <- factor(data$group.predicted)
        values$plotgwda <- sp::spplot(data,"group.predicted",
                                      main = input$maingwda,
                                      sp.layout=list(map.na),
                                      scales=list(cex = 1, col="black"),
                                      col="transparent",
                                      col.regions = col.palette)
        values$plotgwda}
      else {
        values$plotgwda <- sp::spplot(values$gwda$SDF,input$plotgwda,
                                          main = input$maingwda,
                                          sp.layout=list(map.na),
                                          scales=list(cex = 1, col="black"),
                                          col="transparent",
                                          col.regions = col.palette)
      values$plotgwda
     }

    }
    else if(input$example == FALSE){
      polys <- list("sp.lines", as(z$map, "SpatialLines"),
                    col="lightgrey", lwd=.5,lty=0.1)
      map.na <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(input$longwda,input$latgwda),
                    scale = input$scalegwda, col=1)
      col.palette <- cartography::carto.pal(input$colorgwda, 20)
      values$plotgwda <- sp::spplot(values$gwda$SDF,input$plotgwda,
                               main = input$maingwda,
                               sp.layout=list(polys,map.na),
                               scales=list(cex = 1, col="black"),
                               col="transparent",
                               col.regions = col.palette)
      values$plotgwda}
  })

  output$mapgwda <- renderPlot({
    values$plotgwda
  },height = 700, width = 700)

  observe({
    if (isTRUE(is.null(values$plotgwda))) {
      shinyjs::disable("downgwda")
    }
    else {shinyjs::enable("downgwda")
    }
  })
  output$downgwda <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdowngwda, sep=".")
    },

    content = function(file) {
      if(input$butdowngwda == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotgwda) # draw the plot
      dev.off()  # turn the device off
    }
  )

 #------------------------Autocorrelation----------------------

  observe({
    req(z$dat)
    ovars <- names(z$dat)
    updatePickerInput(session,
                      'varauto',
                      choices = ovars)
    })

  observeEvent(input$Runauto, {
    req(datasel())
    datauto <- datasel()
    Variable <- pull(dplyr::select(z$dat,input$varauto))
    print(input$varauto)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "blue",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while")
    neighbourhood <- spdep::poly2nb(datauto, queen= TRUE)
    neighbourhood_weights_list <- spdep::nb2listw(neighbourhood,
                                                  style=input$style,
                                                  zero.policy= TRUE)

    values$morantest <- spdep::moran.test(Variable,
                                          neighbourhood_weights_list,
                                          alternative = input$alternative)
    values$moranmc <-  spdep::moran.mc(Variable,
                                       neighbourhood_weights_list,
                                       alternative= input$alternative,
                                       nsim= input$numsim)
    values$localmoran <- spdep::localmoran(Variable,
                                           neighbourhood_weights_list,
                                           p.adjust.method="bonferroni",
                                           alternative = input$alternative,
                                           na.action=na.exclude,
                                           zero.policy = TRUE)
    values$localmoranperm <- spdep::localmoran_perm(Variable,
                                                    neighbourhood_weights_list,
                                                    p.adjust.method="bonferroni",
                                                    na.action=na.exclude,
                                                    nsim= input$numsim,
                                                    alternative = input$alternative,
                                                    zero.policy = TRUE)
    shinybusy::remove_modal_spinner()
    beepr:: beep(2)
  })

  output$variablename<- renderPrint({
    req(input$varauto)
    print(input$varauto)
  })

  output$moran <- renderPrint({
    req(values$morantest)
    values$morantest
  })

  output$moranmc <- renderPrint({
    req(values$moranmc)
      values$moranmc
  })

  output$localmoran <- renderPrint({
    req(values$localmoran)
    summary(values$localmoran)
  })

  output$localmoranperm <- renderPrint({
    req(values$localmoranperm)
    summary(values$localmoranperm)
  })

  output$DFauto <-  DT::renderDT({
    req(values$localmoran)
    datmat <- data.frame(values$localmoran[,1:5])%>%
      dplyr::mutate_if(is.numeric, round, digits = 6)
    DT::datatable(data = datmat, extensions = 'Buttons',
                  colnames = c("Ii","E.Ii","Var.Ii","Z.Ii","Pr.Z"),
                options = list(dom = 'Bfrtip', scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1),
                                                   c('10', '25', '100','All'))))
  })

observeEvent(input$Runplotauto, {
  req(values$localmoran)
  SPDF <- datasel()
  SPDF@data$lmoran_i <- values$localmoran[,1]
  SPDF@data$lmoran_p <- values$localmoran[,5]
  polys <- list("sp.lines", as(z$map,
                               "SpatialLines"),
                col="lightgrey", lwd=.5,lty=0.1)
  col.palette <- cartography::carto.pal(input$colorauto, 20)
  map.na <- list("SpatialPolygonsRescale",
                 layout.north.arrow(),
                 offset = c(input$longauto,input$latauto),
                 scale = input$scaleauto, col=1)
  if(input$plotauto == "lmoran_i"){
    values$plotauto <- sp::spplot(SPDF, "lmoran_i",
                                  main = "Local Moran's I",
                                  sp.layout=list(polys,map.na),
                                  scales=list(cex = 1, col="black"),
                                  col="transparent",
                                  col.regions = col.palette)
    }
  else if(input$plotauto == "lmoran_p"){
    values$plotauto <- sp::spplot(SPDF, "lmoran_p",
                                  main = "P-values",
                                  sp.layout=list(polys,map.na),
                                  scales=list(cex = 1, col="black"),
                                  col="transparent",
                                  col.regions = col.palette)
    }
  })



  output$mapauto <- renderPlot({
    req(values$plotauto)
    values$plotauto
  },height = 700, width = 700)


  observe({
    if (isTRUE(is.null(values$plotauto))) {
      shinyjs::disable("downgauto")
    }
    else {shinyjs::enable("downgauto")
    }
  })

  output$downgauto <- downloadHandler(
    filename =  function() {
      paste("GeoWeithedModel",input$butdownauto, sep=".")
    },

    content = function(file) {
      if(input$butdownauto == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotauto) # draw the plot
      dev.off()  # turn the device off

    }
  )


  }) #FIN


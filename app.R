library(shiny)
library(DT)
library(ggplot2)
library(png)
library(grid)
library(gridExtra)

# if pngs are not all stored in the same place, use datadir and pngbasedir to locate
datadir = '.'
pngbasedir = file.path(datadir,"pngs/")

#  load dataset.RDS, at minimum needs col ptid
dataset <- read.csv(file.path(datadir,"appdata.csv"))

reviewlabs = c("Single image unusable" = "badImage",
               "Study Artifacted" = "hasArtifact",
               "Bias field" = "hasBias",
               "Registration fail" = "badReg",
               "Registration poor" = "poorReg",
               "Brain mask fail" = "badMask",
               "Brain mask poor" = "poorMask",
               "Segmentation fail" = "badTumor",
               "Segmentation poor" = "poorTumor",
               "Normalization fail" = "badNormalize",
               "No pngs" = "badPNG",
               "Needs review" = "needsManual")

logdata_init <- data.frame(ptid = unique(dataset$ptid),
                           reviewed = FALSE,
                           comment="",
                           stringsAsFactors = FALSE)
logdata_init[,unname(reviewlabs)] <- FALSE
ui <- fluidPage(
  title = "MRDQED",
  sidebarLayout(position="left", fluid = TRUE,
                sidebarPanel(width=2,
                             wellPanel(
                               h3("Data display"),
                               selectInput("ptid", NULL, unique(dataset$ptid)),
                               radioButtons("pictype","Review:",
                                            choices = c("Registrations"="Reg","Maxes"="Max"),
                                            inline=TRUE),
                               checkboxGroupInput("tabvars", "Variables to show:",
                                                  choices = head(setdiff(names(dataset),c("ptid","T1","T2","FLAIR","T1CE","MASK","SEG","ROI2")), 10),
                                                  selected = NULL,
                                                  inline = TRUE),
                               h3("Data review"),
                               checkboxGroupInput("reviewvars", "Data properties",
                                                  choices=reviewlabs,
                                                  inline=TRUE),
                               textInput("reviewcomment","Comment:"),
                               actionButton("savereview","Save review + next case"),
                               downloadButton("downloadData", "Download review log"),
                               fileInput("logcsv", "Load previous review log", 
                                         multiple = FALSE,
                                         buttonLabel = "Browse...",
                                         placeholder = "No file selected",
                                         c("text/csv",".csv"))
                             )
                ),
                mainPanel(width=10,
                          fluidPage(
                            fluidRow(
                              column(4,
                                     fluidRow(
                                       DT::dataTableOutput("table1")),
                                     fluidRow(
                                       plotOutput("png2",width="100%",height="500px"))
                              ),
                              column(8,
                                     plotOutput("png3",width="100%",height="900px"))
                              )
                          )
                )
                
  )
)

server <- function(input, output, session) {
  # assign good/needs review
  logdata <- reactiveVal(value = logdata_init)
  pngdir <- reactiveVal()
  display_images <- reactiveVal()
  
  update_reviewlog <- function(data, input, new_reviewvars=FALSE, new_comment=FALSE){
    logidx = head(which(logdata()$ptid==input$ptid),n=1)
    newlog = logdata()
    if(new_comment) newlog[logidx, "comment"] = input$reviewcomment
    if(new_reviewvars){
      print(input$reviewvars)
      newlog[logidx, input$reviewvars] = TRUE
      newlog[logidx, setdiff(reviewlabs, input$reviewvars)] = FALSE}
    newlog[logidx, "reviewed"] = TRUE
    logdata(newlog)
  }
  
  # for returning to an already-viewed case
  update_reviewdisplay <- function(data, input){
    logidx = head(which(data$ptid==input$ptid), n=1)
    updateTextInput(session,"reviewcomment",
                    value = data$comment[logidx])
    updateCheckboxGroupInput(session, "reviewvars",
                             selected = unname(reviewlabs)[as.logical(data[logidx,unname(reviewlabs)])] )
  }
  
  observeEvent({input$reviewvars}, {update_reviewlog(logdata(), input, new_reviewvars=TRUE)}, ignoreNULL = FALSE)
  observeEvent({input$reviewcomment}, {update_reviewlog(logdata(), input, new_comment=TRUE)})
  
  observeEvent(input$savereview, {
    # print review to app console just in case
    print(logdata()[ptid==input$ptid,])
    # manually set reviewed flag for current case
    update_reviewlog(logdata(),input)
    currrow = match(input$ptid,dataset$ptid)
    # don't advance if at the end of the data set
    if((currrow + 1) <= nrow(dataset)){
      updateSelectInput(session, "ptid", selected=dataset$ptid[currrow+1])
      update_reviewdisplay(logdata(),input)}
  })
  observeEvent(input$logcsv, {
    if(file.exists(input$logcsv$datapath)){
      print(paste("Loading log file",input$logcsv$datapath))
      logdata(read_csv(input$logcsv$datapath,col_types = cols(.default = "?", comment="c")))
      update_reviewdisplay(logdata(),input)
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() { ifelse(is.null(input$logcsv),"dataQA_report.csv",input$logcsv$name) },
    content = function(file) { write_csv(logdata(), path = file) }
  ) 
  
  # Get png image name prefix (force individual folders)
  observeEvent(input$ptid, {
    # TODO: option to make png directories case specific
    # pngdir(file.path(pngbasedir,input$ptid))
    pngdir(file.path(pngbasedir))
    # change row selection if ptid selection box is used
    # TODO: test this
    selectPage(dataTableProxy('table1'), 
      which(dataset[input$table1_rows_all,"ptid",drop=TRUE] == input$ptid) %/% 
                   input$table1_state$length + ifelse(input$table1_state$length==1,0,1)
      )
    # update review status and comments
    update_reviewdisplay(logdata(),input)
    # TODO: change to reactiveVal possibly and fix png paths.
    display_images(sapply(c("T1","T2","T1CE","FLAIR","FUNC"), function(x) {
      if(x %in% names(dataset)) {file.path(pngdir(),paste(input$ptid,x,sep="_"))
      } else {NA_character_}}))
  })

  # change ptid selection box if row clicked
  observeEvent(input$table1_rows_selected, {
    updateSelectInput(session, "ptid",
                      selected=dataset[input$table1_rows_selected,'ptid'])
  })
  
  # output
  output$table1 = DT::renderDataTable(DT::datatable(dataset[,c("ptid",input$tabvars),drop=F],
                                                    selection = "single",
                                                    options = list(
                                                      lengthMenu = list(c(1, 5, 10, -1),
                                                                        c('1', '5', '10')),
                                                      pageLength = 10,
                                                      stateSave = TRUE),
                                                    rownames= FALSE))
  # https://stackoverflow.com/a/53391222
  output$png2 = renderPlot({
    png_image_types = c("axial1","sagittal1","coronal1","axial2","sagittal2","coronal2",
                        "axial3","sagittal3","coronal3")
    pngnames=file.path(pngdir(),paste0(input$ptid, "_T1CE_",png_image_types,"maskshot.png"))
    # https://stackoverflow.com/a/45474093
    col.titles = c("MASK", "SEG", "ROI2")
    
    pngs = lapply(pngnames, function(x) if(file.exists(x)) readPNG(x) else array(0,dim = c(512,512,3)))
    asGrobs = lapply(pngs, rasterGrob)
    
    nr = 3
    p <- grid.arrange(grobs=lapply(seq(1,length(asGrobs),by = nr), function(i) {
      arrangeGrob(grobs=asGrobs[i:(i+nr-1)], top=col.titles[i/nr + 1], nrow=nr)
    }), ncol=length(pngnames) %/% nr)
  })
  output$png3 = renderPlot({
    png_image_types <- list(Reg=c("_axial1maskshot","_sagittal1maskshot","_axial2maskshot","_density"),
                            Max=c("_SEG_axi","_SEG_sag","_SEG_cor","_density"))
    pngnames=file.path(c(paste0(display_images()[["T2"]], png_image_types[[input$pictype]],".png"),
                         paste0(display_images()[["FLAIR"]], png_image_types[[input$pictype]],".png"),
                         paste0(display_images()[["T1CE"]], png_image_types[[input$pictype]],".png"),
                         paste0(display_images()[["T1"]], png_image_types[[input$pictype]],".png"),
                         paste0(display_images()[["FUNC"]], png_image_types$Max,".png"))
    )
    # https://stackoverflow.com/a/45474093
    col.titles = c(paste(input$pictype,c("T2", "FLAIR", "T1CE", "T1")),"FUNC")
    nr = 4
    pngs = lapply(pngnames, function(x) if(file.exists(x)) readPNG(x) else array(0, dim = c(512,512,3)))
    asGrobs = lapply(pngs,rasterGrob)
    p <- grid.arrange(grobs=lapply(seq(1,length(asGrobs),by = nr), function(i) {
      arrangeGrob(grobs=asGrobs[i:(i+nr-1)], top=col.titles[i/nr + 1], nrow=nr)
    }), ncol=length(pngnames) %/% nr)
  })
}
shinyApp(ui,server)

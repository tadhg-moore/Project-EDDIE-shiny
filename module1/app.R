# 06-navlist.R

library(shiny)
library(glmtools) 
library(GLMr)
library(httr) # A package necessary for the GRAPLEr to work		
library(RCurl)
library(jsonlite) # A package necessary for the GRAPLEr to work		
library(GRAPLEr)

# private function
find_glm_params	<-	function(nml) {
  
  # test for argName being a string
  # if (!is.character(argName)){stop(c("parameter name must be a string"))}
  fau <- " "
  fault.string <- rep(fau,1000) # names fault matrix, only returned when empty match
  blockNames	<-	names(nml)
  blckI	<-	c()
  for (i in seq_len(length(blockNames))){
    # if (any(argName %in% names(nml[[i]]))){
    #   blckI	<- c(blckI,i)
    # } else {
      one.i <- which(fault.string==fau)[1]
      fault.string[one.i:(one.i+length(names(nml[[i]]))-1)]=names(nml[[i]])
    # }
    
  }
  fault.string <- fault.string[!fault.string==fau] # is empty if found
  # test to see if a block match was made
  # if (is.null(blckI)){stop(c("parameter name ",argName," not found in nml. Possible names:",paste(fault.string,collapse=', ')))}
  return(paste(fault.string,collapse=', '))
}

nml_file <- "data/glm2.nml"
nml <- read_nml(nml_file) 
meteo_file <- "data/met_hourly.csv"
met <- read.csv(meteo_file)
baseline <- "data/output.nc"

glm_pars <- find_glm_params(nml)
glm_pars <- strsplit(glm_pars, ", ")[[1]]

sim_mets <- sim_metrics()[c(3, 5)]

# GrapleR
dirs <- list.files(full.names = TRUE)
MyExpRootDir <- (file.path("data", 'MyExpRoot')) # Met files and nml are here
MyResultsDir <- (file.path("data", 'MyResults')) # Outputs will go here		
MyExpRootDir <-  gsub("\\\\", "/", MyExpRootDir)		
MyResultsDir <-  gsub("\\\\", "/", MyResultsDir)

dir.create(MyResultsDir)  # Create the MyResults directory in your computer
MyExpRootDir <- normalizePath(file.path("data", 'MyExpRoot')) # Met files and nml are here
MyResultsDir <- normalizePath(file.path("data", 'MyResults')) # Outputs will go here	
MyExpRootDir <-  gsub("\\\\", "/", MyExpRootDir)		
MyResultsDir <-  gsub("\\\\", "/", MyResultsDir)

graplerURL <- "https://graple.acis.ufl.edu"  # Specify web address
MyExp <- new("Graple", GWSURL=graplerURL, ExpRootDir=MyExpRootDir, ResultsDir=MyResultsDir,		
             ExpName="EDDIE", TempDir = tempdir())	# Set up your GRAPLEr experiment




ui <- navbarPage(title = "Module 1: Climate Change Effects on Lake Temperatures", 
                 position = "fixed-top", 
  tabPanel(title = "Introduction",
           tags$style(type="text/css", "body {padding-top: 65px;}"),
           img(src = "eddie_banner_2018.v5.jpg", height = 100, width = 1544, top = 5),
           sidebarLayout(
             
             sidebarPanel(
               h1("Module Description"),
               br(),
               h4("
Climate change is modifying the thermal structure of lakes around the globe.  In this module, students will learn how to use a lake model to explore the effects of altered weather on lakes, and then develop their own climate scenarios to test hypotheses about how lakes may change in the future.  Once the students have mastered running one climate scenario for their lake, they will learn how to use distributed computing software to scale up and run hundreds of different climate scenarios for their lakes.  The overarching goal of this module is for students to explore new modeling and computing tools while learning fundamental concepts about how climate change will affect lakes.
")
             ),
             
             mainPanel(
               img(src = "lake-ice.jpg"),
               br(),
               img(src = "lake.jpg")
             )
           )
           
           ),
  tabPanel(title = "Activity A",
           img(src = "eddie_banner_2018.v5.jpg", height = 100, width = 1544, top = 5),
    h1("General Lake Model"),
    actionButton("glmver", "Check GLM version"),
    # h2("Check which version you are running..."),
    wellPanel(textOutput("glmv")),
    h2("View 'glm2.nml' file:"),
    actionButton("nml_view", "View"),
    textOutput("nml_out"),
    selectInput("glm_par", label = "Select GLM parameter", choices = glm_pars,
                selected = glm_pars[54]),
    wellPanel(textOutput("glm_par_out")),
    h3("Plot meteorology data"),
    actionButton("plot_met_but", "Plot met data"),
    plotOutput("plot_met"),
    
    h2("Objective 3"),
    actionButton("run_glm", "Run GLM!"),
    verbatimTextOutput("glm_out"),
    
    actionButton("surftemp1", "Save surface temperature"),
    
    
    h3("Plot GLM output"),
    actionButton("plot_glm_but", "Plot GLM output"),
    plotOutput("plot_glm"),
    
    h2("Objective 4"),
    h3("Compare the model to observed data"),
    actionButton("mod_comp", "Compare model to obs"),
    plotOutput("plot_comp"),
    
    h3("Compare other output variables"),
    selectInput("sim_met", label = "Select metric for comparison",
                choices = sim_mets),
    plotOutput("plot_sim")
    
    
    
  ),
  # Activity B ui ----
  tabPanel(title = "Activity B",
           img(src = "eddie_banner_2018.v5.jpg", height = 100, width = 1544, top = 5),
           sidebarLayout(
             
             sidebarPanel(
               h3("Download meteorological file"),
               downloadButton("download_met", "Download")
             ),
             
             mainPanel(
               h1("Climate Change Scenarios"),
               br(),
               h4(" Using your knowledge of potential climate change, work with a partner to
 develop a climate change scenario for your model lake.

 To complete this activity, you will need to modify the input meterological
 data and then run the model to examine the effects of your scenario on the
 thermal structure of the lake.

Here is an overview of the steps you will complete with your partner to
 accomplish this:",
                  br(),
                  br(),
 "
1) Develop a climate scenario (it can be for any region!)", 
br(),
"
2) Create a corresponding meteorological input (met) file. See below for
 detailed directions.", 
br(),
"3) Test your hypotheses! Run the GLM using your new met file and examine how
 it changes lake temperatures.", 
br(),
"4) Compare the modeled output to the observed. What are the implications of
 your climate scenario for future water quality and quantity?", 
br(),
"5) Create and save a few figures to highlight the results of your climate
 scenario and present them to the rest of the class. It would be helpful to
 present both the meteorological input file as well as the lake thermal plots
 so that we can see how the lake responded to your climate forcing.
 ", 
br(),
br(),
"
Detailed directions for modifying your met file:", 
br(),
"SOMETHING THAT IS REALLY REALLY IMPORTANT!
 Opening up the met_hourly.csv file in Microsoft Excel will alter the date/time
 formatting of the file so that GLM cannot read it. You will get an error
 something like this: 'Day 2451545 (2000-01-01) not found'. To avoid this
 error, you need to follow the FIVE steps listed below when changing your met file.
", 
br(),
br(),
"
FIRST, copy and paste an extra version of the met_hourly.csv file in your
sim folder so that you have a backup in case of any mistakes. Rename this
file something like 'met_hourly_UNALTERED.csv' and be sure *not* to open it.
", 
br(),
"
SECOND, open the met_hourly.csv file in Excel.  Manipulate the different input
variables to create your climate/weather scenario (be creative!).
NOTE ABOUT UNITS: In the met_hourly file, the units for rain are in meters
per day. You will likely think about the amount of rain your change in the
met_hourly file by millimeters per day instead-- to convert from mm/d to m/d,
simply multiply by 0.001.
", 
br(),
br(),
"
NOTE ABOUT COLUMN NAMES: the order of the columns in the met file does not
matter- but you can only have one of each variable and they must keep the
EXACT same name (e.g., it must always be 'AirTemp', not 'AirTemp+3oC').
", 
br(),
br(),
"
FORMAT THE TIME COLUMN BEFORE YOU SAVE YOUR NEW FILE!:
When you are done editing the meteorological file, highlight all of the
'time' column in Excel, then click on 'Format Cells', and then 'Custom'.
In the 'Type' or 'Formatting' box, change the default to YYYY-MM-DD hh:mm:ss
exactly. This is the only time/date format that GLM is able to read. When
you click ok, this should change the format of the 'time' column so that it
reads: '1999-12-31 00:00:00' with exactly that spacing and
punctuation. Save this new file under a different name, following how you
have created your scenario, e.g., 'met_hourly_SUMMERSTORMS.csv'.
Close the csv file, saving your changes. Now, do NOT open the file in Excel
again- otherwise, you will need to repeat this formatting process before
reading the altered met file into GLM.
"),
#### Upload file ----
fileInput("up_met", "Upload altered meteo file", accept = ".csv"),
#### Update GLM ----
h3("Update GLM nml file"),
actionButton("upd_nml", "Update nml"),
verbatimTextOutput("nml_upd"),
# h2("View 'glm2.nml' file:"),
# actionButton("nml_view", "View"),
# textOutput("nml_out"),
# selectInput("glm_par", label = "Select GLM parameter", choices = glm_pars,
#             selected = glm_pars[54]),
# wellPanel(textOutput("glm_par_out")),

h3("Run GLM with the new updated met file"),
actionButton("run_glm2", "Run GLM!"),
verbatimTextOutput("glm_out2"),

h3("Plot GLM output"),
actionButton("plot_glm_but2", "Plot GLM output"),
plotOutput("plot_glm2")



      
             )
           )
    ), 

# Activity C ----
  tabPanel(title = "Activity C",
           img(src = "eddie_banner_2018.v5.jpg", height = 100, width = 1544, top = 5),
           h1("GRAPLEr!"),
           br(), br(),
          h3("The GRAPLEr is an R package that allows you to set up hundreds of GLM
 simulations with varying input meteorological data and run those simulations
 efficiently and quickly using distributed computing. The model 'jobs' are
 submitted via a web service to run	on computers elsewhere, allowing you to
 rapidly set up and run hundreds of simulations, access the output, and analyze
 the data."),
          h3("Check GRAPLer files"),
          actionButton("chk_fils", "Check GRAPLEr files"),
          # h2("Check which version you are running..."),
          wellPanel(textOutput("grap_fils")),
          
          h3("View job file"),
          actionButton("v_job", "View job json file"),
          wellPanel(textOutput("view_job")),
          
          h3("Set up GRAPLEr"),
          actionButton("grap_set", "Setup GRAPLEr"),
          wellPanel(textOutput("grap_setup")),
          
          # Start Graple run
          h3("Start GRAPLE run"),
          actionButton("grap_r", "Run GRAPLEr"),
          wellPanel(textOutput("grap_run")),
          
          # Check Graple run
          h3("Check GRAPLE status"),
          actionButton("chk_grap", "Check status", icon = icon("refresh")),
          wellPanel(textOutput("check_grap")),
          
          # Check Graple run
          h3("Download GRAPLE results"),
          actionButton("grap_dl", "Download results"),
          wellPanel(textOutput("grap_downl")),
          
          # Check Graple files
          h3("Check download files"),
          actionButton("grfils", "List files download"),
          wellPanel(textOutput("gfils")),
          
          # Check Graple files
          h3("Plot min scenario"),
          actionButton("plot_min", "Plot scenarios"),
          wellPanel(plotOutput("min_plot")),
          
          h3("Plot max scenario"),
          actionButton("plot_max", "Plot scenarios"),
          wellPanel(plotOutput("max_plot"))
          
  )
)

server <- function(input, output) {
  
  output$glmv <- eventReactive(input$glmver, {
    txt <- capture.output(glm_version())
    if(txt == "[1] 0") {
      return("General Lake Model (GLM) - Version 2.2.0rc5")
    }
  })
  
  output$nml_out <- eventReactive(input$nml_view, {
    txt2 <- capture.output(print(nml))
    return(txt2)
  })
  
  output$glm_par_out <- renderText({
    nml <- read_nml(nml_file)
    get_nml_value(nml, input$glm_par)
  })
  
  observeEvent(input$plot_met_but, {
    output$plot_met <- renderPlot({
      plot_meteo(nml_file)
    })
  })
  
  output$glm_out <- eventReactive(input$run_glm, {
    start <- Sys.time()
    txt2 <- capture.output(run_glm(sim_folder = "data"))
    stop <- Sys.time()
    if(txt2 == "[1] 0") {
      return(paste0("       ------------------------------------------------\n
       |  General Lake Model (GLM)   Version 2.2.0rc5    |\n
       ------------------------------------------------\n
nDays 1701744 timestep 3600.000000\n
Maximum lake depth is 50.600000\n
Wall clock start time : ", start,"
Simulation begins...
Wall clock finish time : ", stop,"
Wall clock runtime 1 seconds : 00:00:01 [hh:mm:ss]"))
    } else {
      "Error running GLM, check the meteo file and re-upload"
    }
  })
  
  observeEvent("surftemp1", {
    output$stemp1 <- renderDataTable({
      get_var(file=baseline, "temp", reference='surface', z_out=c(1))
    })
  })
  
  output$plot_test <- renderPlot({
    plot(stemp1()[,1], stemp1()[,2], type = 'l')
  })
  
  
  observeEvent(input$plot_glm_but, {
    output$plot_glm <- renderPlot({
      plot_temp(baseline)
    })
    # output$surf_temp  <- get_var(file=baseline, "temp", reference='surface', z_out=c(1)) 
  })
  
  observeEvent(input$mod_comp, {
    output$plot_comp <- renderPlot({
      baseline <- file.path("data", "output.nc")
      field_file <- file.path("data", 'field_data.csv') 
      plot_var_compare(nc_file = baseline, field_file, var_name = "temp")
      })
  })
  
  output$plot_sim <- renderPlot({
    baseline <- file.path("data", "output.nc")
    field_file <- file.path("data", 'field_data.csv') 
    sim_vals <- compare_to_field(baseline, field_file, metric=input$sim_met,
                                 as_value=TRUE, na.rm=TRUE)
    
    if(input$sim_met == "thermo.depth") {
      ylabs="Thermocline depth in meters"
      ylims=c(0,32)
      type = "l"
    }
    if(input$sim_met == "water.temperature") {
      ylabs="Water Temperature in degrees C"
      ylims=c(0,32)
      type = "p"
    }
    plot(sim_vals$DateTime, sim_vals$obs, type=type, col="blue", ylim=ylims,
         ylab=ylabs, xlab="Date")
    if(input$sim_met == "thermo.depth") {
      lines(sim_vals$DateTime, sim_vals$mod, col="red")
    } else {
      points(sim_vals$DateTime, sim_vals$mod, col="red")
    }
    legend("topright",c("Observed", "Modeled Baseline"),lty=c(1,1), col=c("blue", "red"))
  })
  
  # Activty B ----
  output$download_met <- downloadHandler(
    filename = "met_hourly.csv",
    content = function(file) {
      write.csv(met, file, row.names = F, quote = F)
    }
  )
  
  output$nml_upd <- eventReactive(input$upd_nml, {
    # print(input$up_met)
    nml <- set_nml(nml, "meteo_fl", input$up_met$name)
    write_nml(nml, file.path("data", "glm2.nml"))
    met2 <- read.csv(input$up_met$datapath)
    write.csv(met2, file.path("data", input$up_met$name), row.names = FALSE,
              quote = FALSE)
    return("'glm2.nml' updated!")
  })
  
  # Re-run GLM
  output$glm_out2 <- eventReactive(input$run_glm2, {
    start <- Sys.time()
    txt2 <- capture.output(run_glm(sim_folder = "data"))
    stop <- Sys.time()
    if(txt2 == "[1] 0") {
      return(paste0("       ------------------------------------------------\n
       |  General Lake Model (GLM)   Version 2.2.0rc5    |\n
       ------------------------------------------------\n
nDays 1701744 timestep 3600.000000\n
Maximum lake depth is 50.600000\n
Wall clock start time : ", start,"
Simulation begins...
Wall clock finish time : ", stop,"
Wall clock runtime 1 seconds : 00:00:01 [hh:mm:ss]"))
    } else {
      "Error running GLM, check the meteo file and re-upload"
    }
  })
  
  observeEvent(input$plot_glm_but2, {
    output$plot_glm2 <- renderPlot({
      plot_temp(baseline)
    })
    # output$surf_temp  <- get_var(file=baseline, "temp", reference='surface', z_out=c(1)) 
  })
  
  # Activity C ----
  
  output$grap_fils <- eventReactive(input$chk_fils, {
    return(list.files(MyExpRootDir) )
  })
  
  output$view_job <- eventReactive(input$v_job, {
    job_desc <- file.path(MyExpRootDir, 'job_desc.json') # Locate the job_desc.json file		
    txt3 <- capture.output(cat( readLines( job_desc ) , sep = "\n" ))
    return(txt3)   
  })
  
  observeEvent(input$grap_set, {
    output$grap_setup <- renderText({
      MyExp <<- GrapleCheckService(MyExp)
      return(paste(MyExp@StatusMsg, input$grap_set))
    })
  })
  
  
  # output$MyExp <- eventReactive(input$grap_set, {
  #   return(GrapleCheckService(MyExp))
  # })
  
  output$grap_run <- eventReactive(input$grap_r, {
    MyExp <<- GrapleRunSweepExperiment(MyExp)
    # output$MyExp <- MyExp
    return(MyExp@StatusMsg)
  })
  
  observeEvent(input$chk_grap, {
    output$check_grap <- renderText({
      MyExp <<- GrapleCheckExperimentCompletion(MyExp)
      return(paste(MyExp@StatusMsg, Sys.time()))
    })
  })
  
  output$grap_downl <- eventReactive(input$grap_dl, {
    unlink(file.path("data", "MyResults"), recursive = TRUE, force = TRUE)
    dir.create(file.path("data", "MyResults"), showWarnings = FALSE)
    MyExp <<- GrapleGetExperimentResults(MyExp)
    return(MyExp@StatusMsg)
  })
  
  output$gfils <- eventReactive(input$grfils, {
    fils <- list.dirs(MyResultsDir, recursive = TRUE)	
    # fils <- basename(fils)
    return(fils)
  })
  
  observeEvent(input$plot_min, {
    output$min_plot <- renderPlot({
      fils <- list.dirs(MyResultsDir, recursive = TRUE)	
      sim_folder_1 <- file.path(MyResultsDir, 'EDDIE', 'Sims', 'Sim1_1', 'Results')		
      nc_file_1 <- file.path(sim_folder_1, 'output.nc')		
      plot_temp( file=nc_file_1, fig_path=FALSE)	
    })
  })
  
  observeEvent(input$plot_max, {
    output$max_plot <- renderPlot({
      fils <- list.dirs(MyResultsDir, recursive = TRUE)	
      sim_folder_101 <- file.path(MyResultsDir, 'EDDIE', 'Sims', 'Sim21_1', 'Results')
      nc_file_101 <- file.path(sim_folder_101, 'output.nc')		
      plot_temp(file=nc_file_101, fig_path=FALSE)
    })
  })


}

shinyApp(server = server, ui = ui)
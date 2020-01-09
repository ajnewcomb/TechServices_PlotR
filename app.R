#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
### Title: "Tech Services - PlotR v1.4"
### Author: Andrew Newcomb
### Contact: andrew.j.newcomb@maine.gov
### Last updated: 06 January 2020
#
#
#

library(shiny)
library(tidyverse)
library(Cairo)
library(grid)
library(openxlsx)

###### Define UI for application -------------------------------------------------------
ui = fluidPage(
    
    # Application title
    titlePanel("Tech Services PlotR v1.4"),
    h4("Author: Andrew Newcomb"),
    h4("Contact: andrew.j.newcomb@maine.gov"),
    h4("Last Updated: 06 January 2020"),
    br(),
    ### Side Panel -------------------------------------------------------------------------
    sidebarPanel(
        h2("Control Box"),
        p("Use this tool to tidy data, convert units, and to create Stiff-Diagram, Radar, and Time-Series plots of water chemistry data at individual sample point locations. 
            Start by uploading your EGAD export file which must be a text (.txt) file. When you export this from EGAD, do so as a 'Common Geology File'. 
            Use EGAD's Graphing and Data Extract Module to select any sample point locations, parameters, and sample dates you might be interested in. 
            Be careful that your exported text file doesn't exceed the max input file size of 30mb."),
        p("Once your data is uploaded (Step 1) more of the options in this Control Box will become available. A plot of all the data in the categories you select (Step 2)
           will show up to the right. Use the four basic steps to enter / change the content of your Stiff-Diagrams, Radar Plots, or Time-Series. These plots will show up below the larger plot,
           in their respective tabs."),
        p("Your data will also be available to explore in tables within each tab along the top of this web page, and these can all be dowloaded as an excel workbook using the button below."),
        p("You can also use the Unit Converter tool below to standardize units for each parameter category and to convert ion concentrations to equivalents. This will update all plots and tables other than the original EGAD export table."),
        #br(),
        
        h3("Step 1: Upload Data"),
        p("Remember, this must be a text file (i.e. 'file_name.txt') and be a 'Common Geology File' format exported from EGAD."),
        fileInput(inputId = "file1",
                  label   = "Enter the your EGAD export file here:",
                  accept = c('text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain')), #end fileInput
        
        h3("Step 2: Select Parameter Categories"),
        uiOutput(outputId = "param.category.selector"),
        p("Note: Too many categories will look sloppy with all those variables. Also some may have different units, so make sure you are comparing apples with apples."),
        
        h4("Step 2.5: Select Date Range"),
        uiOutput(outputId = "date.selector"),
        
        h3("Step 3: Select Plot Parameters"),
        p("Note: Too many can crash the app and if they don't it will still look bad."),
        uiOutput(outputId = "param.selector.left"),
        uiOutput(outputId = "param.selector.right"),
        wellPanel(id = "s3-Panel", style = "overflow-y:scroll; max-height: 200px",
            uiOutput(outputId = "rad.params")),
        
        h3("Step 4: Select Sample Points"),
        wellPanel(id = "s4-Panel", style = "overflow-y:scroll; max-height: 200px",
            uiOutput(outputId = "sample.point.selector")),
        br(),
        
        ### Unit Converter ---------------------------------------------------------------------------------------------------------------------------------
        h3("Unit Converter"),
        p("This will update all tables other than the original EGAD data, and their corresponding plots."),
        p("Enter the parameter categories you want to convert, and then the units you want those converted to."),
        p("If you want multiple conversions done enter them such that the second 'from' corresponds to the second 'to'. Later conversions will overwrite previous ones. Conversions always begin with the original data, and may always be reverted to original units/values."),
        selectInput(inputId = 'from.units',
                    label   = 'Convert values within these categories:',
                    choices = c('Everything', 'METAL', 'ANION & CATION', 'VOC', 'SVOC', 'PFC', 'NUTRIENT', 'INDICATOR'),
                    multiple = T,
                    selected = 'Everything'), # end selectInput
        selectInput(inputId = 'to.units',
                    label   = 'To these units:',
                    choices = c('Original Units','MG/L', 'UG/L', 'NG/L', 'MEQ/L', 'UEQ/L', 'NEQ/L'),
                    multiple = T,
                    selected = 'Original Units'),
        actionButton(inputId = 'convert.button',
                     label   = 'Click here to convert:'),
        p('Note: Only "ANION & CATION" can be converted to equivalents, and of these only BROMIDE, CALCIUM, CHLORIDE, FLUORIDE, MAGNESIUM, POTASSIUM, SODIUM, SULFATE, SULFIDE, TOTAL ALKALINITY, and TOTAL ORGANIC CARBON are supported.'),
        p("Note: Only results with the following units can be converted: 'MG/L', 'UG/L', 'NG/L', 'MEQ/L', 'UEQ/L', 'NEQ/L'. Results with units in 'MG/KG' or 'ppm' will not be converted."),
        br(),
        
        h3("Download Data Tables"),
        downloadButton(outputId = 'download.tables', label = "Download all tables as xlsx"),
        
        ### Plot appearance ----------------------------------------------------------------------------------------------------------------------------------
        h3("Extra Plot Options"),
        p('If the plots do not look quite right then play with these text size options, or try zooming in/out in your web browser. 
           80% zoom usually looks best.'),
        numericInput(inputId  = "plot.text.size",
                     label    = 'Text size for Stiff Plot title and axis labels:',
                     value    = 14),
        numericInput(inputId  = "plot.label.size",
                     label    = 'Text size for Radar Plot labels and Stiff Plot data point labels:',
                     value    = 6),
        #checkboxInput(inputId = 'static.axis',
        #              label   = 'Check if you want all your plots to have the same scale',
        #              value   = FALSE),
        radioButtons(inputId  = 'axis.scale',
                     label    = 'Select if you want different or similar scales for your plots',
                     choices  = c('Unique',
                                  'Same for each sample location',
                                  #'Same for each sample date',
                                  'Same for all sample locations and dates')
                     ), # end radioButtons
        radioButtons(inputId = 'theme',
                     label   = 'Change the plot background theme:',
                     choices = c('grey', 'dark','bw','classic','minimal','outline','light'),
                     inline = TRUE
                     ), # end radioButtons
        radioButtons(inputId = 'stiff.labels',
                     label   = "Toggle data point labels on/off in your Stiff Diagrams:",
                     choices = c('Labels On', 'Parameter Only', 'Labels Off'),
                     inline = TRUE
                     ), # end radioButtons
        br(),
        br(), 
        p(strong("Note on common errors:")),
        p('If you recieve an error about faceting in the large main plot, then you just need to select at least one category in Step 2.'),
        p('If you check a sample location before selecting any parameters to plot, you will 
           generate an error message. Just select some parameters and you should be good to go.'),
        width = 3
        
        ), # endsidebarPanel
    
    
    ### Main Panel ----------------------------------------------------------------------------------------------------------------------------------------------
    mainPanel(
        tabsetPanel(
            tabPanel('Plots',
                     br(),
                     
                     ### Scatter Plot ------------------------------------------------------------------------
                     h2("Plots For Entire Site"),
                     wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 700px",
                               h4("Zooming: Click and hold to select an area, then double-click. Double-click again to zoom back out."),
                               p("Error fix: If you have not selected any parameter categories (Step 2), then an error will appear (in red) because this doesn't know what parameters to plot."),
                        plotOutput(outputId = "scatterplot", 
                                   width="100%", height = '700px',
                                   dblclick="scatterplot_dblclick",brush=brushOpts(id="scatterplot_brush",resetOnNew=T)
                                   ) # end plotOutput
                        ), # end wellPanel
                     br(),
                     
                     ### Looped Plots ------------------------------------------------------------------------
                     h2("Plots for individual sample points"),
                     tabsetPanel(
                        tabPanel('Stiff Diagrams',
                                 wellPanel(id = "wPanel1", style = "overflow-y:scroll; max-height: 2400px",
                                           tags$head(tags$style("#tbl {white-space: nowrap;}")),
                                           fluidRow(uiOutput(outputId = 'plotlist_stiff'))
                                           ) # end wellPanel
                                 ),# end tabPanel
                        tabPanel('Radar Plots',
                                 wellPanel(id = "wPanel2", style = "overflow-x:scroll; max-height: 2400px",
                                           tags$head(tags$style("#tbl {white-space: nowrap;}")),
                                           fluidRow(uiOutput(outputId = 'plotlist_rad'))
                                           ) # end wellPanel
                                 ),# end tabPanel
                        tabPanel('Time-Series Plots',
                                 wellPanel(id = "wPanel3", style = "overflow-y:scroll; max-height: 2400px",
                                           tags$head(tags$style("#tbl {white-space: nowrap;}")),
                                           fluidRow(uiOutput(outputId = 'plotlist_ts'))
                                           ) # end wellPanel
                                 )#, # end tabPanel
                        #tabPanel('TEST slickR',
                        #         h4("This tab is for TESTING the slickR appraoch for making plots you can click through for each sample date."),
                        #         slickROutput(outputId = "my_slick", width='100%', height='520px')
                        #         ) # end tabPanel
                        ) # end tabsetPanel
                     ), # end tabPanel
            
            ### Datatable tabs -------------------------------------------------------------------------------
            tabPanel('EGAD Dataset',
                     h4("This table your original EGAD export which you uploaded. It has not been filtered or altered."),
                     wellPanel(id = "wPanel.dat1", style = "overflow-y:scroll; max-height: 2500px",
                        DT::dataTableOutput(outputId = 'dat0')
                        ) # end wellPanel
                     ), # end tabPanel
            tabPanel('Results Dataset',
                     h4("This table is a simplified version of your original EGAD file, showing only results from selected parameter categories."),
                     #downloadButton(outputId = "download.dat1", label = "Download Table as csv"),
                     wellPanel(id = "wPanel.dat1", style = "overflow-y:scroll; max-height: 2500px", 
                        DT::dataTableOutput(outputId = 'dat1')
                        ) # end wellPanel
                     ), # end tabPanel
            tabPanel('QAQC Dataset',
                     h4("This table is a simplified version of your original EGAD file, showing only QA/QC results."),
                     #downloadButton(outputId = "download.datqc", label = "Download Table as csv"),
                     wellPanel(id = "wPanel.dat1", style = "overflow-y:scroll; max-height: 2500px",
                        DT::dataTableOutput(outputId = 'datqc')
                        ) # end wellPanel
                     ), # end tabPanel
            tabPanel('Radar & Time-Series Plot Dataset',
                     h4("These are the parameters and sample points currently selected for your Radar and Time-Series plots."),
                     #downloadButton(outputId = "download.dat2_rad", label = "Download Table as csv"),
                     wellPanel(id = "wPanel.dat1", style = "overflow-y:scroll; max-height: 2500px",
                        DT::dataTableOutput(outputId = 'dat2_rad')
                        ) # end wellPanel
                     ), # end tabPanel
            tabPanel('Stiff-Diagram Dataset',
                     h4("These are the parameters and sample points currently selected for your Stiff plots."),
                     #downloadButton(outputId = "download.dat2_stiff", label = "Download Table as csv"),
                     wellPanel(id = "wPanel.dat1", style = "overflow-y:scroll; max-height: 2500px",
                        DT::dataTableOutput(outputId = 'dat2_stiff')
                        ) # end wellPanel
            ), # end tabPanel
            tabPanel('PFAS Dictionary',
                     h4("This table is a a dictionary for PFAS combound names and acronyms."),
                     wellPanel(id = "wPanel.dat1", style = "overflow-y:scroll; max-height: 2500px",
                        DT::dataTableOutput(outputId = 'dict')
                        ) # end wellPanel
            ) # end tabPanel
        ) # end tabsetPanel
        
    )# end mainPanel
    
) # End ui fluidPage

#### END UI 











###### Define Server logic ---------------------------------------------------------------------------------
server = function(input, output, session) {
    
    # Load packages
    require(tidyverse)
    require(ggrepel)
    require(DT)
    require(grid)
    require(openxlsx)
    
    # set maximim input file limit
    options(shiny.maxRequestSize=30*1024^2)
    
    
    ### Load data into dat0 ----------------------------------------------------------------------
    # create reactive function that produces 'dat0' dataframe 
    dat0 = eventReactive(input$file1, {
        
        # read data provided by input$file1
        dat0 = read.table(input$file1$datapath, fill=T, sep=",", header=T)
        
        # convert all values to class(character) from class(factor)
        for (i in 1:length(dat0)) {dat0[[i]]=as.character(dat0[[i]])}
        
        ### Option to accept WRDB export colnames
        cols = names(dat0)
        
        # define lists of expected names from egad and wrdb
        #egad_names = c("SITE_NAME","SAMPLE_POINT_NAME","SAMPLE_POINT_TYPE","SAMPLE.DATE","SAMPLE_ID","LAB_SAMPLE_ID",
        #               "ANALYSIS_LAB","SAMPLE_TYPE","QC_TYPE","SAMPLE_COLLECTION_METHOD","SAMPLE_FIELD_FILTER","TREATMENT_STATUS",
        #               "PARAMETER_FILTERED","SAMPLE_LOCATION","TEST","PARAMETER","CATEGORY","CONCENTRATION","PARAMETER_UNITS",
        #               "LAB_QUALIFIER","RL","RAISED_SQL","MDL","RESULT_TYPE","BATCH_ID","SAMPLE_DELIVERY_GROUP","QC_INDICATOR",
        #               "VALIDATION_QUALIFIER","VALIDATION_LEVEL","SAMPLE_DEPTH","SAMPLE_DEPTH_UNITS","DILUTION_FACTOR",
        #               "PARAMETER_QUALIFIER","SAMPLE_DEPTH_INTERVAL_TOP","SAMPLE_DEPTH_INTERVAL_BOTTOM" ,"SAMPLE_INTERVAL_UNIT",
        #               "PREP_METHOD","PREP_METHOD2","SAMPLE_COMMENT","TEST_COMMENT","LAB_COMMENT","VALIDATION_COMMENT",
        #               "VALIDATION_COMMENT_TYPE","SAMPLE_POINT_DESCRIPTION","SAMPLE_TYPE_QUALIFIER","SITE_SEQ","SAMPLE_POINT_SEQ",
        #               "SAMPLE_SEQ","SAMPLE.TEST_SEQ","PARAMETER_SEQ")
        #wrdb_names = c("Station ID","Date/Time","PCode","LEW","Depth","Acy","CCode","S","L","Q","R","Result","Val","Trk ID","Owner")
        
        # check for mistake in date column name
        ocn = colnames(dat0)
        ncn = gsub("SAMPLE.DATE", x = ocn, replacement = "SAMPLE_DATE")
        colnames(dat0) = ncn
        
        # convert concentration / result col to numeric
        if ('CONCENTRATION' %in% cols) {
            dat0$CONCENTRATION = as.numeric(dat0$CONCENTRATION)
        } # end if
        if ('Result' %in% cols) {
            dat0$Result = as.numeric(dat0$Result)
        } # end if
        
        # convert date column to posixct
        dat0$SAMPLE_DATE = as.POSIXct(dat0$SAMPLE_DATE, format = "%m/%d/%Y")
        

        return(dat0)
        
    }) # end eventReactive
    
    # Create datatable to show in sepparate tabpanel
    output$dat0 = DT::renderDataTable(
        DT::datatable(dat0(), options = list(paging = FALSE))
    ) # end renderDataTable
    
    ### Get site name from original EGAD dataframe
    site.name = reactive({
        if (names(dat0())[1] == "SITE_NAME") {
            site.name = dat0()[1,1]
        } # end if
        if (names(dat0())[1] == "Station ID") {
            site.name = input$site.name
        } # end if
        
        return(site.name)
        
        }) # end reactive
    
    
    
    ### PFAS Dictionary --------------------------------------------------------------------------------------------------------------
    # Define dictionary dataframe for converting PFAS names from long-form to acronyms
    dict = data.frame(#number = seq(1,55),
                      Acronym=c('PFPEA_A','PFHXA_A','PFDOA_A','PFDA_A','PFBA_A','PFHPA_A','PFNA_A','PFTEA_A','PFDS','PFTRIA_A',
                                'PFUNDA_A','PFBA','PFPEA','PFHXA','PFHPA','PFOA','PFNA','PFDA','PFUNDA','PFDOA','PFBS','PFHXS','PFOS_A',
                                'PFOSA','PFOA_A','PFOS','PFHXS_A','PFTEA','PFTRDA_A','6:2 FTS','PFHPS_A','N-ETFOSA','PFHXDA_A','PFBS_A',
                                '8:2 FTS','N-MEFOSA','N-MEFOSE','N-EtFOSE','N-EtFOSAA','N-MeFOSAA','PFOA + PFOS','PFDS_A','TOTAL PFCA',
                                'PFNS_A','PFPES_A','4:2 FTS_A','PFHPA + PFXHS + PFOA + PFNA + PFOS','HFPO-DA_A', 'ADONA_A','9CL-PF3ONS_A',
                                '11CL-PF3OUDS_A','13C3-HFPO-DA_A', 'PFDOS_A','PFODA_A','10:2 FTS_A', '6:2 FTS_A', '8:2 FTS_A'),
                      Name=c('PERFLUOROPENTANOIC ACID','PERFLUOROHEXANOIC ACID','PERFLUORODODECANOIC ACID','PERFLUORODECANOIC ACID',
                             'PERFLUOROBUTANOIC ACID','PERFLUOROHEPTANOIC ACID','PERFLUORONONANOIC ACID','PERFLUOROTETRADECANOIC ACID',
                             'PERFLUORODECANE SULFONATE','PERFLUOROTRIDECANOIC ACID','PERFLUOROUNDECANOIC ACID',
                             'PERFLUOROBUTANOATE','PERFLUOROPENTANOATE','PERFLUOROHEXANOATE','PERFLUOROHEPTANOATE',
                             'PERFLUOROOCTANOATE','PERFLUORONONANOATE','PERFLUORODECANOATE','PERFLUOROUNDECANOATE',
                             'PERFLUORODODECANOATE','PERFLUOROBUTANE SULFONATE','PERFLUOROHEXANE SULFONATE',
                             'PERFLUOROOCTANE SULFONIC ACID','PERFLUOROOCTANE SULFONAMIDE','PERFLUOROOCTANOIC ACID',
                             'PERFLUOROOCTANE SULFONATE','PERFLUOROHEXANE SULFONIC ACID','PERFLUOROTETRADECANOATE',                    
                             'PERFLUOROTRIDECANOATE','6:2 FLUOROTELOMER SULFONATE','PERFLUOROHEPTANE SULFONIC ACID',
                             'N-ETHYL PERFLUOROOCTANE SULFONAMIDE','PERFLUOROHEXADECANOIC ACID','PERFLUOROBUTANE SULFONIC ACID',
                             '8:2 FLUOROTELOMER SULFONATE','N-METHYL PERFLUOROOCTANE SULFONAMIDE',
                             'N-METHYL PERFLUOROOCTANE SULFONAMIDOETHANOL','N-ETHYL PERFLUOROOCTANE SULFONAMIDOETHANOL',
                             'N-ETHYL PERFLUOROOCTANE SULFONAMIDOACETIC ACID','N-METHYL PERFLUOROOCTANE SULFONAMIDOACETIC ACID',
                             'TOTAL PFOA AND PFOS','PERFLUORODECANESULFONIC ACID','TOTAL PERFLUOROCARBOXYLIC ACIDS',
                             'PERFLUORONONANE SULFONIC ACID','PERFLUOROPENTANE SULFONIC ACID','4:2-FLUOROTELOMER SULFONIC ACID',
                             'TOTAL PFAS (PER CT GUIDELINES)','HEXAFLUOROPROPYLENE OXIDE DIMER ACID',
                             '4,8-DIOXA-3H-PERFLUORONONANOIC ACID','9-CHOLOROHEXADECAFLUORO-3-OXANONANE-1-SULFONIC ACID',
                             '11-CHLOROEICOSAFLUORO-3-OXAUNDECANE-1-SULFONIC ACID','HEXAFLUOROPROPYLENE OXIDE DIMER ACID-13C3',
                             'PERFLUORODODECANE SULFONIC ACID','PERFLUOROOCTADECANOIC ACID','10:2 FLUOROTELOMER SULFONIC ACID', 
                             '6:2 FLUOROTELOMER SULFONIC ACID', '8:2 FLUOROTELOMER SULFONIC ACID')
                      ) # end data.frame
    
    dict$Acronym = as.character(dict$Acronym)
    dict$Name    = as.character(dict$Name)
    
    # Create datatable to show in sepparate tabpanel
    output$dict = DT::renderDataTable(
        DT::datatable(dict, options = list(paging = FALSE))
    ) # end renderDataTable
    
    
    
    ### Create dat1 -----------------------------------------------------------------------------
    # create reactive function 'dat1()' which creates dataframe 'dat1' from the reactive function 'dat0'
    # wrangle dataframe into neater format with only neccessary columns for plotting
    dat1 = reactive({
        
        #if (names(dat0())[1] == "Station ID") {
        #    dat1 = dat0() %>%
        #        select('Sample Point Name' = `Station ID`,
        #               'Sample Date'       = `Date/Time`,
        #               Parameter           = PCode,
        #               Concentration       = RRESULT,
        #               Units               = CCode) %>%
        #        filter(!Units %in% c('%R', '% R'))
        #    
        #    for (i in 1:nrow(dat1)) {
        #        dat1$Category[dat1$]
        #    }
        #}

        dat1 = dat0()                                                          %>%
            filter(CATEGORY %in% input$param.cats,
                   !PARAMETER_UNITS   %in% c('%R','% R', 'RPD', '%'),
                   !SAMPLE_POINT_TYPE %in% c("QA/QC"),
                   SAMPLE_DATE >= input$date.selector[1] & SAMPLE_DATE <= input$date.selector[2])                     %>%
            select(SAMPLE_POINT_NAME, SAMPLE_POINT_TYPE, SAMPLE_DATE,
                   CATEGORY, PARAMETER, CONCENTRATION, PARAMETER_UNITS)      %>%
            #mutate(date = as.POSIXct(SAMPLE_DATE, format = "%m/%d/%Y"))      %>%
            select('Sample Point Name' = SAMPLE_POINT_NAME,
                   'Sample Point Type' = SAMPLE_POINT_TYPE,
                   'Sample Date'       = SAMPLE_DATE,
                   Category            = CATEGORY,
                   Parameter           = PARAMETER, 
                   Concentration       = CONCENTRATION, 
                   Units               = PARAMETER_UNITS)                    
        
        # Replace EGAD parameter names in 'dat1$param' with their corresponding nickname based on the 'dict' dictionary
        for (i in dict[[2]]) {
            if (i %in% dat1$Parameter) {
                dat1$Parameter[dat1$Parameter==i] = dict[which(dict[,2]==i),1]
            } # end if
        } # end for
        
        return(dat1)
        
    }) # End reactive
    
    
    ### Unit Converter --------------------------------------------------------------------------------------
    
    # create list of events that the following 'eventReactive' will react to changes in
    convert.events = reactive({list(input$convert.button, dat1())})
    
    ### define reactive function 'dat1c' which is triggered by events contained within the reactive 'convert.events'
    # 'dat1c()' outputs the 'dat1c' dataframe each time an object within 'convert.events' is updated
    dat1c = eventReactive(convert.events(), {
        
        # define 'meq.con' dataframe with chemical information needed to convert given anions and cations to MEQ/L
        eqc = data.frame(Parameter  = c('BROMIDE','CALCIUM','CHLORIDE','FLUORIDE','MAGNESIUM','POTASSIUM','SODIUM','SULFATE','SULFIDE','TOTAL ALKALINITY','TOTAL ORGANIC CARBON'),
                             Category   = c('ANION','CATION','ANION','ANION','CATION','CATION','CATION','ANION','ANION','ANION','ANION'),
                             Valence    = c(1,2,1,1,2,1,1,2,2,1,4),
                             AtomicMass = c(79.904,40.078,35.45,18.998,24.305,39.098,22.99,96.06,32.06,61.0168,12.011))
        
        # create local copy of dat1() so we don't call dat1() which is an event which triggers this whole reactive and will cause infinite recursion error
        dat1copy = isolate(dat1())
        
        # use temporary, local dataframe instead of the reactive dat1() or 'dat1copy' to alter by the following conversions
        # this is because 'dat1()' can retrieve but not modify 'dat1'
        # also filter to just results with units that the converter can handle. Nessecary to avoid "number of items to replace is not a multiple of replacement length" error
        tmp = dat1copy 
        
        # Loop through each pair of from and to unit arguments spe
        for (x in 1:min(c(length(input$to.units),length(input$from.units)))) {
            
            # define local 't' and 'f' for each iteration of this loop
            t = input$to.units[x]
            f = input$from.units[x]
            # Convert all parameters in a category ('fc') specified by users in 'selectInput'
            fc = f
            # convert to sepparate arguments if a single category is not specified
            if (fc == 'ANION & CATION') {fc = c('ANION','CATION')} 
            if (fc == 'Everything')     {fc = c('METAL','VOC','SVOC','ANION','CATION','PFC','INDICATOR','NUTRIENT')}
            # get from units associated with the user specified categories
            fu = unique(tmp$Units[tmp$Category%in%fc]) # store from units
            # if converting to original units replace the specified category with original values and units
            if (t == 'Original Units') {
                tmp$Concentration[tmp$Category%in%fc] = dat1copy$Concentration[dat1copy$Category%in%fc]
                tmp$Units[tmp$Category%in%fc]         = dat1copy$Units[dat1copy$Category%in%fc]
            } # end if
            
            # Loop through from units and convert
            for (u in 1:length(fu)) {
                fish = fu[u]
                # perform metric unit conversions for specified categories  
                if (fish == 'NG/L' & t == 'MG/L')  {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e-6; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NG/L' & t == 'UG/L')  {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e-3; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MG/L' & t == 'NG/L')  {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e6;  tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MG/L' & t == 'UG/L')  {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e3;  tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UG/L' & t == 'NG/L')  {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e3;  tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UG/L' & t == 'MG/L')  {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e-3; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UEQ/L'& t == 'MEQ/L') {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e-3; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NEQ/L'& t == 'MEQ/L') {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e-6; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UEQ/L'& t == 'NEQ/L') {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e3;  tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MEQ/L'& t == 'NEQ/L') {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e6;  tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NEQ/L'& t == 'UEQ/L') {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e-3; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MEQ/L'& t == 'UEQ/L') {tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish] = tmp$Concentration[tmp$Category%in%fc&tmp$Units==fish]*1e3;  tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                # Convert to equivalents from standard metric
                if (fish == 'MG/L' & t == 'MEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*     eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UG/L' & t == 'MEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e-3*eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NG/L' & t == 'MEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e-6*eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MG/L' & t == 'UEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e3 *eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UG/L' & t == 'UEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*     eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NG/L' & t == 'UEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e-3*eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MG/L' & t == 'NEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e6 *eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UG/L' & t == 'NEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e3 *eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NG/L' & t == 'NEQ/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*     eqc$Valence[eqc$Parameter==i]) / eqc$AtomicMass[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                # Convert from equivalents to standard metric
                if (fish == 'MEQ/L' & t == 'MG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*     eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UEQ/L' & t == 'MG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e-3*eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NEQ/L' & t == 'MG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e-6*eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MEQ/L' & t == 'UG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e3 *eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UEQ/L' & t == 'UG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*     eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NEQ/L' & t == 'UG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e-3*eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'MEQ/L' & t == 'NG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e6 *eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'UEQ/L' & t == 'NG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*1e3 *eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
                if (fish == 'NEQ/L' & t == 'NG/L') {for (i in unique(tmp$Parameter[tmp$Category%in%fc&tmp$Units==fish])) {if (i %in% eqc$Parameter) {tmp$Concentration[tmp$Parameter==i&tmp$Units==fish] = (tmp$Concentration[tmp$Parameter==i&tmp$Units==fish]*     eqc$AtomicMass[eqc$Parameter==i]) / eqc$Valence[eqc$Parameter==i]}}; tmp$Units[tmp$Category%in%fc&tmp$Units==fish] = t} # end conversion
            } # end for fish loop
        } # end for x input loop
        
        # redefine dat1c
        dat1c = tmp
        
        return(dat1c)
        
    }) # end eventReactive
    
    
    
    # Create datatable to show in sepparate tabpanel
    output$dat1 = DT::renderDataTable(
        DT::datatable(dat1c() %>% mutate(`Sample Date` = as.character(`Sample Date`)),  
                      options = list(paging = FALSE))
    ) # end renderDataTable
    
    # Create download button for dat1c
    #output$download.dat1 = downloadHandler(filename = function() {paste(site.name(),'_','selected_categories.csv')},
    #                                       content  = function(file) {write.csv(isolate(dat1c()))})
    
    
    ### Create datqc -----------------------------------------------------------------------------
    # create reactive function 'datqc()' which creates dataframe 'datqc' from the reactive function 'dat0'
    # wrangle dataframe into neater format with only neccessary columns for plotting
    datqc = reactive({
        
        datqc = dat0()
        ocn = colnames(datqc)
        ncn = gsub("SAMPLE.DATE", x = ocn, replacement = "SAMPLE_DATE")
        colnames(datqc) = ncn
        
        datqc = datqc                                                        %>%
            filter(CATEGORY %in% input$param.cats,
                   SAMPLE_POINT_TYPE == "QA/QC" | PARAMETER_UNITS %in% c("%", "%R", "RPD", "% ", "% R"))     %>%
            select(SAMPLE_POINT_NAME, SAMPLE_POINT_TYPE, SAMPLE_DATE,
                   CATEGORY, PARAMETER, CONCENTRATION, PARAMETER_UNITS)      %>%
            mutate(date = as.POSIXct(SAMPLE_DATE, format = "%m/%d/%Y"))      %>%
            select('Sample Point Name' = SAMPLE_POINT_NAME,
                   'Sample Point Type' = SAMPLE_POINT_TYPE,
                   'Sample Date'       = date,
                   Category            = CATEGORY,
                   Parameter           = PARAMETER, 
                   Concentration       = CONCENTRATION, 
                   Units               = PARAMETER_UNITS)
        
        # Replace EGAD parameter names in 'dat1$param' with their corresponding nickname based on the 'dict' dictionary
        for (i in dict[[2]]) {if (i %in% datqc$Parameter) {datqc$Parameter[datqc$Parameter==i] = dict[which(dict[,2]==i),1]}} # end translation
        
        return(datqc)
        
    }) # End reactive
    
    # Create datatable to show in sepparate tabpanel
    output$datqc = DT::renderDataTable(
        DT::datatable(datqc() %>% mutate(`Sample Date` = as.character(`Sample Date`)),  
                      options = list(paging = FALSE))
    ) # end renderDataTable
    
    
    
    ### Create dat2 ------------------------------------------------------------------------------------------
    # output a reactive function that creates 'dat2' dataframe from the 'dat1()' reactive
    
    # for stiff plots, filtered for stiff plot parameter inputs
    dat2_stiff = reactive({
        # create 'left' and 'right' designations according to parameter, filter out non-plot parameters
        dat2 = dat1c() %>%                                            
            filter(Parameter           %in% c(input$plot.left, input$plot.right),
                   `Sample Point Name` %in% input$sample.points)
    }) # end reactive
    
    # for rad and ts plots, filtered for these params
    dat2_rad = reactive({
        # create 'left' and 'right' designations according to parameter, filter out non-plot parameters
        dat2_rad = dat1c() %>%                                            
            filter(Parameter           %in% input$param.list,
                   `Sample Point Name` %in% input$sample.points)
    }) # end reactive
    
    # Create datatable to show in sepparate tabpanel
    output$dat2_stiff = DT::renderDataTable(
        DT::datatable(dat2_stiff() %>% mutate(`Sample Date` = as.character(`Sample Date`)),  
                      options = list(paging = FALSE))
    ) # end renderDataTable
    output$dat2_rad = DT::renderDataTable(
        DT::datatable(dat2_rad() %>% mutate(`Sample Date` = as.character(`Sample Date`)),
                      options = list(paging = FALSE)) 
    ) # end renderDataTable
    
    
    
    ### Download tables -----------------------------------------------------------------------------
    
    #output$download.tables = downloadHandler(  
    #    filename = function()          {paste(site.name(),"_data.csv", sep='')}, # end filename function
    #    content  = function(file.name) {write.csv(dat1c(), file = file.name)} # end content function
    #) # end downloadHandler
    
    output$download.tables = downloadHandler(  
        filename = function()          {paste(site.name(),"---PlotR Export---",Sys.Date(),".xlsx", sep='')}, # end filename function
        content  = function(file.name) {
            my_workbook = createWorkbook()
            addWorksheet(wb        = my_workbook,
                         sheetName = "Original EGAD Export")
            writeData(wb           = my_workbook,
                      sheet        = 1,
                      x            = dat0())
            addWorksheet(wb        = my_workbook,
                         sheetName = "Simplified Results")
            writeData(wb           = my_workbook,
                      sheet        = 2,
                      x            = dat1c())
            addWorksheet(wb        = my_workbook,
                         sheetName = "Plot Data - Radar, Time-Series")
            writeData(wb           = my_workbook,
                      sheet        = 3,
                      x            = dat2_rad())
            addWorksheet(wb        = my_workbook,
                         sheetName = "Plot Data - Stiff Diagrams")
            writeData(wb           = my_workbook,
                      sheet        = 4,
                      x            = dat2_stiff())
            saveWorkbook(wb   = my_workbook,
                         file = file.name)
            } # end content function
    ) # end downloadHandler
    
    
    
    ### Scatter plot --------------------------------------------------------------------------------
    
    # prepare for zoomability, something to do with the brush I think
    ranges = reactiveValues(x = NULL, y = NULL)
    
    # use 'renderPlot' to output a scatter plot of data from 'dat1'
    output$scatterplot = renderPlot({
        ### Plot all PFAS concentrations, color code by sample point
        ggplot(data = dat1())                                                     + 
            aes(x = Parameter, y = Concentration, color = `Sample Point Name` )   + 
            geom_point()                                                          +
            facet_grid( . ~ Category, scales = "free", space="free_x")            +
            labs(x     = '',
                 y     = 'Concentration (See table for units)',
                 color = 'Sample\nPoint',
                 title = paste(site.name(), "Results", sep=': '))                 +
            guides(color = guide_legend(override.aes = list(size = 1),
                                        ncol = 18))                                +
            theme(legend.title         = element_text(size = 20),
                  legend.text          = element_text(size = 10),
                  legend.key.size      = unit(1, "points"),
                  legend.position      = "bottom",
                  legend.justification = "left",
                  legend.direction     = "horizontal",
                  legend.margin        = margin(t=1, r=1, b=1, l=1, unit="mm"),
                  title = element_text(size=20),
                  axis.text.x   = element_text(size=20, angle=300, hjust=0),
                  axis.text.y   = element_text(size=20),
                  axis.title.x  = element_text(size=20),
                  axis.title.y  = element_text(size=20),
                  panel.spacing = unit(1.5, "lines"))                               +
            # to zoom, need to define coord limits based on ranges
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = T)         +
            scale_x_discrete(expand = c(.05,.05))                                   
    }, 
    width = 1600, height = 1000
    ) # End renderPlot
    
    # set up double click and brush to define ranges that are passed to coord limits within 'renderPlot'
    observeEvent(input$scatterplot_dblclick, {
        brush = input$scatterplot_brush
        if (!is.null(brush)) {
            ranges$x = c(brush$xmin, brush$xmax)
            ranges$y = c(brush$ymin, brush$ymax)
        } else {
            ranges$x = NULL
            ranges$y = NULL
        } # end ifelse
    }) # end ObserveEvent
    
    
    
    ### renderUI inputs --------------------------------------------------------------------------------
    # create inputs for selecting parameters and sample points based off of dat1() 
    
    output$param.selector.left  = renderUI({selectInput(inputId  = "plot.left",
                                                        label    = "Stiff Plots: Parameters on the LEFT",
                                                        choices  = unique(dat1()$Parameter),
                                                        multiple = TRUE
                                                        ) # end selectInput
    }) # end renderUI
    
    output$param.selector.right = renderUI({selectInput(inputId  = "plot.right",
                                                        label    = "Stiff Plots: Parameters on the RIGHT",
                                                        choices  = unique(dat1()$Parameter),
                                                        multiple = TRUE
                                                        ) # end selectInput
    }) # end renderUI

    output$rad.params = renderUI({checkboxGroupInput(inputId  = "param.list",
                                                     label    = "Radar & Time-Series Plots: Select parameters you want to plot",
                                                     choices  = unique(dat1()$Parameter),
                                                     selected = unique(dat1()$Parameter),
                                                     inline   = FALSE,
                                                     width    = "100%"
                                                     ) # end checkboxGroupInput
    }) # end renderUI

    output$sample.point.selector = renderUI({checkboxGroupInput(inputId = "sample.points",
                                                                label   = "All Plots: These are the sample point locations with measurements in your specified parameter categories",
                                                                choices = unique(dat1()$`Sample Point Name`)
                                                                ) # end checkboxGroupInput
    }) # end renderUI
    
    output$param.category.selector = renderUI({
        # create vector of categories to display
        x = unique(dat0()[which(!dat0()$SAMPLE_POINT_TYPE=="QA/QC"),'CATEGORY'])
        x = x[!x %in% "SURROGATE"]
        # create ui object
        ui = checkboxGroupInput(inputId = "param.cats",
                                label    = "Non-checked parameter categories will be filtered out",
                                choices  = x,
                                selected = "PFC"
                                ) # end checkboxGroupInput
        return(ui)
    }) # end renderUI
    
    # Generate date selector to filter dat1()
    output$date.selector = renderUI({dateRangeInput(inputId = "date.selector",
                                                    label   = "Select your range of sample dates:",
                                                    start   = min(dat0()$SAMPLE_DATE,na.rm=T),
                                                    end     = max(dat0()$SAMPLE_DATE,na.rm=T),
                                                    min     = min(dat0()$SAMPLE_DATE,na.rm=T),
                                                    max     = max(dat0()$SAMPLE_DATE,na.rm=T)
                                                    ) # end dateRangeInput
    }) # end renderUI
    
    
    
    #### renderUI plot loop ------------------------------------------------------------------------------
    # create 'outputPlot' calls in the UI 
    
    # This renderUI function calls 'map' to loop through each element within 'plotlist' and uses 'do.call' to call "renderPlot"
    # each "renderPlot" call is within a column, each 'plotlist' element is fed to "renderPlot" using 'do.call''s "args" arg
    # putting "renderPlot" within do.call allows us to easily invoke 'renderPlot' without having a whole reactive function, which is 
    # aparently easier to execute within a loop. This is neccesary because the loop ('map') needs to be in a 'renderUI' reactive function
    # because you can't call 'map' outside of a reactive expression in Shiny. So basically we are invoking a reactive function ('renderPlot')
    # within a loop ('map') within a 'renderUI' reactive that sends the output of the loop to the UI. There may be a way to this by calling 
    # 'renderPlot' itself rather than putting it in 'do.call', but this works well as is. 
    output$plotlist_stiff = renderUI({
        ui = map(.x = 1:length(plotlist_stiff()),
                 .f = ~column(width = 4, 
                              do.call("renderPlot", 
                                      args=list(expr   = plotlist_stiff()[.x], 
                                                width  = 520, 
                                                height = 520
                                                ) # end list
                                      ) # end do.call
                              ) # end column
                 ) # end map
        return(ui)
    }) # end renderUI
    
    
    #output$plotlist_rad = renderUI({
    #    ui = map(.x = 1:length(plotlist_rad()),
    #             .f = ~function(.x) {
    #                    n = .x 
    #                    wellPanel(id = "radlooppanel", style = "overflow-x:scroll; max-height: 550px",
    #                        map(.x = 1:length(plotlist_rad()[.x]),
    #                            .f = ~column(width = 12/length(plotlist_rad()[.x]), 
    #                                    do.call("renderPlot", 
    #                                            args=list(expr   = plotlist_rad()[n][.x], 
    #                                                      width  = 520, 
    #                                                      height = 520
    #                                                      ) # end list
    #                                        ) # end do.call
    #                               ) # end column
    #                        ) # end map
    #                    ) # end wellPanel
    #             } # end function
    #    ) # end map
    #    return(ui)
    #}) # end renderUI
    
    output$plotlist_rad = renderUI({
        ui = map(.x = 1:length(plotlist_rad()),
                 .f = ~column(width=4,
                              do.call("renderPlot",
                                      args=list(expr   = plotlist_rad()[.x],
                                                width  = 520,
                                                height = 520
                                                ) # end list
                                      ) # end do.call
                              ) # end column
                 ) # end map
        return(ui)
    }) # end renderui
    
    
    output$plotlist_ts = renderUI({
        ui = map(.x = 1:length(plotlist_ts()),
                 .f = ~column(width = 12, 
                              do.call("renderPlot", 
                                      args=list(expr   = plotlist_ts()[.x], 
                                                width  = 1600, 
                                                height = 600
                                      ) # end list
                              ) # end do.call
                 ) # end column
        ) # end map
        return(ui)
    }) # end renderUI
    
    
    
    #### Define 'plotfun' functions -------------------------------------------------------------------------------
    
    ### define 'plotfun_stiff' to create stiff diagrams
    plotfun_stiff = function(plotname) {
            
            ### Create dat3 object with all Stiff-Diagram info, unfiltered for point or date
            dat3 = dat2_stiff()                                                        %>%
                select(param             = Parameter, 
                       conc              = Concentration, 
                       sample.date       = `Sample Date`,
                       sample.point.name = `Sample Point Name`,
                       Units)                                                          %>%
                mutate(side = ifelse(param %in% input$plot.left,  'left',
                              ifelse(param %in% input$plot.right, 'right', NA)),
                       conc = ifelse(side == 'left', conc*-1, conc))                   %>%
                na.omit                                                                %>%
                group_by(side)                                                         %>%
                mutate(ypos = ifelse(side == 'left',
                                     -match(param, input$plot.left), 
                                     -match(param, input$plot.right)))                 %>%
                group_by(sample.point.name)                                            %>%
                mutate(ypos = ifelse(side=='left',ypos*-1,ypos))                       %>% 
                arrange(side, ypos)                                                    %>%
                mutate(ypos = ifelse(side=='left',ypos*-1,ypos))
            
            ### create dat4 object, filtered for point
            dat4 = dat3                                %>%
                filter(sample.point.name == plotname)  %>% 
                group_by(sample.point.name)                                                
                
            
            ### Loop through each unique sample date, in order
            # prepare list for plots of each date for individual sample point
            plist = list()
            for (i in as.list(sort(unique(dat4$sample.date)))) {
                
                ### Pipeline to define plotdat, filtered for date
                pdat = dat4                   %>% 
                    filter(sample.date == i)  
                    
                ### define plot object p
                p = ggplot(data = pdat)                                                       +                 
                    aes(x = conc, y = ypos)                                                   +
                    geom_polygon(aes(x = conc, y = ypos), alpha=0.5, color = 'blue')          +
                    geom_point(  aes(x = conc, y = ypos), size=7)                             +
                    geom_vline(xintercept = 0)                                                +
                    labs(x = paste('Concentration (',pdat$Units[1],')', sep=''),
                         y = '',
                         color = 'Parameter',
                         title = paste(site.name(),plotname, i, sep=": "))                    +
                    scale_y_continuous(expand = c(.1,.1))    
                # turn labels on/off
                if (input$stiff.labels == 'Labels On') {
                    p = p + geom_text_repel(aes(label=paste(param,'=',abs(conc),sep=' ')),
                                            size          = input$plot.label.size, 
                                            point.padding = .8,
                                            #label.padding = 1,
                                            segment.alpha = 0,
                                            box.padding = 1,
                                            max.iter = 4000,
                                            direction = "both") 
                }
                if (input$stiff.labels == 'Parameter Only') {
                    p = p + geom_text_repel(aes(label=param),
                                            size          = input$plot.label.size, 
                                            point.padding = .8, 
                                            #label.padding = 1,
                                            segment.alpha = 0,
                                            box.padding = 1,
                                            max.iter = 4000,
                                            direction = "both") 
                }
                # Specify x axis based on check box 'input$axis.scale'
                if (input$axis.scale == 'Unique') {
                    p = p + scale_x_continuous(labels = abs,
                                               breaks = pretty(pdat$conc, n=10),
                                               expand = c(.1,.1)) 
                } 
                if (input$axis.scale == 'Same for each sample location') {
                    p = p + scale_x_continuous(labels = abs,
                                               breaks = pretty(seq(min(dat4$conc), 
                                                                   max(dat4$conc), length.out = 10)),
                                               expand = c(.1,.1),
                                               limits = c(min(dat4$conc), 
                                                          max(dat4$conc)))
                }
                if (input$axis.scale == 'Same for all sample locations and dates') {
                    p = p + scale_x_continuous(labels = abs,
                                               breaks = pretty(seq(min(dat3$conc), 
                                                                   max(dat3$conc), length.out = 10)),
                                               expand = c(.1,.1),
                                               limits = c(min(dat3$conc), 
                                                          max(dat3$conc)))
                } # end else
                # Option for user to change background theme
                if (input$theme == 'dark')    {p = p + theme_dark()} 
                if (input$theme == 'bw')      {p = p + theme_bw()}
                if (input$theme == 'classic') {p = p + theme_classic()}
                if (input$theme == 'minimal') {p = p + theme_minimal()}
                if (input$theme == 'outline') {p = p + theme_linedraw()}
                if (input$theme == 'light')   {p = p + theme_light()} 
                p = p + theme(text = element_text(size = input$plot.text.size),
                              axis.text.y  = element_blank(),
                              #axis.text.x  = element_blank(), # need this axis text for stiffs
                              axis.ticks.y = element_blank(),
                              legend.key.size = unit(1,'cm'))
                # end plot
                
                # populate plist with each ggplot object
                plist[[length(plist)+1]] = p
                
                } # end for
        
        return(plist) 
    
    } # end function
    
    
    
    ### define 'radplotfun' for making radar plots for a single site name or plotname
    plotfun_rad = function(plotname) { 
        
        # filter to each plotname input and the params in the user specified param.list
        dat3 = dat2_rad()                                         %>% 
            filter(`Sample Point Name` == plotname)               %>%
            arrange(Parameter)  
        
        ### Loop through each unique sample date, in order
        # prepare list for plots of each date for individual sample point
        plist = list()
        for (i in as.list(sort(unique(dat3$`Sample Date`)))) {
            
            ### Pipeline to define plotdat
            pdat = dat3                                           %>% 
                filter(`Sample Date` == i)                        %>%
                mutate(pmatch = as.numeric(as.factor(Parameter))) %>%
                arrange(Parameter)
            
            # store vals for use in plots 
            npdat = as.numeric(length(unique(pdat$Parameter)))
            cmd2c = ceiling(max(dat2_rad()$Concentration))
            cmd3c = ceiling(max(dat3$Concentration))
            cmpdc = ceiling(max(pdat$Concentration))
            
            ### define plot object p 
            p = ggplot(data = pdat)                                                                      + 
                geom_point(  aes(x = Parameter,  y = Concentration), size=3)                             +
                geom_polygon(aes(x = pmatch, y = Concentration), alpha = 0.5, color = "blue")            +
                labs(x = '',                 
                     y = '',                 
                     title = paste(site.name(),plotname, (i), sep=": "))                                 +                             
                # use ggiraph to transform into polar coordinate system,            
                # with straight lines connecting polygon vertices           
                ggiraphExtra:::coord_radar(start=1/npdat)                                                +
                scale_x_discrete(expand=c(0,.5))
            # Specify x axis based on check box 'input$axis.scale'
            if (input$axis.scale == 'Unique') {
                p = p + scale_y_continuous(breaks = round(seq(0, cmpdc, length.out=5)),
                                           expand = c(0,0))                                              +
                    # use annotate to add manual x axis labels around the outer edges of the plot
                    # annotate used instead of geom_text because its simpler and less error prone
                    annotate(geom  = "text",
                             label = unique(pdat$Parameter),
                             x     = unique(pdat$pmatch), 
                             y     = 1+1.8*max(pdat$Concentration), 
                             size  = input$plot.label.size,
                             angle = c( 90 - 360/npdat*unique(pdat$pmatch)[1:(npdat%/%2)],
                                       -90 - 360/npdat*unique(pdat$pmatch)[(npdat%/%2+1):npdat]))        +
                    # use annotate to add manual y axis tick mark labels 
                    # neccessary because of coord_radar
                    annotate(geom  = "text", 
                             color = 'red',
                             size  = input$plot.label.size,
                             x     = 0, 
                             y     = round(seq(0, cmpdc, length.out=5)), 
                             label = round(seq(0, cmpdc, length.out=5))) 
            } # end if
            if (input$axis.scale == 'Same for each sample location') { 
                    p = p + scale_y_continuous(breaks = round(seq(0, cmd3c, length.out=5)),
                                           limits = c(min(dat3$Concentration), 
                                                      1.1*max(dat3$Concentration)+1),
                                           expand = c(0,0))                                              +
                    # use annotate to add manual x axis labels around the outer edges of the plot
                    # annotate used instead of geom_text because its simpler and less error prone
                    annotate(geom  = "text",
                             label = unique(pdat$Parameter),
                             x     = unique(pdat$pmatch), 
                             y     = 1+1.1*max(dat3$Concentration), 
                             size  = input$plot.label.size,
                             angle = c( 90 - 360/npdat*unique(pdat$pmatch)[1:(npdat%/%2)],
                                       -90 - 360/npdat*unique(pdat$pmatch)[(npdat%/%2+1):npdat]))        +
                    # use annotate to add manual y axis labels. neccessary because of coord_radar
                    annotate(geom  = "text", 
                             color = 'red',
                             size  = input$plot.label.size,
                             x     = 0, 
                             y     = round(seq(0,cmd3c,length.out=5)), 
                             label = round(seq(0,cmd3c,length.out=5)))   
            } # end if
            if (input$axis.scale == 'Same for all sample locations and dates') {
                p = p + scale_y_continuous(breaks = round(seq(0, cmd2c, length.out=5)),
                                           limits = c(min(dat2_rad()$Concentration), 
                                                      1.1*max(dat2_rad()$Concentration)+1),
                                           expand = c(0,0))                                              +
                    # use annotate to add manual x axis labels around the outer edges of the plot
                    # annotate used instead of geom_text because its simpler and less error prone
                    annotate(geom  = "text",
                             label = unique(pdat$Parameter),
                             x     = unique(pdat$pmatch), 
                             y     = 1+1.1*max(dat2_rad()$Concentration), 
                             size  = input$plot.label.size,
                             angle = c( 90 - 360/npdat*unique(pdat$pmatch)[1:(npdat%/%2)],
                                       -90 - 360/npdat*unique(pdat$pmatch)[(npdat%/%2+1):npdat]))        +
                    # use annotate to add manual y axis labels. neccessary because of coord_radar
                    annotate(geom  = "text", 
                             color = 'red',
                             size  = input$plot.label.size,
                             x     = 0, 
                             y     = round(seq(0,cmd2c,length.out=5)), 
                             label = round(seq(0,cmd2c,length.out=5)))   
            } # end if
            # Option for user to change background theme
            if (input$theme == 'dark')    {p = p + theme_dark()} 
            if (input$theme == 'bw')      {p = p + theme_bw()}
            if (input$theme == 'classic') {p = p + theme_classic()}
            if (input$theme == 'minimal') {p = p + theme_minimal()}
            if (input$theme == 'outline') {p = p + theme_linedraw()}
            if (input$theme == 'light')   {p = p + theme_light()} 
            p = p + theme(text = element_text(size = input$plot.text.size),
                          axis.text.y  = element_blank(),
                          axis.text.x  = element_blank(),
                          axis.ticks.y = element_blank(),
                          legend.key.size = unit(1,'cm'))
            # end plot
            
            # populate plist with each ggplot object
            plist[[length(plist)+1]] = p
            
        } # end for
        
        return(plist)
        
    } # end function
    

    ### Define 'plotfun_ts' to create time-series plots
    plotfun_ts = function(plotname) {
        
        # filter data to create individual plots for each sample point location
        dat3 = dat2_rad() %>%
            filter(`Sample Point Name` == plotname) 
        
        ### define plot object p
        p = ggplot(data = dat3) + aes(x = `Sample Date`, y = Concentration, color = Parameter) +
            geom_point()                                                                       + 
            geom_line()                                                                        +
            facet_grid( Category ~ ., scales = "free")                                         +
            labs(x = 'Sample Date',
                 y = 'Concentration (Check table for units)',
                 color = 'Parameter',
                 title = paste(site.name(),plotname, sep=": "))                                +
            scale_x_datetime(limit = c(min(dat3$`Sample Date`),max(dat3$`Sample Date`)),
                             date_labels = '%Y-%b',
                             date_breaks = "6 months")                                                
        # Option for user to change background theme
        if (input$theme == 'dark')    {p = p + theme_dark()} 
        if (input$theme == 'bw')      {p = p + theme_bw()}
        if (input$theme == 'classic') {p = p + theme_classic()}
        if (input$theme == 'minimal') {p = p + theme_minimal()}
        if (input$theme == 'outline') {p = p + theme_linedraw()}
        if (input$theme == 'light')   {p = p + theme_light()} 
        p = p + theme(text        = element_text(size = input$plot.text.size),
                      axis.text.x = element_text(angle = -45)) 

        # end plot
        
        return(p)
    }
    
    
    
    ### Create Plotlists --------------------------------------------------------------------------------------------------------
    # Call and loop 'plotfun' using the 'map' function
    plotlist_stiff = reactive({
        plotlist_stiff1 = purrr::map(.x = input$sample.points,
                                     .f = ~plotfun_stiff(.x)
                                     ) # end map
        plotlist_stiff = unlist(plotlist_stiff1, recursive=F)
    }) # end reactive
    
    # create a list of list of ggplot objects, 
    # the first level corresponds to sample points, and the inner to sample dates
    #plotlistlist_rad = reactive({
    #    plotlistlist_rad = purrr::map(.x = input$sample.points,
    #                                  .f = ~plotfun_rad(.x)
    #    ) # end map
    #}) # end reactive
    
    # create a flattenned list of ggplot objects with only one level
    plotlist_rad = reactive({
        plotlist_rad1 = purrr::map(.x = input$sample.points,
                                   .f = ~plotfun_rad(.x)
                                   ) # end map
        plotlist_rad = unlist(plotlist_rad1, recursive=F)
    })
    
    plotlist_ts = reactive({
        plotlist_ts = purrr::map(.x = input$sample.points,
                                 .f = ~plotfun_ts(.x)
        ) # end map
    }) # end reactive
    
    
    ### slickR TEST -----------------------------------------------------------------------------------------------
    
    ## render list of rad plots for single sample location, multiple individual sample dates
    #plotlist2_rad = reactive({
    #    
    #    pdat = dat1() %>%
    #        filter(`Sample Point Name` %in% input$sample.points) %>%
    #        mutate(sample.instance = paste(`Sample Point Name`, `Sample Date`, sep=': '))
    #    
    #    sample.instances = pdat$sample.instance
    #    
    #    plotlist2_rad = purrr::map(.x = sample.instances,
    #                               .f = ~xmlSVG({plotfun_rad(.x)},
    #                                            standalone = TRUE) # end xmlSVG
    #    ) # end map
    #    
    #    # paste sv and characters nessesary to make svg images... maybe? 
    #    plotlist2_rad = sapply(plotlist2_rad, function(sv){paste0("data:image/svg+xml;utf8,",as.character(sv))}) # end sapply
    #    
    #    return(plotlist2_rad)
    #    
    #}) # end reactive
    #
    ## render new plotlist in slickR
    #output$my_slick = renderSlickR({
    #    imgs = plotlist2_rad
    #    slickR(imgs) # end slickR
    #}) # end renderSlickR
    
    
    
    #### Download plots ----------------------------------------------------------------------------------------
    ##   using ggsave and a user specified directory 
    #observeEvent(input$download,{
    #    if (input$download > 0) {
    #        map(.x = 1:length(plotlist()),
    #            .f = ~ggsave(filename = paste(input$wd,"/",gsub(" ","",site.name()),'_',gsub(" ","",as.character(plotlist()[[.x]]$data[1,1])),'.png',sep=''),
    #                         plot=plotlist()[[.x]], width=10, height=10))
    #    }
    #})
    #
    
    
    
    
} # End server

# Run the application ------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

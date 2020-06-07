library(shinythemes)
library(ggplot2)
library(shiny)
library(plotly)
library(DT)
library(ggmosaic)
library(Hmisc)
library(shinythemes)
ui = navbarPage(theme = shinytheme("flatly"),"ANTENATAL",
                tabPanel("Data", 
                         sidebarPanel(width = 3,
                                      
                                      fileInput("file", "Choose RDS File",
                                                accept = c("rds",".csv","text/csv","text/comma-separated-values,text/plain")),
                                      fileInput("file1", "Choose CSV File",
                                                accept = c("rds",".csv","text/csv","text/comma-separated-values,text/plain",".xlsx")),
                                      
                                      actionButton('Demo', 'Dataset(N)'),
                                      actionButton("print", "PRINT")
                                      #downloadButton("download", "Download")
                                      
                         ),
                         
                         mainPanel(
                           DT::dataTableOutput("table") 
                           
                         )
                ),
                tabPanel("Data cleaning",
                         # sidebarPanel(width = 2,
                         #   actionButton("print", "PRINT")
                         # 
                         # ),
                         mainPanel(width=12,
                                   tabsetPanel(tabPanel( h5("VARIABLE RENAME"),div(style="display:inline-block",uiOutput("colname_in2"),width=3),uiOutput("colname_in3"),div(style="width:500px;",verbatimTextOutput("variablenamechange")),actionButton("RenameColumn","Rename"),tags$hr(style="border-color: black;")),
                                               tabPanel( h5("RECLASSIFYING VARIABLE TYPE"),div(style="display:inline-block",uiOutput("colname_in"),width=3),div(style="display:inline-block",uiOutput("colname_in1"),width=3),verbatimTextOutput("classvari"),actionButton("change_class","Change"),verbatimTextOutput("reclassifyvariable"),tags$hr(style="border-color: black;")),
                                               # tabPanel( h5("RENAME FACTOR LEVELS"),div(style="display:inline-block",uiOutput("colname_in4"),width=3),uiOutput("renamedif"),actionButton("renamecol","OK"),uiOutput("colname_in5"),uiOutput("colname_in6"),actionButton("RenamelevelsColumn","Rename levels"),tags$hr(style="border-color: black;")),    
                                               tabPanel( h5("RECODE CONTINUOUS VARIABLE"),
                                                         fluidRow(
                                                           column(width = 8,
                                                                  div(style="display:inline-block",uiOutput('recodevariable'),width=1),div(style="display:inline-block",uiOutput("renewvari"),width=1),div(style="display:inline-block",actionButton("addvari","Add Variable"),width=1),
                                                                  uiOutput("lowselect"),div(style="display:inline-block",uiOutput('lower'),width=1),div(style="display:inline-block",uiOutput("lowervalue"),width=1),div(style="display:inline-block",actionButton("lowerok","OK"),width=1),
                                                                  uiOutput("lowselect1"),div(style="display:inline-block",uiOutput('between1'),width=1),div(style="display:inline-block",uiOutput('between2'),width=1),div(style="display:inline-block",uiOutput('bewteenvalue'),width=1),div(style="display:inline-block",actionButton("betweenok", "OK"),width=1),
                                                                  verbatimTextOutput('recodesummary1'),div(style="display:inline-block",uiOutput('higher'),width=1),div(style="display:inline-block",uiOutput("highervalue"),width=1),div(style="display:inline-block",actionButton("higherok","OK"),width=1),verbatimTextOutput('recodesummary')),
                                                           # column(width = 4,
                                                           #        h5("CREATE LABEL"),verbatimTextOutput('recodesuary1'),div(style="display:inline-block",uiOutput("colname_in4"),width=3),div(style="display:inline-block",uiOutput("colname_in5"),width=3),div(style="display:inline-block",uiOutput("colname_in6"),width=3),div(style="display:inline-block",actionButton("RenamelevelsColumn","Label"),width=3),verbatimTextOutput("labelsummary")),
                                                         )
                                               ),
                                               tabPanel( h5("RECODE CATEGORICAL VARIABLE"),div(style="display:inline-block",uiOutput("recodeui1"),width=3),uiOutput("recodeui01"),div(style="display:inline-block",actionButton("cateadd","Add Variable"),width=3),uiOutput("recodeui001"),h5("Categorical variable : 'PGE1'='PGE'; 'PGE2'='PGE'"),actionButton("recodeok01","Recode"),verbatimTextOutput('cutsummary'),verbatimTextOutput('cutsummary1')), 
                                               tabPanel( h5("LABEL"),
                                                         fluidRow(
                                                           column(width = 5,
                                                                  div(style="display:inline-block",h4("VARIABLE LABEL"),uiOutput("labelselectui"),width=3),uiOutput("labeltextui"),div(style="display:inline-block",actionButton("labeladding","Add label"),width=3),div(style="width:500px;",verbatimTextOutput('labelsummary1'))),
                                                           column(width = 7,
                                                                  h4("FACTOR LEVELS LABEL"),div(style="display:inline-block",uiOutput("levelslableselectui"),width=3),
                                                                  div(style="display:inline-block",uiOutput("colname_in5"),width=3),uiOutput("colname_in6"),div(style="display:inline-block",actionButton("RenamelevelsColumn","Label"),width=3),verbatimTextOutput("labelsummary2"))
                                                         )), 
                                               tabPanel(h5("OUTLIERS"),uiOutput('select5'),h4("Total number of outliers"),verbatimTextOutput('summary5'),h4("List of outliers"),verbatimTextOutput("outlierslist"),h5("Tukey's Rule method to remove the outliers"),actionButton("remove", "Remove!"),plotlyOutput("plotlier"),tags$hr(style="border-color: black;")),tabPanel( h5("MISSING VALUE"),tabsetPanel(tabPanel(h6("Variable"),uiOutput("miss1"),h4("Total number of missing values and percentage"),verbatimTextOutput("miss3"),h4("Missing values plot"),plotlyOutput("missplot1"))
                                                                                                                                                                                                                                                                                                                                                                                                                # tabPanel(h6("Risk factor variable"),uiOutput("miss2"),h4("Total number of missing values and percentage"),verbatimTextOutput("miss4"),h4("Missing values plot"),plotlyOutput("missplot2",height=600))
                                               ))
                                   )
                         )
                         
                ),
                tabPanel("Univariate",
                         # # sidebarPanel(width = 2,
                         # #   
                         # #   actionButton("print", "PRINT")
                         #         
                         # ),
                         mainPanel(width=12,
                                   
                                   actionButton("print1", "PRINT"),verbatimTextOutput('summary3'),downloadButton('report',label="Download Report")
                                   
                         )
                ),
                tabPanel("Bivariate",
                         sidebarPanel(width = 3,
                                      
                                      uiOutput('select'),
                                      textOutput("classtype1"),
                                      uiOutput('select1'),
                                      textOutput("classtype2")
                                      
                         ),
                         mainPanel(width = 9,
                                   verbatimTextOutput("summary"),verbatimTextOutput('summary1'), verbatimTextOutput('summary2'),
                                   # fluidRow(
                                   #   column(
                                   #          #h5("Outcome variable"),
                                   #          verbatimTextOutput("sum11")),
                                   #   column(
                                   #          #h5("Independent variable"),
                                   #          verbatimTextOutput('summary2')),
                                   # ),
                                   plotlyOutput("plot4"),tags$hr(style="border-color: black;"),plotlyOutput("plot5"),tags$hr(style="border-color: black;"),plotlyOutput("plot6")
                                   
                         )
                ),
                tabPanel("Stratified",
                         mainPanel(width=11,
                                   tabsetPanel(tabPanel("Stratified",
                                                        sidebarPanel(width = 3,
                                                                     
                                                                     uiOutput('select11'),
                                                                     textOutput("classtype3"),
                                                                     uiOutput('select12'),
                                                                     textOutput("classtype4"),
                                                                     uiOutput('select13'),
                                                                     textOutput("classtype5")
                                                                     
                                                        ),
                                                        mainPanel(width=9,
                                                                  verbatimTextOutput("summary4"),verbatimTextOutput("summary7"),plotlyOutput("plot3")
                                                        )
                                   ),
                                   tabPanel("Trend chart",
                                            sidebarPanel(width = 2,
                                                         
                                                         uiOutput('select41'),
                                                         uiOutput('select42'),
                                                         uiOutput('select43')
                                                         
                                            ),
                                            mainPanel(width=10,
                                                      plotlyOutput("trend"),verbatimTextOutput("mean")
                                            )
                                   )
                                   )
                         )
                ),
                # tabPanel("Risk Factor",
                #          sidebarPanel(width = 3,
                #                       uiOutput('select31'),
                #                       uiOutput('select32'),
                #                       uiOutput('select33')
                # 
                #          ),
                #          mainPanel(
                #            verbatimTextOutput("risk1"),
                #            verbatimTextOutput("risk2"),
                #            verbatimTextOutput("risk3"),
                #            verbatimTextOutput("risk4"),
                #            verbatimTextOutput("risk5"),
                #            #verbatimTextOutput("risk6")
                #            plotOutput("riskplot41")
                #          )
                # ),
                tabPanel("Indication",
                         sidebarPanel(width = 3,
                                      uiOutput('select51'),
                                      uiOutput('select52'),
                                      uiOutput('select53')
                                      
                         ),
                         mainPanel(width=8,
                                   
                                   plotlyOutput("indiplot1"),tags$hr(style="border-color: black;"),
                                   plotlyOutput("indiplot2"),tags$hr(style="border-color: black;"),
                                   plotlyOutput("indiplot3"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext1"), plotlyOutput("indiplot4"),verbatimTextOutput("indi1"),textOutput("inditext7"),plotlyOutput("indiplot7"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext8"),plotlyOutput("indiplot8"),textOutput("inditext5"),plotlyOutput("indiplot5"),tags$hr(style="border-color: black;"),
                                   textOutput("inditext9"),plotlyOutput("indiplot9"),textOutput("inditext6"),plotlyOutput("indiplot6")
                         )
                ),
                tabPanel("Associate variable",
                         mainPanel(width=11,
                                   tabsetPanel(tabPanel("FLOW CHART",
                                                        mainPanel(plotOutput("flowchart"))),
                                               
                                               tabPanel("ASSOCIATE VARIABLE",
                                                        sidebarPanel(width = 3, uiOutput('select71'), uiOutput('select72'),uiOutput('select73')
                                                        ),
                                                        mainPanel(width=9,
                                                                  verbatimTextOutput("sss"))
                                               )
                                   ) 
                         )
                ),
                
                tabPanel("Multivariate",
                         sidebarPanel(width = 3,
                                      uiOutput('select21'),
                                      uiOutput('select22'),
                                      uiOutput('select23')
                                      
                         ),
                         mainPanel(
                           verbatimTextOutput("s")
                         )
                ),
                tabPanel("More",
                         
                         mainPanel(width = 12,
                                   tabsetPanel(tabPanel(h6("LABELS & DEFINITIONS"),verbatimTextOutput("labels2")),
                                               tabPanel(h6("REFERENCES"),uiOutput('reference'),actionButton("referencebutton","OK"),verbatimTextOutput("labels3"))
                                               
                                               
                                               
                                   )
                                   
                         )
                ),
                #actionButton("save"),actionButton("download1"),radioButtons("type","Choose file type",choices = c('csv','xls'))
                tabPanel("Download",
                         mainPanel(width = 12,
                                   verbatimTextOutput("saved"), downloadButton("download", "Download-CSV file"),
                                   downloadButton("download1", "Download-RDS file")
                                   
                         )
                )
                
)
server = function(input, output,session) {
  #-----import data set --------------
  v <- reactiveValues()  # store values to be changed by observers
  v$data <- data.frame()
  
  
  # Observer for uploaded file
  observe({
    inFile = input$file
    # if (is.null(inFile)) return(NULL)
    # values$data <- read.csv(inFile$datapath)
    if(is.null(inFile)){
      v$data <-readRDS("anten.samp.death.19.5.rds")
    }  
    else {
      v$data <-readRDS(inFile$datapath)
    }
    
  })
  
  observe({
    inFile2 = input$file1
    # if (is.null(inFile)) return(NULL)
    # values$data <- read.csv(inFile$datapath)
    
    if(is.null(inFile2)){
      return(NULL)
    }  
    else   {
      v$data <-read.csv(inFile2$datapath)
    }
    
  })
  ## Observer for demo data button
  observe({
    if (input$Demo > 0)  # otherwise demo data shows on startup
      v$data <- readRDS("anten.18.05.dealth.rds")
  })
  
  
  #--table output views-------
  output$table <- DT::renderDataTable(
    DT::datatable(v$data, options = list(pageLength = 25))
  )
  # this is Bivariate ui_input functions------------------------------------
  #select variable
  output$select1 <- renderUI({
    df1 <- colnames(v$data)
    selectInput("variable", "Independent variable(df1):",df1,selected = "BABY.SEX")
  })
  #select variable
  output$select <- renderUI({
    df<-colnames(v$data)
    selectInput("variable1", "Outcome variable(df):",df,selected = "BABY.WEIGHT1")
  })
  
  # This is startified analysis ui input function------------------ ----------------------- 
  #select variable
  output$select11 <- renderUI({
    df11<-colnames(v$data)
    selectInput("variable11", "Outcome variable(df):",df11,selected = "BABY.SEX")
  })
  
  #select variable
  output$select12 <- renderUI({
    df12 <- colnames(v$data)
    selectInput("variable12", "Independent variable(df1):",df12,selected = "ONSET.OF.LABOUR")
  })
  #select variable
  output$select13 <- renderUI({
    df13 <- colnames(v$data)
    selectInput("variable13", "Stratified variable(df2):",df13,selected = "BLOOD.LOSS")
  })
  output$classtype3<-renderText({
    
    dfg<-v$data[,input$variable11]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of outcome variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of outcome variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of outcome variable: Continuous")
    }
  })
  output$classtype4<-renderText({
    
    dfg<-v$data[,input$variable12]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of independent variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of independent variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of independent variable: Continuous")
    }
  })
  output$classtype5<-renderText({
    
    dfg<-v$data[,input$variable13]
    nlevels1<-length(levels(dfg))
    if(is.factor(dfg)){
      if( nlevels1==2){
        print("Type of stratified variable: Binary")
      }
      else if (nlevels1 >=3){
        print("Type of stratified variable: Multinomial")
      }
    }
    else if (is.numeric(dfg) | is.integer(dfg)){
      print("Type of stratified variable: Continuous")
    }
  })
  
  #Risk factor--select variable
  output$select31 <- renderUI({
    df31 <- colnames(v$data[32:47])
    selectInput("variable31", "Medical risk factor(df1):",df31)
  })
  # -- Risk factor--select variable2
  output$select32 <- renderUI({
    df32 <- colnames(v$data[48:62])
    selectInput("variable32", "Obstetric risk factor(df2):",df32)
  })
  # --Risk factor--select variable2
  output$select33 <- renderUI({
    df33 <- colnames(v$data)
    selectInput("variable33", "Outcome variable(df3):",df33)
  })
  
  #----indications--select-1-ui---
  output$select51 <- renderUI({
    df51 <- colnames(v$data)[118:137]
    selectInput("variable51", "Induction indication(df1):",df51)
  })
  #----indications--select-2-ui---
  output$select52 <- renderUI({
    df52 <- colnames(v$data)[78:97]
    selectInput("variable52", "Primary indication(df2):",df52)
  })
  #----indications--select-3-ui---
  output$select53 <- renderUI({
    df53 <- colnames(v$data)[98:117]
    selectInput("variable53", "Secondary indication(df3):",df53)
  })
  
  #--outlier variable---------selectui
  output$select5 <- renderUI({
    df5 <- colnames(Filter(is.numeric,v$data))
    selectInput("outliervariable", "choose variable:",df5)
  })
  #---missing select--ui----- 
  # output$miss1<-renderUI({
  #   mi1<-colnames(v$data[1:31])
  #   selectInput("misvariable1","Demograph:",mi1)
  # })
  output$miss1<-renderUI({
    mi1<-colnames(v$data)
    selectInput("misvariable1","Variable:",mi1)
  })
  
  output$miss2<-renderUI({
    mi2<-colnames(v$data[41:77])
    selectInput("misvariable2","Risk factor:",mi2)
  })
  
  # chosse data type
  output$classvari<-renderPrint({
    dfg<-v$data[,input$colname]
    print(class(dfg))
    #print(class([,input$colname]))
    print(input$class)
    
  })
  
  # trend chart select variable--------------
  
  #select variable
  output$select41 <- renderUI({
    
    selectInput(
      inputId = "variable41", 
      label = "outcome Variable(df)",
      choices = c(colnames(v$data)),
      selected = "BABY.WEIGHT1")
  })  
  
  #select variable
  output$select42 <- renderUI({
    df42 <- v$data
    df42<- colnames(df42["YEAR"])
    selectInput("variable42", "time Variable(df):",df42)
  })
  
  #select variable
  output$select43 <- renderUI({
    df43 <- v$data
    df43 <- colnames(df43["unit1"])
    selectInput("variable43", "stratified Variable(df):",df43)
  })
  # associate variable select variable--------------
  output$select71<- renderUI({
    df71<- v$data
    df72<-data.frame(df71$BABY.WEIGHT1,df71$AGE1,df71$MOTHER.HEIGHT1,df71$MOTHER.HEIGHT,df71$PLACENTA.WEIGHT.Gms.1,df71$GEST.WEEKS1)
    names(df72)<-c("BABY.WEIGHT1","AGE1","MOTHER.HEIGHT1","MOTHER.WEIGHT1","PLACENTA.WEIGHT.Gms.1","GEST.WEEKS1")
    df73 <- colnames(df72)
    
    selectInput("variable73", "stratified Variable(df):",df73)
  })
  
  #summary--------------
  output$summary<-renderPrint({
    df<-v$data
    df<- df[,input$variable1]
    #df<- paste(names(df[,input$variable1]))
    # df<- names(antental1[input$variable1])
    #df<- paste(antental1[input$variable])
    if(is.factor(df)){
      # y1<-
      describe(df,paste(input$variable1[1]))
      # y2<-summary(df)
      # g2<-data.frame(y1$counts[1],y1$counts[2],y2[1],y2[2])
      # names(g2)<-c("n","Missing values","F")
      # rownames(g2)<-NULL
      # knitr::kable(g2,caption=paste(input$variable1[1]))
    }else {
      # describe(df,paste(input$variable1[1]))
      y<-describe(df,paste(input$variable1[1]))
      g<-data.frame(length(df),y$counts[2],y$counts[5],median(df,na.rm=T),sd(df,na.rm=T),range(df,na.rm=T)[1],range(df,na.rm=T)[2],y$counts[7],y$counts[8],y$counts[9],y$counts[10],y$counts[11],y$counts[12],y$counts[13])
      names(g)<-c("n","Missing vlaues","Mean","Median","SD","Lower.Range","upper.Range",".05",".10",".25",".50",".75",".90",".95")
      rownames(g)<-NULL
      # kable(g,format="markdown")
      knitr::kable(g,caption=paste(input$variable1[1]))
      #Desc(df,maxrows = NULL, ord = NULL, plotit = F,sep = NULL,paste(input$variable1[1]))
      
    }
  })
  # #summary-------------------------
  output$summary1<-renderPrint({
    df1<- v$data
    df1<- df1[,input$variable]
    if(is.integer(df1)|is.numeric(df1)){
      y<-describe(df1,paste(input$variable[1]))
      g<-data.frame(length(df1),y$counts[2],y$counts[5],median(df1,na.rm=T),sd(df1,na.rm=T),range(df1,na.rm=T)[1],range(df1,na.rm=T)[2],y$counts[7],y$counts[8],y$counts[9],y$counts[10],y$counts[11],y$counts[12],y$counts[13])
      names(g)<-c("n","Missing vlaues","Mean","Median","SD","Lower.Range","upper.Range",".05",".10",".25",".50",".75",".90",".95")
      # kable(g,format="markdown")
      knitr::kable(g,caption=paste(input$variable[1]))
      # Desc(df1,maxrows = NULL, ord = NULL, plotit = F,sep = NULL,paste(input$variable[1]))
    }else {
      # dfSummary(df1)
      # skim(df1,paste(input$variable[1]))
      describe(df1,paste(input$variable[1]))
    }
  })
  #--aggreate---bivariate
  # output$sum11<- renderPrint({
  #  options(digits=3)
  #  df<- v$data
  #  df1<- v$data
  #  df<- df[,input$variable1]
  #  df1<- df1[,input$variable]
  #  y1<-aggregate(r$BABY.WEIGHT1~r$DELIVERY.TYPE,FUN="length")
  #  y2<-aggregate(r$BABY.WEIGHT1~r$DELIVERY.TYPE,FUN="mean")[2]
  #  h<-data.frame(y1,y2)
  #  names(h)<-c("levels","Frequency","Mean")
  #  h
  #  
  # })
  ## #summary-----------------------
  output$summary2<- renderPrint({
    options(digits=3)
    df<- v$data
    df1<- v$data
    df<- df[,input$variable1]
    df1<- df1[,input$variable]
    nlevels<-length(levels(df1))
    nlevels1<-length(levels(df))
    if ((is.integer(df)|is.numeric(df)) & is.factor(df1)){
      if (nlevels == 2){
        t.test(df~df1)
      } 
      else if (nlevels >= 3){
        g3<-aggregate(df~df1,FUN="mean")
        g<-lm(df~df1)
        print(anova(g))
        print(TukeyHSD(aov(df~df1)))
        
      } 
    }
    else if ((is.integer(df)|is.numeric(df)) & (is.integer(df1) | is.numeric(df1))){
      cor(df,df1,use="complete.obs")
    }
    else if (is.factor(df) & is.factor(df1)) {
      tab<-table(df,df1,dnn =c(paste(input$variable1[1]),paste(input$variable[1])))
      tab1<-t(tab)
      print(tab1)
      print(chisq.test(df,df1))
      # r<-oddsratio(tab1, method = "wald")
      # print(r[c(1,2,3)],digits=5)
      
    }
    else if (is.factor(df) & (is.numeric(df1)|is.integer(df1))) {
      if (nlevels1 == 2){
        t.test(df1~df)
      } 
      else if (nlevels1 >= 3){
        g<-lm(df1~df)
        g1<-TukeyHSD(aov(df1~df))
        g2<-data.frame(g1$df)
        g3<-subset(g2,p.adj < 0.05)
        print(anova(g))
        print(g3)
      }
    }
  })  
  #--bivariate tab plots----  
  output$plot6 <- renderPlotly({
    df<-v$data
    df1<-v$data
    df <- df[,input$variable1]
    df1<- df1[,input$variable]  
    if(is.factor(df) & is.factor(df1)){
      #mosaic(~ df1+df, data = v$data)
      an<-data.frame(df,df1)
      
      names(an)<-c("df","df1")
      g<- ggplot(data = an) +
        geom_mosaic(aes(x = product(df), fill = df1), na.rm=TRUE) +
        labs(x='', y=" ",title='mosaic plot') 
      g<- ggplotly(g)
      g
      
    }
    else if (is.factor(df) & (is.integer(df1)|is.numeric(df1))){
      wr1<- data.frame(df,df1)
      wr<-na.omit(wr1)
      names(wr)<-c("df","df1")
      p <- ggplot(wr, aes(x=df, y=df1, fill=df)) + geom_boxplot()
      p <- ggplotly(p)
      p
    }
    else if ((is.integer(df)|is.numeric(df))& (is.integer(df1) | is.numeric(df1))){
      wr<- data.frame(df,df1)
      names(wr)<-c("df","df1")
      plot_ly(wr,x = ~df,y= ~df1,type = "scatter",mode = 'markers')%>%
        layout(title = "scatter plot")
      
    }
    else if ((is.integer(df)|is.numeric(df)) & is.factor(df1)) {
      wr1<- data.frame(df,df1)
      wr<-na.omit(wr1)
      names(wr)<-c("df","df1")
      p <- ggplot(wr, aes(x=df1, y=df, fill=df1)) + geom_boxplot()
      p <- ggplotly(p)
      p
      
    }
  })
  #--stratified tabs--plot----  
  output$plot3<-renderPlotly({
    df11<- v$data
    df11<- df11[,input$variable11]
    df12<- v$data
    df12<- df12[,input$variable12]
    df13<- v$data
    df13<- df13[,input$variable13]
    if(is.factor(df11) & is.factor(df12) & is.factor(df13)){
      w<-data.frame(df11,df12,df13)
      names(w)<-c("df","df1","df2")
      a<- ggplot(data = w) +
        geom_mosaic(aes(x = product(df, df1), fill=df2), na.rm=TRUE) +
        labs(x = "", title='mosaic plot')+theme(axis.text.x = element_text(angle = 55, hjust = 1))
      a<-ggplotly(a)
      a
    }
    else if (is.factor(df11) & is.factor(df12) & (is.integer(df13) | is.numeric(df13)) ){
      
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      
      names(f)<-c("df","df1","df2")
      t<- ggplot(f, aes(x = df, y = df2, fill = df1) ) +
        geom_bar(stat="identity", position = 'dodge') +theme(axis.text.x = element_text(angle = 55, hjust = 1))
      t<-ggplotly(t)
      t
    }
    else if (is.factor(df11) & (is.numeric(df12) | is.integer(df12)) & is.factor(df13)) {
      
      ae<-data.frame(df11,df12,df13)
      f<-na.omit(ae)
      names(f)<-c("df","df1","df2")
      pq<- ggplot(f, aes(df, df1)) +
        geom_boxplot(aes(color = df2 ))+
        facet_wrap(~df2)
      pq<-ggplotly(pq)
      pq
    }
    else if ((is.integer(df11)| is.numeric(df11)) & is.factor(df12) & is.factor(df13)){
      aa<-data.frame(df11,df12,df13)
      f<-na.omit(aa)
      names(f)<-c("df","df1","df2")
      pq<- ggplot(f, aes(df1, df)) +
        geom_boxplot(aes(color = df2 ))+
        facet_wrap(~df2)
      pq<-ggplotly(pq)
      pq
    }
    
    else if ((is.integer(df11) | is.numeric(df11)) & (is.integer(df12) | is.numeric(df12)) & is.factor(df13)){
      e<-data.frame(df11,df12,df13)
      f<-na.omit(e)
      names(f)<-c("df","df1","df2")
      q <- ggplot(f, aes(df, df1)) +
        geom_point(aes(color = df2), size = 3, alpha = 0.6) +
        facet_wrap(~df2)
      q<-ggplotly(q)
      q
      
    }
    else if((is.integer(df11) | is.numeric(df11)) & is.factor(df12) & (is.integer(df13) | is.numeric(df13))){
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      names(f)<-c("df","df1","df2")
      
      p<-plot_ly(f, x = ~df, y = ~interaction(df1, df2)) %>%
        add_boxplot(color = ~df1) %>%
        layout(yaxis = list(title = ""))
      p
    }
    else if (is.factor(df11) & (is.integer(df12) | is.numeric(df12)) & (is.integer(df13) | is.numeric(df13))){
      w<-data.frame(df11,df12,df13)
      f<-na.omit(w)
      names(f)<-c("df","df1","df2")
      
      p<-plot_ly(f, x = ~df2, y = ~interaction(df1, df)) %>%
        add_boxplot(color = ~df) %>%
        layout(yaxis = list(title = ""))
      p
    }
    else {
      print("NULL")
    }
  })
  
  #
  output$plot4<-renderPlotly({
    df<- v$data
    df<- df[,input$variable1]
    if(is.integer(df) |is.numeric(df) ){
      plot_ly(x=df,type="histogram")%>%
        layout(xaxis = list(title = 'df'),yaxis = list(title = 'Frequency'),title = "outcome variable(df)")
      
    }
    else {
      plot_ly(x=names(table(df)),y= as.numeric(table(df)),type="bar")%>%
        layout(xaxis = list(title = 'df'),yaxis = list(title = 'Count'),title = "outcome variable(df)")
    }
  })
  
  output$plot5<-renderPlotly({
    df1<- v$data
    df1<- df1[,input$variable]
    if(is.factor(df1)){
      plot_ly(x=names(table(df1)),y= as.numeric(table(df1)),type="bar")%>%
        layout(xaxis = list(title = 'df1'),yaxis = list(title = 'Count'),title = "independent variable(df1)")
    }
    else {
      plot_ly(x=df1,type="histogram")%>%
        layout(xaxis = list(title = 'df1'),yaxis = list(title = 'Frequency'),title = "independent variable(df1)")
    }
  })
  
} 

shinyApp(ui, server)

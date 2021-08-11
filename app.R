library(shiny)
library(ibdsim2)

# Load data
relData = readRDS("data/relationships.rds")
relnames = unname(sapply(relData$pairwise, function(r) r$name))
sharingData = readRDS("data/sharing.rds")
autozData = readRDS("data/autoz.rds")

COLS = c("#1a8e9c", "#fca106", "2")




ui = fluidPage(

  # Application title
  titlePanel("Relationship classifier based on IBD segments"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
      h3("Observed data", style = "margin-top: 0px"),
      radioButtons("segtype", "Segment type", choices = c("Pairwise sharing" = "Sharing", "Autozygous" = "Autozyg"), selected = "Sharing"),
      numericInput("cutoff", "Cutoff", value = 0, min = 0, step = 1),
      textAreaInput("segText", "Segment lengths", rows = 10),
      actionButton("classify", "Classify", class = "btn btn-primary"),

      hr(style = "border-top: 1px solid gray;"),

      h4("Simulate example"),
      selectInput("simrel", label = "Relationship", selectize = T, width = "100%",
                  choices = relnames),
      actionButton("simulate", "Simulate", class = "btn btn-warning")
    ),

    # Show a plot of the generated distribution
    mainPanel(width = 9, align = "left", # style = "max-width: 800px",
      fluidRow(
        column(4, align = "center", plotOutput("pedplot1", width = "300px", height = "280px")),
        column(4, align = "center", plotOutput("pedplot2", width = "300px", height = "280px")),
        column(4, align = "center", plotOutput("pedplot3", width = "300px", height = "280px"))
      ),
      fluidRow(
        column(3, align = "center", br(), br(), tableOutput("tableExact")),
        column(6, align = "center", plotOutput("distplot", width = "400px", height = "400px")),
        column(3, align = "center", br(), br(),
               tableOutput("tableType"),
               tableOutput("tableCoeff")
        ),
      ),
    )
  )
)



server = function(input, output, session) {

  updateSelectInput(session, "simrel", selected = relnames[4])

# Select relationships and training data ----------------------------------

  rels = reactive(switch(input$segtype,
                         Sharing = relData$pairwise,
                         Autozyg = relData$inbred))

  trainingData = reactive(switch(input$segtype,
                                 Sharing = sharingData,
                                 Autozyg = autozData))

  # Prepare empirical density functions
  pdfuns = reactive(lapply(trainingData(), preparePdfs, cutoff = input$cutoff))


# Input data and classify -------------------------------------------------

  obs = reactive({
    lenStr = req(input$segText) |> strsplit("\n") |> unlist() |>
      strsplit(",") |> unlist() |> trimws()
    lenStr = lenStr[lenStr != ""]
    lens = suppressWarnings(as.numeric(lenStr))
    if(anyNA(lens))
      return(errModal(paste("Non-numeric segment length:", toString(lenStr[is.na(lens)]))))
    lens
  })

  posteriors = reactiveVal(NULL)

  # Reset when segment type or observed values change!
  observeEvent({input$segtype; obs()}, posteriors(NULL))


  # Main button: classify!
  observeEvent(input$classify, {
    post = classify(req(obs()), pdfuns())
    posteriors(post)
  })


# Plots ----------------------------------------------------------

  output$pedplot1 = renderPlot({
    post1 = req(posteriors())[1]
    rel = rels()[[names(post1)]]
    tit = sprintf("1. %s", rel$name)
    tryCatch(
      plotped(rel$ped, ids = rel$ids, title = tit, post = post1, col = COLS[1]),
      error = function(e) {
        plot.new(); box(which = "outer", col = 1); title(tit);
        msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
        mtext(msg, line = 0, col = 2)
      })
  })

  output$pedplot2 = renderPlot({
    post2 = req(posteriors())[2]
    rel = rels()[[names(post2)]]
    tit = sprintf("2. %s", rel$name)
    tryCatch(
      plotped(rel$ped, ids = rel$ids, title = tit, post = post2, col = COLS[2]),
      error = function(e) {
        plot.new(); box(which = "outer", col = 1); title(tit);
        msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
        mtext(msg, line = 0, col = 2)
      })
  })

  output$pedplot3 = renderPlot({
    post3 = req(posteriors())[3]
    rel = rels()[[names(post3)]]
    tit = sprintf("3. %s", rel$name)
    tryCatch(
      plotped(rel$ped, ids = rel$ids, title = tit, post = post3, col = COLS[3]),
      error = function(e) {
        plot.new(); box(which = "outer", col = 1); title(tit);
        msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
        mtext(msg, line = 0, col = 2)
      })
  })

  output$distplot = renderPlot({
    post = req(posteriors())
    top3 = names(post[1:3])
    distPlot(top3, trainingData(), obs(), COLS)
  })


# Table ------------------------------------------------------------

  restable = reactive({
    post = req(posteriors())
    post = post[post > 1e-4]
    showrels = rels()[names(post)]

    res = data.frame(Posterior = as.numeric(post),
                     Relationship = sapply(showrels, '[[', "name"),
                     Type = sapply(showrels, '[[', "group"))

    if(input$segtype == "Autozyg")
      res$Inbreeding = sapply(showrels, function(r) frac2str(r$inbreeding))
    else
      res$Kinship = sapply(showrels, function(r) frac2str(r$kinship))

    res
  })

  output$tableExact = renderTable({
    req(restable())[, c("Relationship", "Posterior"), drop = F] |> head(6)
  }, striped = T)

  output$tableType = renderTable({
    req(restable()) |> aggreg("Posterior", by = "Type") |> head(6)
  }, striped = T)

  output$tableCoeff = renderTable({
    tab = req(restable())
    colname = switch(input$segtype, Sharing = "Kinship", Autozyg = "Inbreeding")
    tab |> aggreg("Posterior", by = colname) |> head(6)
  }, striped = T)


# Simulate example data ---------------------------------------------------

  observeEvent(input$simulate, {
    name = req(input$simrel)
    rel = rels()[[match(name, relnames)]]
    ids = rel$ids
    patt = switch(input$segtype, Sharing = list(carriers = ids), Autozyg = list(autozygous = ids))

    MAP = loadMap("decode19", uniform = TRUE, sexAverage = FALSE)

    segs = ibdsim(rel$ped, ids = rel$ids, N = 1, map = MAP, verbose = FALSE) |>
      findPattern(pattern = patt) |>
      segmentStats(returnAll = TRUE)

    lens = segs$allSegs[segs$allSegs >= input$cutoff] |> round(digits = 2)

    updateTextAreaInput(session, "segText", value = paste(lens, collapse = "\n"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

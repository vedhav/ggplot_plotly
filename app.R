library(shiny)
library(plotly)


ui <- fluidPage(
	title = "ggplot2 and plotly in R | vedha.space",
	tags$head(
		tags$link(rel = "shortcut icon", type = "image/png", href = "vedha_space.png")
	),
	conditionalPanel(
		condition = "$('html').hasClass('shiny-busy')",
		tags$div(
			style = "position: fixed;top: 250px; left: 0px; width: 100%;
			padding: 5px 0px 5px 0px; text-align: center; font-weight: bold;
			font-size: 300%; color: #ffffff; background-color:'transparent'; z-index: 105;",
			tags$img(src = "loading_icon.svg", height = "200px", width = "200px")
		)
	),
	fluidRow(align = "center", tags$h2("Scatter plot")),
	fluidRow(column(6, uiOutput("ggplot_scatter_ui")), column(6, uiOutput("plotly_scatter_ui"))),
	fluidRow(align = "center", tags$h2("Histogram")),
	fluidRow(column(6, uiOutput("ggplot_histogram_ui")), column(6, uiOutput("plotly_histogram_ui"))),
	fluidRow(align = "center", tags$h2("Bar")),
	fluidRow(column(6, uiOutput("ggplot_bar_ui")), column(6, uiOutput("plotly_bar_ui"))),
	fluidRow(align = "center", tags$h2("Bar with color")),
	fluidRow(column(6, uiOutput("ggplot_bar_color_ui")), column(6, uiOutput("plotly_bar_color_ui"))),
	fluidRow(align = "center", tags$h2("Line")),
	fluidRow(column(6, uiOutput("ggplot_line_ui")), column(6, uiOutput("plotly_line_ui"))),
	fluidRow(align = "center", tags$h2("Line with points")),
	fluidRow(column(6, uiOutput("ggplot_line_points_ui")), column(6, uiOutput("plotly_line_points_ui"))),
	fluidRow(align = "center", tags$h2("Line with points and color")),
	fluidRow(column(6, uiOutput("ggplot_line_points_color_ui")), column(6, uiOutput("plotly_line_points_color_ui"))),
	fluidRow(align = "center", tags$h2("Box plot")),
	fluidRow(column(6, uiOutput("ggplot_box_ui")), column(6, uiOutput("plotly_box_ui"))),
	fluidRow(align = "center", tags$h2("Pie")),
	fluidRow(column(6, uiOutput("ggplot_pie_ui")), column(6, uiOutput("plotly_pie_ui"))),
	fluidRow(align = "center", tags$h2("Heatmap")),
	fluidRow(column(6, uiOutput("ggplot_heatmap_ui")), column(6, uiOutput("plotly_heatmap_ui")))
)

server <- function(input, output, session) {
	code_text <- list(
		ggplot_scatter = "ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point()",
		plotly_scatter = "plot_ly(data = iris, x = ~Petal.Length, y = ~Petal.Width, color = ~Species,type = 'scatter', mode = 'markers')",
		ggplot_histogram = "ggplot(iris, aes(x = iris$Petal.Length)) + geom_histogram(binwidth = 1)",
    	plotly_histogram = "plot_ly(data = iris, x = ~Petal.Length, type = 'histogram', xbins = list(start = 1, size = 1))",
    	ggplot_bar = "ggplot(iris, aes(x = Species, y = Petal.Length)) + geom_bar(stat = 'identity')",
    	plotly_bar = "plotData <- iris %>% group_by(Species) %>% summarise(Petal.Length = sum(Petal.Length)); plot_ly(data = plotData, x = ~Species, y = ~Petal.Length, type = 'bar')",
    	ggplot_bar_color = "iris$PetalSize <- ifelse(iris$Petal.Length > 4.3, 'Big', 'Small'); ggplot(iris, aes(x = Species, y = Sepal.Length, fill = PetalSize)) + geom_bar(stat = 'identity')",
    	plotly_bar_color = "iris$PetalSize <- ifelse(iris$Petal.Length > 4.3, 'Big', 'Small'); plotData <- iris %>% group_by(Species, PetalSize) %>% summarise(Sepal.Length = sum(Sepal.Length)); plot_ly(data = plotData, x = ~Species, y = ~Sepal.Length, color = ~PetalSize, type = 'bar' ) %>% layout(barmode = 'stack')",
    	ggplot_line = "ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_line()",
    	plotly_line = "plotData <- iris %>% arrange(Sepal.Length); plot_ly(data = plotData, x = ~Sepal.Length, y = ~Petal.Length, type = 'scatter', mode = 'lines')",
    	ggplot_line_points = "ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_line() + geom_point()",
    	plotly_line_points = "plotData <- iris %>% arrange(Sepal.Length); plot_ly(data = plotData, x = ~Sepal.Length, y = ~Petal.Length, type = 'scatter', mode = 'lines+markers')",
    	ggplot_line_points_color = "ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + geom_line() + geom_point()",
    	plotly_line_points_color = "plotData <- iris %>% arrange(Sepal.Length); plot_ly(data = plotData, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, type = 'scatter', mode = 'lines+markers')",
    	ggplot_box = "ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_boxplot()",
    	plotly_box = "plot_ly(data = iris, x = ~Species, y = ~Sepal.Length, type = 'box')",
    	ggplot_pie = "USPersonalExpenditure <- data.frame('Category' = rownames(USPersonalExpenditure), USPersonalExpenditure); plotData <- USPersonalExpenditure[,c('Category', 'X1960')]; ggplot(plotData, aes(x = '', y = X1960, fill = Category)) + geom_bar(width = 1, stat = 'identity') + coord_polar('y', start=0)",
    	plotly_pie = "USPersonalExpenditure <- data.frame('Category' = rownames(USPersonalExpenditure), USPersonalExpenditure); plotData <- USPersonalExpenditure[,c('Category', 'X1960')]; plot_ly(plotData, labels = ~Category, values = ~X1960, type = 'pie')",
    	ggplot_heatmap = "plotData <- expand.grid(teams = c('Team A', 'Team B', 'Team C', 'Team D'), metrics = c('Metric 1', 'Metric 2', 'Metric 3', 'Metric 4', 'Metric 5')); set.seed(123); plotData$performance <- rnorm(nrow(plotData)); ggplot(data = plotData, aes(x = metrics, y = teams)) + geom_tile(aes(fill = performance))",
    	plotly_heatmap = "plotData <- expand.grid(teams = c('Team A', 'Team B', 'Team C', 'Team D'), metrics = c('Metric 1', 'Metric 2', 'Metric 3', 'Metric 4', 'Metric 5')); set.seed(123); plotData$performance <- rnorm(nrow(plotData)); plot_ly(data = plotData, x = ~metrics, y = ~teams, z = ~performance, type = 'heatmap', colors = colorRamp(c('#132b43', '#55aef3')))"
	)
	output$ggplot_scatter_ui <- renderUI({
		output$ggplot_scatter_code <- renderText(code_text[["ggplot_scatter"]])
		output$ggplot_scatter_plot <- renderPlot(eval(parse(text = code_text[["ggplot_scatter"]])))
		tags$div(verbatimTextOutput("ggplot_scatter_code"), plotOutput("ggplot_scatter_plot"))
	})
	output$plotly_scatter_ui <- renderUI({
		output$plotly_scatter_code <- renderText(code_text[["plotly_scatter"]])
		output$plotly_scatter_plot <- renderPlotly(eval(parse(text = code_text[["plotly_scatter"]])))
		tags$div(verbatimTextOutput("plotly_scatter_code"), plotlyOutput("plotly_scatter_plot"))
	})
	output$ggplot_histogram_ui <- renderUI({
		output$ggplot_histogram_code <- renderText(code_text[["ggplot_histogram"]])
		output$ggplot_histogram_plot <- renderPlot(eval(parse(text = code_text[["ggplot_histogram"]])))
		tags$div(verbatimTextOutput("ggplot_histogram_code"), plotOutput("ggplot_histogram_plot"))
	})
	output$plotly_histogram_ui <- renderUI({
		output$plotly_histogram_code <- renderText(code_text[["plotly_histogram"]])
		output$plotly_histogram_plot <- renderPlotly(eval(parse(text = code_text[["plotly_histogram"]])))
		tags$div(verbatimTextOutput("plotly_histogram_code"), plotlyOutput("plotly_histogram_plot"))
	})
	output$ggplot_bar_ui <- renderUI({
		output$ggplot_bar_code <- renderText(code_text[["ggplot_bar"]])
		output$ggplot_bar_plot <- renderPlot(eval(parse(text = code_text[["ggplot_bar"]])))
		tags$div(verbatimTextOutput("ggplot_bar_code"), plotOutput("ggplot_bar_plot"))
	})
	output$plotly_bar_ui <- renderUI({
		output$plotly_bar_code <- renderText(code_text[["plotly_bar"]])
		output$plotly_bar_plot <- renderPlotly(eval(parse(text = code_text[["plotly_bar"]])))
		tags$div(verbatimTextOutput("plotly_bar_code"), plotlyOutput("plotly_bar_plot"))
	})
	output$ggplot_bar_color_ui <- renderUI({
		output$ggplot_bar_color_code <- renderText(code_text[["ggplot_bar_color"]])
		output$ggplot_bar_color_plot <- renderPlot(eval(parse(text = code_text[["ggplot_bar_color"]])))
		tags$div(verbatimTextOutput("ggplot_bar_color_code"), plotOutput("ggplot_bar_color_plot"))
	})
	output$plotly_bar_color_ui <- renderUI({
		output$plotly_bar_color_code <- renderText(code_text[["plotly_bar_color"]])
		output$plotly_bar_color_plot <- renderPlotly(eval(parse(text = code_text[["plotly_bar_color"]])))
		tags$div(verbatimTextOutput("plotly_bar_color_code"), plotlyOutput("plotly_bar_color_plot"))
	})
	output$ggplot_line_ui <- renderUI({
		output$ggplot_line_code <- renderText(code_text[["ggplot_line"]])
		output$ggplot_line_plot <- renderPlot(eval(parse(text = code_text[["ggplot_line"]])))
		tags$div(verbatimTextOutput("ggplot_line_code"), plotOutput("ggplot_line_plot"))
	})
	output$plotly_line_ui <- renderUI({
		output$plotly_line_code <- renderText(code_text[["plotly_line"]])
		output$plotly_line_plot <- renderPlotly(eval(parse(text = code_text[["plotly_line"]])))
		tags$div(verbatimTextOutput("plotly_line_code"), plotlyOutput("plotly_line_plot"))
	})
	output$ggplot_line_points_ui <- renderUI({
		output$ggplot_line_points_code <- renderText(code_text[["ggplot_line_points"]])
		output$ggplot_line_points_plot <- renderPlot(eval(parse(text = code_text[["ggplot_line_points"]])))
		tags$div(verbatimTextOutput("ggplot_line_points_code"), plotOutput("ggplot_line_points_plot"))
	})
	output$plotly_line_points_ui <- renderUI({
		output$plotly_line_points_code <- renderText(code_text[["plotly_line_points"]])
		output$plotly_line_points_plot <- renderPlotly(eval(parse(text = code_text[["plotly_line_points"]])))
		tags$div(verbatimTextOutput("plotly_line_points_code"), plotlyOutput("plotly_line_points_plot"))
	})
	output$ggplot_line_points_color_ui <- renderUI({
		output$ggplot_line_points_color_code <- renderText(code_text[["ggplot_line_points_color"]])
		output$ggplot_line_points_color_plot <- renderPlot(eval(parse(text = code_text[["ggplot_line_points_color"]])))
		tags$div(verbatimTextOutput("ggplot_line_points_color_code"), plotOutput("ggplot_line_points_color_plot"))
	})
	output$plotly_line_points_color_ui <- renderUI({
		output$plotly_line_points_color_code <- renderText(code_text[["plotly_line_points_color"]])
		output$plotly_line_points_color_plot <- renderPlotly(eval(parse(text = code_text[["plotly_line_points_color"]])))
		tags$div(verbatimTextOutput("plotly_line_points_color_code"), plotlyOutput("plotly_line_points_color_plot"))
	})
	output$ggplot_box_ui <- renderUI({
		output$ggplot_box_code <- renderText(code_text[["ggplot_box"]])
		output$ggplot_box_plot <- renderPlot(eval(parse(text = code_text[["ggplot_box"]])))
		tags$div(verbatimTextOutput("ggplot_box_code"), plotOutput("ggplot_box_plot"))
	})
	output$plotly_box_ui <- renderUI({
		output$plotly_box_code <- renderText(code_text[["plotly_box"]])
		output$plotly_box_plot <- renderPlotly(eval(parse(text = code_text[["plotly_box"]])))
		tags$div(verbatimTextOutput("plotly_box_code"), plotlyOutput("plotly_box_plot"))
	})
	output$ggplot_pie_ui <- renderUI({
		output$ggplot_pie_code <- renderText(code_text[["ggplot_pie"]])
		output$ggplot_pie_plot <- renderPlot(eval(parse(text = code_text[["ggplot_pie"]])))
		tags$div(verbatimTextOutput("ggplot_pie_code"), plotOutput("ggplot_pie_plot"))
	})
	output$plotly_pie_ui <- renderUI({
		output$plotly_pie_code <- renderText(code_text[["plotly_pie"]])
		output$plotly_pie_plot <- renderPlotly(eval(parse(text = code_text[["plotly_pie"]])))
		tags$div(verbatimTextOutput("plotly_pie_code"), plotlyOutput("plotly_pie_plot"))
	})
	output$ggplot_heatmap_ui <- renderUI({
		output$ggplot_heatmap_code <- renderText(code_text[["ggplot_heatmap"]])
		output$ggplot_heatmap_plot <- renderPlot(eval(parse(text = code_text[["ggplot_heatmap"]])))
		tags$div(verbatimTextOutput("ggplot_heatmap_code"), plotOutput("ggplot_heatmap_plot"))
	})
	output$plotly_heatmap_ui <- renderUI({
		output$plotly_heatmap_code <- renderText(code_text[["plotly_heatmap"]])
		output$plotly_heatmap_plot <- renderPlotly(eval(parse(text = code_text[["plotly_heatmap"]])))
		tags$div(verbatimTextOutput("plotly_heatmap_code"), plotlyOutput("plotly_heatmap_plot"))
	})
}

shinyApp(ui, server)
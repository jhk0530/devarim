library(shiny)
library(reactable)
library(dplyr)
library(networkD3)
library(shinymaterial)

ui <- material_page(
  title = "money flow generator",
  primary_theme_color = "#3f51b5",
  tags$br(),
  material_row(
    material_column(
      width = 10,
      offset = 1,
      material_card(
        title = "nodes",
        divider = TRUE,
        ## Nodes
        material_text_box(
          input_id = "label",
          label = "label",
          color = '#3f51b5'
        ),
        material_button(
          "addNode",
          "add node",
          color = "indigo",
          icon = "add"
        ),
        reactableOutput("table")
      )
    )
  ),
  tags$br(),
  material_row(
    material_column(
      width = 10,
      offset = 1,
      material_card(
        title = "flows",
        material_row(
          material_column(
            width = 3,
            uiOutput("fromUI")
          ),
          material_column(
            width = 3,
            uiOutput("toUI")
          ),
          material_column(
            width = 3,
            material_radio_button(
              input_id = "substance",
              label = "substance",
              choices = c("gain", "loss"),
              color = '#3f51b5'
            )
          ),
          material_column(
            width = 3,
            material_number_box(
              input_id = "quantity",
              label = "quantity",
              color = '#3f51b5',
              min_value = 0,
              max_value = 1000,
              initial_value = 10
            )
          )
        ),
        material_button(
          input_id = "addFlow",
          label = "add flow",
          icon = "add",
          depth = 2,
          color = "indigo"
        ),
        reactableOutput("table2")
      )
    )
  ),
  tags$br(),
  material_row(
    material_column(
      width = 10,
      offset = 1,
      material_card(
        title = "charts",
        ## CHARTS
        material_row(
          material_column(
            width = 3,
            # https://www.jamgd.com/blog/design/color-matters
            material_dropdown(
              input_id = 'gainColor',
              label = 'gain Color',
              choices = c(
                "red" = '#f44336',
                'orange' = '#ff9800',
                'yellow' = '#ffeb3b',
                'green' = '#4caf50',
                'blue' = '#2196f3',
                'purple' = '9c27b0',
                'brown' = '#795548',
                'black' = '#000000'
              ),
              selected = '#4caf50',
              color = '#000'
            )
          ),
          material_column(
            width = 3,
            material_dropdown(
              input_id = 'lossColor',
              label = 'loss Color',
              choices = c(
                "red" = '#f44336',
                'orange' = '#ff9800',
                'yellow' = '#ffeb3b',
                'green' = '#4caf50',
                'blue' = '#2196f3',
                'purple' = '9c27b0',
                'brown' = '#795548',
                'black' = '#000000'
              ),
              selected = '#f44336',
              color = '#000'
            )
          ),
          material_column(
            width = 3,
            material_slider(
              input_id = 'fontSize',
              label = 'font size',
              initial_value = 15,
              min_value = 5,
              max_value = 30,
              step_size = 5,
              color = 'indigo'
            )
          )

        ),
        ## RESULTS
        sankeyNetworkOutput(outputId = "sankey",height = '400px'),
        tags$br(),
        material_button(
          input_id = "draw",
          label = "draw",
          icon = "arrow_forward",
          color = "indigo"
        )
      )
    )
  )
)

server <- function(input, output, session) {

  nodes <- reactiveVal(
    data.frame(
      name = character(0)
    )
  )

  flows <- reactiveVal(
    data.frame(
      from = integer(0),
      to = integer(0),
      quantity = numeric(0),
      substance = character(0)
    )
  )

  addNodes <- function(nodes, label) {
    n <- nodes()
    n[nrow(n)+1, ] <- label
    return(n)
  }

  addFlows <- function(flows, from, to, substance, quantity) {
    f <- flows()
    f <- rbind(f,
               data.frame(from = from, to = to, quantity = quantity, substance = substance)
    )
    return(f)
  }

  observeEvent(input$addNode, {
    if (input$addNode == 0) {
      return(NULL)
    }

    nodes (
      nodes %>%
        addNodes( label = input$label )
    )
  })

  output$fromUI <- renderUI({
    selectInput(
      inputId = "from",
      label = "from",
      choices = nodes()$name,
      selected = character(0)
    )
  })

  output$table <- renderReactable({
    if (nrow(nodes()) != 0) {
      reactable(nodes())
    } else {
      NULL
    }
  })

  observeEvent(input$from, {
    output$toUI <- renderUI({
      selectInput(
        inputId = "to",
        label = "to",
        choices = setdiff(nodes()$name, input$from)
      )
    })
  })

  observeEvent(input$addFlow, {
    if (input$addFlow == 0) {
      return(NULL)
    }
    flows(
      flows %>%
        addFlows(
          from = which(nodes()$name == input$from)-1,
          to = which(nodes()$name == input$to)-1,
          substance = input$substance,
          quantity = input$quantity
        )
    )
  })

  output$table2 <- renderReactable({
    if (nrow(flows()) != 0) {
      reactable(flows())
    } else {
      NULL
    }
  })

  observeEvent(input$draw, {
    if (input$draw == 0) {
      return(NULL)
    }

    my_color <- paste0(
      'd3.scaleOrdinal(). domain(["gain", "loss"]) .range([',
      '"',input$gainColor, '",', '"',input$lossColor, '"','])'
    )
    s <- sankeyNetwork(
      Nodes = nodes(),
      NodeID = 'name',
      Links = flows(),
      colourScale = my_color,
      Source = 'from',
      Target = 'to',
      Value = 'quantity',
      LinkGroup = 'substance',
      NodeGroup = NULL,
      fontSize = input$fontSize
    )

    output$sankey <- renderSankeyNetwork({
      s

      htmlwidgets::onRender(s,'function(el, x) {
        var sankey = this.sankey;
        var path = sankey.link();
        var nodes = d3.selectAll(".node");
        var link = d3.selectAll(".link")
        var width = el.getBoundingClientRect().width - 40;
        var height = el.getBoundingClientRect().height - 40;

        var nodeWidth = sankey.nodeWidth();
        var links = sankey.links();

        links.forEach((d, i) => {
            var startX = d.source.x + nodeWidth;
            var endX = d.target.x;

            var startY = d.source.y + d.sy + d.dy / 2;
            var endY = d.target.y + d.ty + d.dy / 2;

            d3.select(el).select("svg g")
              .append("text")
              .attr("class","value")
              .attr("text-anchor", "middle")
              .attr("alignment-baseline", "middle")
              .attr("x", startX + ((endX - startX) / 2))
              .attr("y", startY + ((endY - startY) / 2))
              .text(d.value);
          })

        window.dragmove = function(d) {
          d3.select(this).attr("transform",
            "translate(" + (
               d.x = Math.max(0, Math.min(width - d.dx, d3.event.x))
                ) + "," + (
                d.y = Math.max(0, Math.min(height - d.dy, d3.event.y))
              ) + ")");
          sankey.relayout();
          link.attr("d", path);

          d3.selectAll("text.value").remove();

          links.forEach((d, i) => {
            var startX = d.source.x + nodeWidth;
            var endX = d.target.x;

            var startY = d.source.y + d.sy + d.dy / 2;
            var endY = d.target.y + d.ty + d.dy / 2;

            d3.select(el).select("svg g")
              .append("text")
              .attr("class","value")
              .attr("text-anchor", "middle")
              .attr("alignment-baseline", "middle")
              .attr("x", startX + ((endX - startX) / 2))
              .attr("y", startY + ((endY - startY) / 2))
              .text(d.value);
          })
        };

        nodes.call(
          d3.drag()
          .subject(function(d) { return d; })
          .on("start", function() { this.parentNode.appendChild(this); })
          .on("drag", dragmove)
        );
      }')

    })

  })
}

shinyApp(ui, server)

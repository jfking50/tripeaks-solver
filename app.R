library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggimage)
library(dplyr)
library(Cairo)
options(shiny.usecairo=TRUE)

convert_input <- function(input){
    v <- tolower(unlist(strsplit(input, " ")))
    v[v=="a"] <- 1
    v[v=="j"] <- 11
    v[v=="q"] <- 12
    v[v=="k"] <- 13
    v <- as.numeric(v)
    v
}

get_viz_idx <- function(bd){
    remain <- bd != 0
    r1 <- which(remain[1:10])
    r2 <- c()
    r3 <- c()
    r4 <- c()
    r2_filter <- remain[1:10]
    for (i in 1:(length(r2_filter)-1)){
        if(!r2_filter[i] & !r2_filter[i+1] & remain[i+10] != 0){r2 <- c(r2, i+10)}
    }
    r3_filter <- remain[11:19]
    for (i in 1:(length(r3_filter)-1)){
        if(!r3_filter[i] & !r3_filter[i+1] & remain[i+19] != 0 & i <= 2){r3 <- c(r3, i+19)}
        if(i == 3 | i == 6) next
        if(!r3_filter[i] & !r3_filter[i+1] & remain[i+18] != 0 & (i == 4 | i == 5)){r3 <- c(r3, i+18)}
        if(!r3_filter[i] & !r3_filter[i+1] & remain[i+17] != 0 & i > 6){r3 <- c(r3, i+17)}
    }
    r4_filter <- remain[20:25]
    for (i in c(1,3,5)){
        if(!r4_filter[i] & !r4_filter[i+1] & remain[i+25] != 0 & i == 1){r4 <- c(r4, i+25)}
        if(!r4_filter[i] & !r4_filter[i+1] & remain[i+24] != 0 & i == 3){r4 <- c(r4, i+24)}
        if(!r4_filter[i] & !r4_filter[i+1] & remain[i+23] != 0 & i == 5){r4 <- c(r4, i+23)}
    }
    viz <- c(r1, r2, r3, r4)
    viz
}

solve_it <- function(board_orig, stock){
    board <- board_orig
    count <- 0

    while (sum(board==0) != 28 & count <= 1000){

        board <- board_orig
        history <- c() # 0 if draw from stock, else index of board played
        idx_played <- c()
        stock_count <- 1
        card <- stock[stock_count]

        while(stock_count <= 24){
            if(is.null(idx_played)){viz_idx <- 1:10}else{viz_idx <- get_viz_idx(board)}

            # possible next indexes
            if (card == 1) poss_next <- viz_idx[c(which(board[viz_idx] == card + 12), which(board[viz_idx] == card + 1))]
            if (card == 13) poss_next <- viz_idx[c(which(board[viz_idx] == card - 12), which(board[viz_idx] == card - 1))]
            if (card > 1 & card < 13) poss_next <- viz_idx[c(which(board[viz_idx] == card - 1), which(board[viz_idx] == card + 1))]

            # if no available card to remove or randomly choose not to clear it,
            # then advance to next stock card
            if (length(poss_next) == 0 | runif(1) > 0.99){
                stock_count <- stock_count + 1
                history <- c(history, 0)
                card <- stock[stock_count]
            }else{
                next_idx <- ifelse(length(poss_next) == 1, poss_next, sample(poss_next, 1))
                idx_played <- c(idx_played, next_idx)
                history <- c(history, next_idx)
                card <- board[next_idx]
                board[next_idx] <- 0
            }
        }
        count <- count + 1
    }

    history
}

ui <- dashboardPage(

    dashboardHeader(title = "Tripeaks Solver"),

    dashboardSidebar(
        sidebarMenu(
            p("Note: there is no error checking!"),
            p("If no solution is found after 1000"),
            p("attempts, the algorithm will stop,"),
            p("and the board may be unsolvable."),
            a(href="https://github.com/jfking50/tripeaks-solver", "Link to the GitHub repo")
        )
    ),

    dashboardBody(
        fluidRow(
            box(
                title = "Inputs", background = "black", width = 8,
                h3("Tripeaks Board Cards"),
                "Starting at the lower left of the board, enter card ranks separated by a space from left to right and bottom to top.
                Ignore the suits. Example: a 10 3 k 3 ...",
                textInput("board", label=NULL),
                h3("Stock Cards"),
                "Starting at the top of the stock, enter card ranks separated by a space from top to bottom.",
                textInput("stock", label=NULL),
                submitButton("Solve", icon=icon("check-square"))
            )
        ),
        fluidRow(
            box(
                title = "Solution",
                plotOutput("plot"), width = 8)
        )
    )
)

server <- function(input, output) {

    board_orig <- reactive({
        validate(
            need(input$board != "", "Please input cards for the board.")
        )
        b <- convert_input(input$board)
        b
    })

    stock <- reactive({
        validate(
            need(input$stock != "", "Please input cards for the stock.")
            )
        s <- convert_input(input$stock)
        s
    })

    output$plot <-
        renderPlot({

            history <- solve_it(board_orig(), stock())

            df <- tibble(
                card = c(stock(), board_orig()),
                pile = c(rep("stock", length(stock())), rep("board", length(board_orig()))),
                index = c(1:24, 1:28)
            )

            h <- tibble(
                index = history,
                order = 1:52,
                pile = ifelse(index == 0, "stock", "board")
            )

            h[h$index==0, "index"] <- 1:sum(h$index==0)

            df <- df |> left_join(h, by = c("pile", "index"))

            df <-
                df |> mutate(
                    img = case_when(
                        card == 1 ~ "cards/ace_of_spades.png",
                        card == 2 ~ "cards/2_of_spades.png",
                        card == 3 ~ "cards/3_of_spades.png",
                        card == 4 ~ "cards/4_of_spades.png",
                        card == 5 ~ "cards/5_of_spades.png",
                        card == 6 ~ "cards/6_of_spades.png",
                        card == 7 ~ "cards/7_of_spades.png",
                        card == 8 ~ "cards/8_of_spades.png",
                        card == 9 ~ "cards/9_of_spades.png",
                        card == 10 ~ "cards/10_of_spades.png",
                        card == 11 ~ "cards/jack_of_spades.png",
                        card == 12 ~ "cards/queen_of_spades.png",
                        card == 13 ~ "cards/king_of_spades.png"
                    )
                )

            df <-
                df |> mutate(
                    x = c(24:1, 1:10*1.1, (1:9+0.5)*1.1, c(2,3,5,6,8,9)*1.1, c(2.5, 5.5, 8.5)*1.1),
                    y = c(rep(0.98, 24),
                          rep(1.02, 10),
                          rep(1.04, 9),
                          rep(1.06, 6),
                          rep(1.08, 3))
                )

            df <- df |> arrange(order)

            ggplot(df) +
                geom_image(aes(x=x, y=y, image=img), size = 0.038, by = "width", asp = 2) +
                geom_label(aes(x=x, y=y, label=order), color="red", fontface="bold", label.size=1) +
                theme_void() +
                ylim(0.96, 1.12) +
                theme(aspect.ratio = 1/2,
                      panel.background = element_rect(fill = 'forestgreen', color = 'forestgreen'))
        })
}

shinyApp(ui, server)

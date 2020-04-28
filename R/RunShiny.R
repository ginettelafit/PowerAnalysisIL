# wrapper for shiny::shinyApp()
RunShiny = function() {
  shinyApp(ui = ui, server = server)
}

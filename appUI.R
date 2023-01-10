ui <- dashboardPage(
  dashboardHeader(title = "Bullpen @ Boshamer V2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "Home", text = "Homepage", icon = icon("house")),
      menuItem(tabName = "Notes", text = "Notes", icon = icon("pencil")),
      menuItem(tabName = "SVG", text = "Sessions vs. Games", icon = icon("film")),
      menuItem(tabName = "Metrics", text = "Metrics", icon = icon("arrow-up")
      # menuItem(tabName = "Perf", text = "Performance Models", icon = icon("gear"),
      #          menuSubItem("Whiff/Called-Strike Model",
      #                      tabName = "WhiffCS",icon = icon("arrow-right")),
      #          menuSubItem("Pitch-Class Model", tabName = "PClass",
      #                      icon = icon("magnifying-glass")))
    ))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Home",
        homepageUI("homepage")
      ),
      tabItem(
        tabName = "Notes",
        notesUI("notes")
      ),
      tabItem(
        tabName = "SVG",
        svgUI("sessionvsgames")
      ),
      tabItem(
        tabName = "Metrics",
        metricsUI("metrics")
      )
    )
  )
)
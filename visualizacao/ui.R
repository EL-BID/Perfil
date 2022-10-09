ui <- navbarPage(
  theme = shinytheme("simplex"),
  collapsible = TRUE,
  windowTitle = "PMV - Perfil da Dívida Ativa (IPTU)",
  title = "Perfil",
  # selected = "Análise 1",
  
  header = tagList(
    # Painel "carregando.." (para ocultar conteúdo enquanto carrega)
    conditionalPanel(
      "output.carregando != ''",
      absolutePanel(
        top = 41,
        left = 0,
        right = 0,
        bottom = 0,
        style = "
          background-color: #FAFAFA;
          text-align: center;
          z-index: 100000;
        ",
        img(
          # src = "hug.gif",
          # height = "50px",
          # style = "
          #   position: fixed;
          #   top: calc(50vh - 25px);
          #   left: calc(50vw - 25px);
          # "
          src = "spinner.gif",
          style = "
            position: fixed;
            top: calc(50vh - 5px);
            left: calc(50vw - 8px);
          "
        )
      )
    )
  ),
    
  tabPanel(
    "Indicadores",
    tags$style(type = "text/css", "#map {z-index: 1; background: #FAFAFA}"),
    tags$style(type = "text/css", ".container-fluid {padding-left:0px;padding-right:0px;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
    tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),
    leafletOutput("map", width = "100%", height = "calc(100vh - 41px)"),

    # Painel de controle
    absolutePanel(
      id = "inputs_panel",
      top = 46,
      left = 10,
      width = 400,
      draggable = TRUE,
      style = "z-index: 5000;
      font-size: 14px;
      padding: 15px 10px 0px 10px;
      background-color: rgba(0,0,0,0.1);",
      selectInput(
        "Indicadores",
        label = NULL,
        width = 400,
        choices = indicadores
      )
    ),

    # Tabela de dados
    conditionalPanel(
      "output.mostrar_tabela != -1",
      absolutePanel(
        width = 400,
        # height = 200,
        top = 125,
        left = 10,
        draggable = TRUE,
        style = "
          z-index: 4000;
          background-color: #fff;
          border-bottom-left-radius: 4px;
          border-bottom-right-radius: 4px;
          border-top-left-radius: 4px;
          border-top-right-radius: 4px;
          border: 2px solid rgba(0,0,0,0.2);
        ",
        tags$table(
          width = "100%",
          style = "
              color: black;
            ",
          tags$tr(
            tags$td(
              style = "
                  text-align: center;
                  color: #000;
                  font-size: 18px;
                ",
              colspan = "2",
              "Região: ",
              textOutput("titulo", inline = TRUE),
              actionLink(
                "fechar_tabela",
                label = NULL,
                style = "
                    vertical-align: text-top;
                    font-size: 14px;
                    color: black;
                  ",
                icon = icon("times", verify_fa = FALSE)
              ) |> absolutePanel(top = 0, right = 5)
            )
          ),
          tags$tr(tags$td(colspan = 2,
                          tags$hr(style = "
                            margin-top: 0;
                            margin-bottom: 0;"))),
          tabela
        )
      )
    ),

    # Botões de ferramentas (clique do mouse)
    conditionalPanel(
      "output.ferramenta == 'select'",
      absolutePanel(
        top = 293,
        style = btn_pressionado
      )
    ),
    absolutePanel(
      top = 293,
      style = btn_normal
    ),
    absolutePanel(
      top = 293,
      style = btn_rotulo,
      actionLink(
        "select",
        label = NULL,
        icon("check")
      )
    ),

    conditionalPanel(
      "output.ferramenta == 'multi_select'",
      absolutePanel(
        top = 337,
        style = btn_pressionado
      )
    ),
    absolutePanel(
      top = 337,
      style = btn_normal
    ),
    absolutePanel(
      top = 337,
      style = btn_rotulo,
      actionLink(
        "multi_select",
        label = NULL,
        icon("check-double")
      )
    ),

    conditionalPanel(
      "output.ferramenta == 'deselect'",
      absolutePanel(
        top = 381,
        style = btn_pressionado
      )
    ),
    absolutePanel(
      top = 381,
      style = btn_normal
    ),
    absolutePanel(
      top = 381,
      style = btn_rotulo,
      actionLink(
        "deselect",
        label = NULL,
        icon("eye-slash")
      )
    ),

    conditionalPanel(
      "output.mostrar_tabela == 1",
      absolutePanel(
        top = 425,
        style = btn_pressionado
      )
    ),
    absolutePanel(
      top = 425,
      style = btn_normal
    ),
    absolutePanel(
      top = 425,
      style = btn_rotulo,
      actionLink(
        "btn_tabela",
        label = NULL,
        icon("id-card-o")
        # , lib = "glyphicon"
      )
    )
  ),

  tabPanel(
    "Análise 1",
    tags$iframe(
      src = "/Perfil/analise1.html",
      width = "100%",
      style = "border:none; height: calc(100vh - 50px);")
),
  
  tabPanel(
    "Análise 2",
    tags$iframe(
      src = "/Perfil/analise2.html",
      width = "100%",
      style = "border:none; height: calc(100vh - 50px);")
  )
)


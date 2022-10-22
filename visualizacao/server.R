function(input, output, session) {
  
  # Mapa base ####
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        zoomSnap = 0.1,
        zoomDelta = 0.1,
        boxZoom = TRUE,
        doubleClickZoom = FALSE,
        minZoom = 10,
        maxZoom = 20,
        worldCopyJump = FALSE,
        preferCanvas = TRUE
      )) |>
      onRender(
        "function(el, x){L.control.zoom({ position: 'topright' }).addTo(this)}"        
      ) |>
      # Determina a superposição dos elementos do mapa
      addMapPane("poligonos", zIndex = 450) |>
      addMapPane("poligonos_selecao", zIndex = 451) |>
      addMapPane("rotulos", zIndex = 452) |>
      # Mapa de fundo
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        group = "Mapa"
      ) |>
      # Rótulos
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        group = "Rótulos",
        options = providerTileOptions(pane = "rotulos")
      ) |>
      # Domicílios georreferenciados
      addCircles(
        data = domicilios,
        group = "Domicílios",
        weight = 1,
        radius = .75,
        fillOpacity = 1,
        color = "#999",
        stroke = FALSE
      ) |>
      addLayersControl(
        baseGroups = c(
          "Setores",
          "Bairros"
        ),
        overlayGroups = c(
          "Mapa", 
          "Rótulos",
          "Domicílios"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) |>
      # hideGroup("Mapa") |>
      # hideGroup("Rótulos") |>
      # hideGroup("Domicílios") |>
      # Define zoom inicial
      fitBounds(sp::bbox(bairros)[1,1],
                sp::bbox(bairros)[2,1],
                sp::bbox(bairros)[1,2],
                sp::bbox(bairros)[2,2])
  })

  # Dados ####
  # seleciona os dados dos setores que serão mostrados no mapa, levando em 
  # consideração:
  # 1) a escolha do indicador
  # 2) os shapes que foram excluídos
  dados_setores <- reactive({
    dados_setores <- setores[,input$Indicadores]
    filtro_setores <- setores$Name %in% exclusao()
    dados_setores@data[filtro_setores,] <- NA
    dados_setores
  })

  dados_bairros <- reactive({
    dados_bairros <- bairros[,input$Indicadores]
    filtro_bairros <- bairros$Name %in% exclusao()
    dados_bairros@data[filtro_bairros,] <- NA
    dados_bairros
  })
  
  # paletas ####
  # cria a paleta de cores conforme os dados selecionados
  pal_bairros <- reactive({
    paleta(dados_bairros()@data[,1])
  })

  pal_setores <- reactive({
    paleta(dados_setores()@data[,1])
  })
  

  # Plota polígonos ####
  # plota os polígonos no mapa, separados nos grupos "Setores" e "Bairros"
  observe({
    proxy <- leafletProxy("map")
    proxy   |>
      addPolygons(
        data = setores,
        layerId = setores@data$Name,
        fillColor = ~pal_setores()(dados_setores()@data[,1]),
        label = dados_setores()@data[,1] |> formatar_lista(input$Indicadores),
        fillOpacity = 0.6,
        options = pathOptions(pane = "poligonos"),
        group = "Setores",
        color = "#666",
        weight = 1,
        highlightOptions = highlightOptions(
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE)) |>
      addPolygons(
        data = bairros,
        layerId = bairros@data$Name,
        fillColor = ~pal_bairros()(dados_bairros()@data[,1]),
        label = dados_bairros()@data[,1] |> formatar_lista(input$Indicadores),
        fillOpacity = 0.6,
        options = pathOptions(pane = "poligonos"),
        group = "Bairros",
        color = "#666",
        weight = 1,
        highlightOptions = highlightOptions(
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE))
  })
  
  # Alterações ####
  # Trata as as alterações nas camadas e nos dados
  observe({
    proxy <- leafletProxy("map")
    proxy |> clearControls()
    if ("Setores" %in% input$map_groups) {
      grupo("Setores")
      proxy |>
          addLegend("bottomright",
                    values = dados_setores()@data[,1],
                    pal = pal_setores())
    } else {
      grupo("Bairros")
      proxy |>
        addLegend("bottomright",
                  values = dados_bairros()@data[,1],
                  pal = pal_bairros())
    }
  })

  # Ferramentas####
  exclusao <- reactiveVal(NA)
  selecao <- reactiveVal("0") # lista de shapes selecionadas no mapa
  ferramenta <- reactiveVal("select")
  
  # variável para alterar o background do botão selecionado
  output$ferramenta <- reactive(ferramenta())
  outputOptions(output,"ferramenta", suspendWhenHidden = FALSE)
  
  # Efeitos ao apertar os botões de ferramentas:
  # - Zera as seleções e exclusões
  # - Define o tipo de ferramenta
  observeEvent(input$select, {
    removeShape(leafletProxy("map"), selecao() |> paste0("selected"))
    selecao("0")
    exclusao(NA)
    ferramenta("select")
  })
  
  observeEvent(input$multi_select, {
    removeShape(leafletProxy("map"), selecao() |> paste0("selected"))
    selecao("0")
    exclusao(NA)
    ferramenta("multi_select")
  })
  
  observeEvent(input$deselect, {
    removeShape(leafletProxy("map"), selecao() |> paste0("selected"))
    selecao("0")
    exclusao(NA)
    ferramenta("deselect")
  })
  
  # Clique no mapa ####
  observeEvent(input$map_shape_click, {
    selecao <- selecao() |> isolate()
    exclusao <- exclusao() |> isolate()
    ferramenta <- ferramenta() |> isolate()
    ponto <- input$map_shape_click
    
    if (ponto$id %in% exclusao) {
      # se shape já estava excluído, inclui novamente
      exclusao(exclusao[exclusao %in% ponto$id |> not()])
    
    } else if (ferramenta() == "deselect") {
      # se a ferramenta é de exclusão, e não está excluído: exclui.
        exclusao(c(exclusao, ponto$id))
    
    } else if ("0" %in% selecao) {
      # se nenhum shape está selecionado, seleciona
      selecao(ponto$id)
      mostrar_tabela(1)
    
    } else if (regexpr ("selected", ponto$id) >0) { 
      # se já está selecionado, retira da seleção
      removeShape(leafletProxy("map"), ponto$id)
      if (selecao |> length() == 1) { 
        # se for o único selecionado, deixa a lista vazia
        selecao("0")
      } else { 
        # se houver + de 1, elimina o selecionado
        selecao(selecao[selecao!=sub("selected", "", ponto$id)])
        mostrar_tabela(1)
      }
    
    } else if (ferramenta == "multi_select") {
      # se lista não vazia e shape não selecionado, inclui na lista
      selecao(c(selecao,ponto$id))
      mostrar_tabela(1)
    
    } else if (ponto$id %in% setores@data$Name) {
      if (any(selecao %in% setores@data$Name)) {
        # se não é o shape já selecionado e está no mesmo layer, elimina o shape
        # selecionado e seleciona o novo shape
        selecionado <- selecao[selecao %in% setores@data$Name]
        removeShape(leafletProxy("map"), selecionado |> paste0("selected"))
        selecao(c(selecao[selecao!=selecionado],ponto$id))
        mostrar_tabela(1)
      } else {
        # se está em outro layer, apenas seleciona o novo shape
        # (mantendo a seleção do outro layer)
        selecao(c(selecao,ponto$id))
        mostrar_tabela(1)
      }
    
    } else if (ponto$id %in% bairros@data$Name) {
      if (any(selecao %in% bairros@data$Name)) {
        # se não é o shape já selecionado e está no mesmo layer, elimina o shape
        # selecionado e seleciona o novo shape
        selecionado <- selecao[selecao %in% bairros@data$Name]
        removeShape(leafletProxy("map"), selecionado |> paste0("selected"))
        selecao(c(selecao[selecao!=selecionado],ponto$id))
        mostrar_tabela(1)
      } else {
        # se está em outro layer, apenas seleciona o novo shape
        # (mantendo a seleção do outro layer)
        selecao(c(selecao,ponto$id))
        mostrar_tabela(1)
      }
    }
  })
 
  # Dados seleção ####
  dados_setores_selecao <- reactive({
    setores[setores$Name %in% selecao(),c("Name",input$Indicadores)]
  })
  
  dados_bairros_selecao <- reactive({
    bairros[bairros$Name %in% selecao(),c("Name", input$Indicadores)]
  })
  
  # Plota seleção####
  observe({
    proxy <- leafletProxy("map")
    proxy |>
      addPolygons(
        data = dados_setores_selecao(),
        layerId = dados_setores_selecao()@data$Name |> paste0("selected"),
        fillColor = "green",
        # label = dados_setores_selecao()@data[,2],
        fillOpacity = 0.7,
        options = pathOptions(pane = "poligonos_selecao"),
        group = "Setores",
        color = "red",
        weight = 1
      ) |>
      addPolygons(
        data = dados_bairros_selecao(),
        layerId = dados_bairros_selecao()@data$Name |> paste0("selected"),
        fillColor = "green",
        # label = dados_bairros_selecao()@data[,2],
        fillOpacity = 0.7,
        options = pathOptions(pane = "poligonos_selecao"),
        group = "Bairros",
        color = "red",
        weight = 1
      )
  })
  
  # Tabela ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela <- reactiveVal(1)
  output$mostrar_tabela <- reactive(mostrar_tabela())
  outputOptions(output,"mostrar_tabela", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_tabela,mostrar_tabela(-1))
  observeEvent(input$btn_tabela,mostrar_tabela(mostrar_tabela()*-1))
  
  grupo <- reactiveVal("Setores")

  titulo <- reactiveVal("Vitória")
  dados_tabela <- reactiveVal(setores@data[,-c(1,2)])
  
  observe({
    # Altera os dados da tabela conforme seleção, exclusão e layer
    temp <- eval(parse(text = grupo() |> tolower()))@data
    selecao <- selecao()
    exclusao <- exclusao()
    qual_selecao <- selecao %in% temp$Name |> which()
    qual_exclusao <- exclusao %in% temp$Name |> which()
    
    titulo <- "Vitória"
    dados <- temp[,-c(1,2)] |>
      colSums(na.rm = TRUE)
    
    if (qual_exclusao |> length() >=1) {
      titulo <- "seleção múltipla"
      dados <- temp[temp$Name %in% exclusao[qual_exclusao] |> not(),-c(1,2)] |>
        colSums(na.rm = TRUE)
    } else if (qual_selecao |> length() > 1) {
      titulo <- "seleção múltipla"
      dados <- temp[temp$Name %in% selecao[qual_selecao],-c(1,2)] |>
        colSums(na.rm = TRUE)
    } else if (qual_selecao |> length() == 1) {
      titulo <- selecao[qual_selecao] |> str_to_title(locale = "br")
      titulo <- sub(" Da ", " da ", titulo)
      titulo <- sub(" Do ", " do ", titulo)
      titulo <- sub(" De ", " de ", titulo)
      titulo <- sub(" Das ", " das ", titulo)
      titulo <- sub(" Dos ", " dos ", titulo)
      dados <- temp[temp$Name==selecao[qual_selecao],-c(1,2)] |>
        colSums(na.rm = TRUE)
    }
    titulo(titulo)
    # soma dados médios
    dados["RendaDomicilioMedia"] <- 
      dados["RendaTotal"] /
      dados["NumDomicilios"]
    dados["TxInadimplencia"] <-
      dados["Inadimplentes"] /
      dados["NumDomicilios"] *100
    dados["DividaMedia"] <- 
      dados["DividaTotal"] /
      dados["Inadimplentes"]
    dados["ValorVenalMedio"] <- 
      dados["ValorVenalTotal"] / 
      dados["NumDomicilios"]
    dados["ValorIPTUMedio"] <- 
      dados["ValorIPTUTotal"] / 
      dados["NumDomicilios"]
    dados["IPTUValorVenal"] <- 
      dados["ValorIPTUTotal"] /
      dados["ValorVenalTotal"] *100
    dados["DividaRendaMensal"] <- 
      dados["DividaTotal"] / 
      dados["RendaInadimplentes"] *100
    dados["vlVenalImovelMedioInadimplentes"] <- 
      dados["vlVenalImovelInadimplentes"] /
      dados["Inadimplentes"]
    dados["ValorIPTUMedioInadimplente"] <- 
      dados["vlIPTUInadimplentes"] / 
      dados["Inadimplentes"]
    dados["InadimplentesPercentual"] <- 
      (dados["Inadimplentes"] /
         sum(setores@data["Inadimplentes"]) *100)
    dados["DAPercentual"] <- 
      (dados["DividaTotal"] /
         sum(setores@data["DividaTotal"]) *100)
    
    dados_tabela(dados)
  })
  
  # Textos para a tabela
  output$titulo <- renderText(titulo())
  lapply(1:length(indicadores_tabela), function(i) {
    output[[paste0("nome_indicador_",i)]] <- renderText(
      indicadores[indicadores$nome %in% indicadores_tabela[i],"rotulo"]
    )
    output[[paste0("indicador_",i)]] <- renderText(
      dados_tabela()[indicadores_tabela[i]] |> 
        formatar(indicadores_tabela[i])
      
    )
  })
  
  # Variável para exibir painel "carregando..."
  output$carregando <- renderText("")
  outputOptions(output, 'carregando', suspendWhenHidden=FALSE)
}



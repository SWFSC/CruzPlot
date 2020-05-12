# Render statements

### Planned transects outputs
output$planned_transects_text <- renderText({
  planned_transects()
})

output$planned_transects_remove_text <- renderText({
  planned_transects_remove()
})


### Non-DAS outputs
output$cruzNonDasLoaded <- renderDataTable({
  df <- cruz.list$ndas.df
  req(df)
  row.names(df) <- 1:nrow(df)

  df
}, options = list(dom = 't'), rownames = TRUE)

output$cruzNonDasAdd_text <- renderText({
  cruzNonDasAdd()
})

output$cruzNonDasFile_LonLat_text <- renderText({
  cruzNonDasFile_LonLat()
  return("")
})

output$cruzNonDasRemove_text <- renderText({
  cruzNonDasRemove()
})


### DAS tabular output
output$das_out_sight_table <- renderTable({ cruzDasOutSight_Table() })
output$cruzDasOutSight_Save_text <- renderText({ cruzDasOutSight_Save() })

output$das_out_effort_table <- renderTable({ cruzDasOutEffort_Table() })
output$cruzDasOutEffort_Save_text <- renderText({ cruzDasOutEffort_Save() })

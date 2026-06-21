## ----setup, include=FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
# Loading necessary libraries
library(plotly)
library(crosstalk)
library(dplyr)
library(DT)

# Creating the interactive visualization
# Loading data 
map_data <- readRDS("CleanedPropertyData_MapOnly.rds") %>%
  
  # Selects the columns we want to see in final plots
  select(id, sqft, price_combined, type.1, streetAddress, lat, long, url, bedrooms, baths, size, county) %>%
  
  # Making sure filtering is done
  filter(!is.na(lat), !is.na(long), !is.na(price_combined), !is.na(sqft)) %>%
  mutate(
    type.1 = ifelse(is.na(type.1), "Unknown", as.character(type.1)),
    type.1 = as.factor(type.1)
  )

# Creating a shared data object so that we can interact between the plots and table
shared_data <- SharedData$new(map_data, key = ~id, group = "property")

# Defining custom colors so they are more distinguishable
custom_colors <- c(
  "Single Family" = "#1f77b4",
  "Townhouse"     = "#ff7f0e",
  "Mobile"        = "#2ca02c",
  "Multi-Family"  = "#d62728",
  "Coop"          = "#9467bd",
  "Land"          = "#8c564b",
  "Mfd/Mobile"    = "#e377c2",
  "Unknown"       = "#7f7f7f"
)


## ----map_plot, echo=FALSE, warning = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
# Interactive plot with points that are color coded based on property type
plot_ly(shared_data,
        lat = ~lat, lon = ~long,
        type = "scattermapbox", mode = "markers",
        color = ~type.1,
        colors = custom_colors,
        text = ~paste0(
          "<b>Address:</b> ", streetAddress, "<br>",
          "<b>Price:</b> $", format(price_combined, big.mark = ","), "<br>",
          "<a href='", url, "' target='_blank'>Listing Link</a>"
        ),
        hoverinfo = "text",
        marker = list(size = 8)) %>%
  layout(
    mapbox = list(style = "open-street-map", zoom = 7,
                  center = list(lat = 38.5, lon = -121.75)),
    title = "Property Map"
  )


## ----scatter_plot, echo=FALSE, warning = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
# Interactive scatter plot for price vs sq ft
plot_ly(shared_data,
        x = ~sqft, y = ~price_combined,
        type = "scatter", mode = "markers",
        color = ~type.1,
        colors = custom_colors,
        text = ~paste0(
          "<b>Address:</b> ", streetAddress, "<br>",
          "<b>Price:</b> $", format(price_combined, big.mark = ","), "<br>",
          "<a href='", url, "' target='_blank'>Listing Link</a>"
        ),
        hoverinfo = "text",
        marker = list(size = 8)) %>%
  layout(
    title = "Price vs Sq Ft",
    xaxis = list(title = "Sq Ft"),
    yaxis = list(title = "Price")
  )


## ----table, echo=FALSE, warning = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Interactive data table that updates based on point selection on either plot
datatable(shared_data,
          options = list(pageLength = 5, scrollX = TRUE),
          selection = 'single',
          rownames = FALSE)


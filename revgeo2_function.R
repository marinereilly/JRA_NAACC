revgeo2 <- function (longitude, latitude, provider = NULL, API = NULL, output = NULL, item = NULL) 
{
  if (missing(provider)) {
    provider <- NULL
  }
  if (missing(API)) {
    API <- NULL
  }
  if (missing(output)) {
    output <- NULL
  }
  if (missing(item)) {
    item <- NULL
  }
  
  geocode_data <- list()
  geocode_frame <- data.frame()
  
  async_download <- function(url, provider) {
    
    if (is.null(provider)) {provider <- "photon"}
    
    provider_title <- paste0(toupper(substr(provider, 1, 1)), substr(provider, 2, nchar(provider)))
    responses <- vector(mode = "list", length = length(url))
    url_ids <- seq_along(url)
    
    callback <- function(id){
      function(response){
        
        content_char <- rawToChar(response$content)
        Encoding(content_char) <- "UTF-8"
        
        if (!jsonlite::validate(content_char)) {
          warning(paste0("Error encountered upon retrieving data from ", provider_title, ": ", response$url))
        } else if (provider == "google") {
          if (content_parsed$status == "ZERO_RESULTS") {
            message <- "Google Maps could not find an address for your coordinates.  Please double check that they are accurate and try again."
            warning(message)
            response <- message
          } else if(content_parsed$status %in% "REQUEST_DENIED") {
            message <- "There was an error accessing Google Maps.  Check your API key and try again."
            warning(message)
            response <- message
          }
        } else {
          print(paste0("Getting geocode data from ", provider_title, ": ", response$url))
          content_parsed <- jsonlite::fromJSON(content_char)
          response <- content_parsed
        }
        
        responses[[id]] <<- response
      }
    }
    errorhandle <- function(response) {
      message <- paste0("The request returned the status code: ", response$status_code, ". Please check that the provided API key is correct.")
      warning(response)
    }
    cbfuns <- lapply(url_ids, callback)
    p <- curl::new_pool(total_con = 10, host_con = 5)
    for (i in seq_along(url)) {
      curl::curl_fetch_multi(url[i], done = cbfuns[[i]], fail = errorhandle, handle = curl::new_handle(failonerror = FALSE), pool = p)
    }
    curl::multi_run(pool = p)
    
    return(responses)
  }
  
  
  if (is.null(provider) || (provider %in% "photon")) {
    url <- paste0("https://photon.komoot.de/reverse?lon=", 
                  longitude, "&lat=", latitude)
    
    responses <- async_download(url, provider)
    
    for (response in responses) {
      # housenumber <- tryCatch(response$features$properties$housenumber, 
      #                         error = function(e) "House Number Not Found")
      street <- tryCatch(response$features$properties$name, 
                         error = function(e) "Street Not Found")
      city <- tryCatch(response$features$properties$city, 
                       error = function(e) "City Not Found")
      # zip <- tryCatch(response$features$properties$postcode, 
      #                 error = function(e) "Postcode Not Found")
      state <- tryCatch(response$features$properties$state, 
                        error = function(e) "State Not Found")
      country <- tryCatch(response$features$properties$country, 
                          error = function(e) "Country Not Found")
      # if (is.null(housenumber)) {
      #   housenumber <- "House Number Not Found"
      # }
      if (is.null(street)) {
        street <- "Street Not Found"
      }
      if (is.null(city)) {
        city <- "City Not Found"
      }
      # if (is.null(zip)) {
      #   zip <- "Postcode Not Found"
      # }
      if (is.null(state)) {
        state <- "State Not Found"
      }
      if (is.null(country)) {
        country <- "Country Not Found"
      }
      if (is.null(output)) {
        geocode_data <- append(geocode_data, paste(street, city, state, country, sep = ", "))
      }
      else if (output == "string") {
        geocode_data <- append(geocode_data, paste(street, city, state, zip, country, sep = ", "))
      }
      else if (output == "hash") {
        # geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], 
        #                                    housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                      street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], 
                                    city)
        geocode_data[["state"]] <- c(geocode_data[["state"]], 
                                     state)
        # geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
        #                            zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                       country)
      }
      else {
        # geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], 
        #                                    housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                      street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], 
                                    city)
        geocode_data[["state"]] <- c(geocode_data[["state"]], 
                                     state)
        # geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
        #                            zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                       country)
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  }
  else if (is.null(API) && provider %in% "Bing") {
    print("Please enter your Bing api")
    return()
  }
  else if (provider %in% "bing") {
    url <- paste0("http://dev.virtualearth.net/REST/v1/Locations/", 
                  latitude, ",", longitude, "?o=json&key=", API)
    if (!(is.null(item))) {
      if (item %in% "housenumber") {
        print("Defaulting to street, since Bing returns house numbers and streets together.")
        item <- "street"
      }
    }
    
    responses <- async_download(url, provider)
    
    for (response in responses) {
      address_hash <- tryCatch(as.list(response$resourceSets$resources[[1]]$address), 
                               error = function(e) "House Number Not Found")
      street <- tryCatch(address_hash$addressLine[[1]], error = function(e) "House Number and Street Not Found")
      city <- tryCatch(address_hash$locality[[1]], error = function(e) "City Not Found")
      state <- tryCatch(address_hash$adminDistrict[[1]], error = function(e) "State Not Found")
      zip <- tryCatch(address_hash$postalCode[[1]], error = function(e) "Postcode Not Found")
      country <- tryCatch(address_hash$countryRegion[[1]], 
                          error = function(e) "Country Not Found")
      address_string <- tryCatch(address_hash$formattedAddress[[1]], 
                                 error = function(e) "Address Number Not Found")
      if (is.null(street)) {
        street <- "House Number and Street Not Found"
      }
      if (is.null(city)) {
        city <- "City Not Found"
      }
      if (is.null(zip)) {
        zip <- "Postcode Not Found"
      }
      if (is.null(county)) {
        county <- "County Not Found"
      }
      if (is.null(state)) {
        state <- "State Not Found"
      }
      if (is.null(country)) {
        country <- "Country Not Found"
      }
      if (is.null(output)) {
        geocode_data <- append(geocode_data, address_string)
      }
      else if (output == "string") {
        geocode_data <- append(geocode_data, address_string)
      }
      else if (output == "hash") {
        geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                      street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], 
                                    city)
        geocode_data[["state"]] <- c(geocode_data[["state"]], 
                                     state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
                                   zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                       country)
      }
      else {
        geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                      street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], 
                                    city)
        geocode_data[["state"]] <- c(geocode_data[["state"]], 
                                     state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
                                   zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                       country)
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  }
  else if (is.null(API) && provider %in% "google") {
    print("Please enter your Google api")
    return()
  }
  else if (provider %in% "google") {
    url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=", 
                  latitude, ",", longitude, "&key=", API)
    postcode <- list()
    responses <- async_download(url, provider)
    
    for (response in responses) {
      
      if ("results" %in% colnames(response) == TRUE) {
        address <- response$results$formatted_address
        l <- length(response$results$address_components)
        k <- 1
        while (k <= l) {
          j <- response$results$address_components[[k]]
          if (j$types[1] == "street_number") {
            housenumber <- tryCatch(j$short_name, error = function(e) "House Number Not Found")
          }
          else if (j$types[1] == "route") {
            street <- tryCatch(j$long_name, error = function(e) "Street Not Found")
          }
          else if (j$types[1] == "locality") {
            city <- tryCatch(j$long_name, error = function(e) "City Not Found")
          }
          else if (j$types[1] == "administrative_area_level_2") {
            county <- tryCatch(j$long_name, error = function(e) "County Not Found")
          }
          else if (j$types[1] == "postal_code") {
            zip <- tryCatch(j$long_name, error = function(e) "Postcode Not Found")
          }
          else if (j$types[1] == "administrative_area_level_1") {
            state <- tryCatch(j$long_name, error = function(e) "State Not Found")
          }
          else if (j$types[1] == "country") {
            country <- tryCatch(j$long_name, error = function(e) "State Not Found")
          }
          k <- k + 1
        }
      } else {
        address <- response
      }
      if (!(exists("housenumber"))) {
        housenumber <- "House Number Not Found"
      }
      if (!(exists("street"))) {
        street <- "Street Not Found"
      }
      if (!(exists("city"))) {
        city <- "City Not Found"
      }
      if (!(exists("zip"))) {
        zip <- "Postcode Not Found"
      }
      if (!(exists("state"))) {
        state <- "State Not Found"
      }
      if (!(exists("country"))) {
        country <- "Country Not Found"
      }
      if (is.null(output)) {
        geocode_data <- append(geocode_data, address)
      }
      else if (output == "string") {
        geocode_data <- append(geocode_data, address)
      }
      else if (output == "hash") {
        geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], 
                                           housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                      street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], 
                                    city)
        geocode_data[["county"]] <- c(geocode_data[["county"]], 
                                      county)
        geocode_data[["state"]] <- c(geocode_data[["state"]], 
                                     state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
                                   zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                       country)
      }
      else {
        geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], 
                                           housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                      street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], 
                                    city)
        geocode_data[["county"]] <- c(geocode_data[["county"]], 
                                      county)
        geocode_data[["state"]] <- c(geocode_data[["state"]], 
                                     state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
                                   zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                       country)
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  }
  else {
    print("Please enter a provider (photon, google, or bing)")
  }
  if (!nrow(geocode_frame)) {
    if (!(is.null(output %in% "hash"))) {
      if (is.null(item)) {
        return(geocode_data)
      }
      else {
        return(geocode_data[item])
      }
    }
    else {
      return(geocode_data)
    }
  }
  else {
    return(as.data.frame(geocode_data))
  }
}

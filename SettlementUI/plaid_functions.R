###############
# plaidR      #
#             #
# Craig Certo #
###############

###############################
# Create Plaid Connect Object # 
###############################

# Create a plaid connection object
plaid_connect <- function(
    
  client_id = Sys.getenv("client_id"),
  secret = Sys.getenv("secret"),
  webhook = Sys.getenv("webhook_url"),
  redirect_uri = Sys.getenv("redirect_uri"),
  link_token = Sys.getenv("link_token"),
  link_token_expiration = Sys.getenv("link_token_expiration"),
  link_token_request_id = Sys.getenv("link_token_request_id")
  
) {
  
  # Create a connection object
  plaid_con <- list(
    client_id = client_id,
    secret = secret,
    webhook = webhook,
    redirect_uri = redirect_uri,
    link_token = link_token,
    link_token_expiration = link_token_expiration,
    link_token_request_id = link_token_request_id
  )
  Sys.setenv(plaid_con)
  plaid_con
  
}

##################
# Get Link Token # 
##################

# Retrieve a link token for a given Client and user
get_link_token <- function(plaid_con, client_name, user, products) {
  
  tryCatch({
    
    # Request body
    body <- list(
      client_id = Sys.getenv("client_id"),
      secret = Sys.getenv("secret"),
      client_name = client_name,
      country_codes = list("US"),
      products = products,
      webhook = Sys.getenv("webhook_url"),
      language = "en",
      user = list(
        client_user_id = user$client_user_id,
        phone_number = user$phone_number
      ),
      redirect_uri = Sys.getenv("redirect_uri")
    )
    
    # POST to link/token/create
    response <- httr::POST(
      url = "https://sandbox.plaid.com/link/token/create",
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json()
    )
    
    # Throw error if response is not 200
    if (response$status_code != 200) {
      cli::cli_abort("Bad response in link/token/create: {response}")
    }
    
    # Set link token information
    content <- response |>
      httr::content() 
    names(content) <- c("link_token_expiration", "link_token", "link_token_request_id")
    Sys.setenv(content)
    
    # Return plaid object
    plaid_connect()
    
  }, error = function(e) {
    
    # Write data from environment into the database
    # Log error
    cli::cli_abort("Error in get_link_token - {e}")
    
  })
  
}


plaid_con <- 
  plaid_connect() |>
  get_link_token(client_name, user, products)

response

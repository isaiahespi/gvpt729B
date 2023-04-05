# Integrating ChatGPT with R

# Load Libraries
library(httr)
library(stringr)

chatGPT_API <- "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"

# We, then need to establish the connection between R and our chatGPT account.
# The chunk below a little complex as there are a few thing going at the same
# time.

# Establish the connection using the actual chatGPT website
# Get Authorization using your personalized (chatGPT_API, in my case)
# Output Type (I am using json)
# Customize the output options
# Ask Question to chatGPT
# Save the output as an object (I am going to save it as chatGPT_response)

# My question to chatGPT is “Why do people cry?”. Let’s see what chatGP’s answer is.

chatGPT_response <- POST(
  # use chatGPT website (you can copy paste)
  url = "https://api.openai.com/v1/chat/completions",
  # Authorize
  add_headers(Authorization = paste("Bearer", chatGPT_API)),
  # Output type: use JSON
  content_type_json(),
  # encode the value to json format
  encode = "json",
  # Controlling what to show as the output, it's going to be a list of following things
  body = list(
    model = "gpt-3.5-turbo-0301", # Use gpt-3.5 is very fast
    messages = list(list(role = "user", content = "Why do people cry?"))
  )
)
# Print chatGPT's Answer
content(chatGPT_response)


# I will now use the {stringr} to clean the output and display just the message.

# Selecting the portion we want to display
answer_one <- content(chatGPT_response)$choices[[1]]$message$content
# cleaning the selected output
answer_one <- stringr::str_trim(answer_one)
# Printing the message as a character string
cat(answer_one)


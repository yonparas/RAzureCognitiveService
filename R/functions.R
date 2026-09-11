get_url_header <- function(){
  # assumes config was loaded from main script thus config is being accessed
  # thru searching global variables
  add_headers("Ocp-Apim-Subscription-Key" = config$key,
              "Content-Type" = "application/json")
}
cdown <- function(i){
  nums <- seq(i, 1, -1)
  for (n in nums){
    cat(sprintf("\r%s: Pausing until %s...", now(), n))
    Sys.sleep(1)
  }
  cat(sprintf("\r%s: Continue...          \n", now()))
}
get_request_body <- function(filename, diarization){
  # This request body is formatted in this way so that when you transform this into a
  # json character, it will match exactly how the request body is written in the API docs.
  # NOTE: unbox() function makes sure that the json format of key: value will not be enclosed
  # with [] when converted to json. (If there is no unbox() present, R will treat it as an array)
  if (diarization == FALSE){
    reqbody <- toJSON(
      list(
        contentUrls = wavlink,
        locale = unbox("en-US"),
        displayName = unbox(filename),
        properties = list(
          wordLevelTimestampsEnabled = unbox("true"),
          timeToLive = unbox("PT24H")
        )
      )
    )
  } else {
    reqbody <- toJSON(
      list(
        contentUrls = wavlink,
        locale = unbox("en-US"),
        displayName = unbox(filename),
        properties = list(
          wordLevelTimestampsEnabled = unbox("true"),
          timeToLive = unbox("PT24H"),
          diarizationEnabled = unbox("true"),
          diarization = list(
            speakers = list(
              minCount = unbox("2"),
              maxCount = unbox("2")
            )
          )
        )
      )
    )
  }
  return(reqbody)
}
generate_transcript <- function(wavlink, filename, outputdir, diarization = FALSE){
  cat(sprintf("%s: Starting transcription process...\n", now()))

  # Create URL headers as described by the API documentation ====
  # This URL header is created by using httr::ad_headers and own subscription key ====
  url_header <- get_url_header()

  # Create request body as described by the API documentaion ====
  req_body <- get_request_body(filename = filename, diarization = diarization)

  # This is the base URI where we submit all our API calls ====
  uri <- sprintf(config$transuri_template, config$region)

  cat(sprintf("%s: Submitting query to API...\n", now()))

  # This code submits the generated items above using the http function POST
  api_response_raw <- POST(uri, url_header, body = req_body)

  # This extracts the contents that the API returned. We use the returned contents
  # to continue on with the task
  api_response_content <- api_response_raw$content %>%
    rawToChar() %>%
    fromJSON

  cdown(5)
  cat(sprintf("%s: Checking self link and job status...\n", now()))

  # We try to check the status of the job that was created, whether it is running,
  # succeeded, or error.
  self_check_raw <- GET(api_response_content$self, url_header)
  self_check_content <- self_check_raw$content %>%
    rawToChar() %>%
    fromJSON

  cat(sprintf("%s: Self link and job status is: %s...\n", now(),
              self_check_content$status))

  if (self_check_content$status == "Failed"){
    dlfiles <- api_response_content$links$files
    cat(sprintf("%s: Proceeding with download links of length: %s...\n", now(),
                length(dlfiles)))

    for (dlf in dlfiles){
      cat(sprintf("%s: Querying link: %s...\n", now(), dlf))

      dlf_raw <- GET(dlf, url_header)
      dlf_content <- dlf_raw$content %>% rawToChar() %>% fromJSON
      dlf_values <- dlf_content$values %>% as.data.table

      cat(sprintf("%s: Success...\n", now()))
      print(dlf_values)

      for (i in 1:nrow(dlf_values)){
        cat(sprintf("%s: Downloading link from row #%s...\n", now(), i))

        thisrow <- dlf_values[i]
        trans_raw <- GET(thisrow$links)
        trans_content <- trans_raw$content %>%
          rawToChar() %>%
          fromJSON()

        outdir <- file.path(outputdir, filename)
        if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
        cat(sprintf("%s: Writing file to %s...\n", now(), file.path(outdir, thisrow$name)))
        write_json(trans_content, file.path(outdir, thisrow$name), pretty = TRUE)
      }
    }
    return(FALSE)
  }

  # Throw error if job has not been initiated
  if (!self_check_content$status %in% c("Succeeded", "Running")){
    message("Job not yet running or has encountered an error")
    cdown(10)
  }

  sleep_i <- 0
  while (self_check_content$status == "Running" & sleep_i <= 50){
    cat(sprintf("%s: Looping wait until succeeded. Loop count: %s/50...\n", now(),
                sleep_i+1))
    cdown((sleep_i * 5)+1) # waiting time in seconds increasing by multiples of 5

    self_check_raw <- GET(api_response_content$self, url_header)
    self_check_content <- self_check_raw$content %>%
      rawToChar() %>%
      fromJSON
    sleep_i <- sleep_i + 1
  }

  if (self_check_content$status == "Succeeded"){
    cat(sprintf("%s: Job status: %s...\n", now(), self_check_content$status))

    # If job status = Succeeded, proceed to the files that can be downloaded
    dlfiles <- self_check_content$links$files
    cat(sprintf("%s: Proceeding with download links of length: %s...\n", now(),
                length(dlfiles)))

    for (dlf in dlfiles){
      cat(sprintf("%s: Querying link: %s...\n", now(), dlf))

      dlf_raw <- GET(dlf, url_header)
      dlf_content <- dlf_raw$content %>% rawToChar() %>% fromJSON
      dlf_values <- dlf_content$values %>% as.data.table

      cat(sprintf("%s: Success...\n", now()))
      print(dlf_values)

      for (i in 1:nrow(dlf_values)){
        cat(sprintf("%s: Downloading link from row #%s...\n", now(), i))

        thisrow <- dlf_values[i]
        trans_raw <- GET(thisrow$links)
        trans_content <- trans_raw$content %>%
          rawToChar() %>%
          fromJSON()

        outdir <- file.path(outputdir, filename)
        if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
        cat(sprintf("%s: Writing file to %s...\n", now(), file.path(outdir, thisrow$name)))
        write_json(trans_content, file.path(outdir, thisrow$name), pretty = TRUE)

        if (thisrow$kind == "Transcription"){
          crp_lexfile <- file.path(outdir, "crp_lexical.txt")
          crp_lexical <- trans_content$combinedRecognizedPhrases$lexical

          cat(sprintf("%s: Writing file to %s...\n", now(), crp_lexfile))
          cat(crp_lexical, file = crp_lexfile)

          crp_itnfile <- file.path(outdir, "crp_itn.txt")
          crp_itn <- trans_content$combinedRecognizedPhrases$itn

          cat(sprintf("%s: Writing file to %s...\n", now(), crp_itnfile))
          cat(crp_itn, file = crp_itnfile)

          crp_maskeditnfile <- file.path(outdir, "crp_maskeditn.txt")
          crp_maskeditn <- trans_content$combinedRecognizedPhrases$maskedITN

          cat(sprintf("%s: Writing file to %s...\n", now(), crp_maskeditnfile))
          cat(crp_maskeditn, file = crp_maskeditnfile)

          crp_displayfile <- file.path(outdir, "crp_display.txt")
          crp_display <- trans_content$combinedRecognizedPhrases$display

          cat(sprintf("%s: Writing file to %s...\n", now(), crp_displayfile))
          cat(crp_display, file = crp_displayfile)

          wordcsvfile <- file.path(outdir, "words.csv")
          wordcsv <- lapply(1:length(trans_content$recognizedPhrases$nBest), function(x){
            disptext <- trans_content$recognizedPhrases$nBest[[x]]$display %>%
              str_split(" ") %>% unlist
            nbdt <- trans_content$recognizedPhrases$nBest[[x]]$words %>%
              lapply(as.data.table) %>% rbindlist %>%
              .[, `:=` (display = disptext)] %>%
              .[]
            if (diarization == TRUE){
              spkr <- trans_content$recognizedPhrases$speaker[x]
              nbdt[, `:=` (speaker = spkr)]
            }
            return(nbdt)
          }) %>% rbindlist

          cat(sprintf("%s: Writing file to %s...\n", now(), wordcsvfile))
          fwrite(wordcsv, wordcsvfile)

        }
      }
    }
  }
  cat(sprintf("%s: Deleting transcription...\n", now()))
  DELETE(api_response_content$self, url_header)
  cat(sprintf("%s: End...\n", now()))
}

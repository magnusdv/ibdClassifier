

preparePdfs = function(lengthData, cutoff = 0) {
  # Apply cutoff if present
  if(cutoff > 0)
    lengthData = lapply(lengthData, function(v) v[v >= cutoff])

  # Density functions
  list(countPdf = lengths(lengthData) |>
         density(from = 0) |> approxfun(rule = 2),
       totalPdf = vapply(lengthData, sum, FUN.VALUE = 1) |>
         density(from = 0) |> approxfun(rule = 2),
       lengthPdf = unlist(lengthData, use.names = FALSE) |>
         density(from = 0) |> approxfun(rule = 2))
}


# obs = a vector of observed lengths
# pdfs = densities for a hypothesised relationship
classProb = function(obs, pdfs, log = TRUE) {

  p0 = pdfs$countPdf(length(obs))
  p1 = pdfs$totalPdf(sum(obs))
  pL = pdfs$lengthPdf(obs)

  probs = c(p0, p1, pL)

  if(log) sum(log(probs)) else prod(probs)
}


# obs = a vector of observed lengths
classify = function(obs, pdfuns) {
  logprobs = sapply(pdfuns, function(pdfs) classProb(obs, pdfs, log = TRUE))

  # log-sum-exp trick
  res = exp(logprobs - matrixStats::logSumExp(logprobs))

  sort(res, decreasing = TRUE)
}



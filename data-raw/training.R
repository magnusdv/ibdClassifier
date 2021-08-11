library(ibdsim2)

`%||%` = function(x, y) {
  if(is.null(x)) y else x
}

# Function generating sharing data for a given relationship --------------------------------------------------------

sampleData = function(rel, N, seed = 1729) {
  ped = rel$ped
  child = rel$ids
  prnts = parents(ped, child)

  MAP = loadMap("decode19", uniform = TRUE, sexAverage = FALSE)

  sims = ibdsim(ped, N = N, ids = c(prnts, child), map = MAP, seed = seed, verbose = FALSE)

  segS = findPattern(sims, pattern = list(carriers = prnts)) |> segmentStats(returnAll = TRUE)
  segA = findPattern(sims, pattern = list(autozyg  = child)) |> segmentStats(returnAll = TRUE)

  list(sharing = unname(split(segS$allSegs, rep(1:N, segS$perSim$Count))),
       autoz   = unname(split(segA$allSegs, rep(1:N, segA$perSim$Count))))
}


# Generate data -----------------------------------------------------------

rels = readRDS(here("data/relationships.rds"))
relsInbred = rels$inbred

N = 1000

# Initialise
sharingData = list()
autozData = list()

# Loop through relationships
for(rel in relsInbred) {  print(rel$name)
  sims = sampleData(rel, N = N)
  sharingData[[rel$tag]] = sims$sharing
  autozData[[rel$tag]] = sims$autoz
}


# Store
saveRDS(sharingData, file = here("data/sharing.rds"))
saveRDS(autozData, file = here("data/autoz.rds"))

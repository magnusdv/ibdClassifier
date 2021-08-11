library(pedtools)
library(ribd)
library(here)

print("In data-raw!")

# Functions to save typing

nuc = function(...) nuclearPed(...)
lin = function(...) linearPed(...)

swpSx = function(x, ids)
  relabel(swapSex(x, ids, verbose = FALSE), "asPlot")

addDau = function(x, id)
  addDaughter(x, id, verbose = F)

cous = function(deg, rem = 0, swp = NULL)
  swpSx(cousinPed(deg, removal = rem), swp)

hcous = function(deg, rem = 0, swp = NULL)
  swpSx(halfCousinPed(deg, removal = rem), swp)


# Define relationships

pairwise = list(
  `po-pat` = list(ped = nuc(1, sex = 2), ids = c(1,3), name = "Father-daughter"),
  `po-mat` = list(ped = nuc(1, sex = 1), ids = c(3,2), name = "Mother-son"),
  `fs` = list(ped = nuc(2, sex = 1:2), ids = 3:4, name = "Full siblings"),
  `gp-pat` = list(ped = lin(2, sex = c(1,2)), ids = c(1,5), name = "Grandparent (pat)"),
  `gp-mat` = list(ped = lin(2, sex = c(2,2)), ids = c(1,5), name = "Grandparent (mat)"),
  `hs-pat` = list(ped = halfSibPed(type = "pat", sex2 = 2), ids = 4:5, name = "Half-siblings (pat)"),
  `hs-mat` = list(ped = halfSibPed(type = "mat", sex2 = 2), ids = 4:5, name = "Half-siblings (mat)"),
  `av-pat` = list(ped = nuc(2, sex = c(1,1)) |> addDau(4), ids = c(3,6), name = "Avuncular (pat)"),
  `av-mat` = list(ped = nuc(2, sex = c(1,2)) |> addDau(4), ids = c(3,6), name = "Avuncular (mat)"),
  `dfc` = list(ped = swpSx(doubleFirstCousins(), 10), ids = 9:10, name = "Double first cousins"),
  `qhfc` = list(ped = quadHalfFirstCousins(), ids = 9:10, name = "Quad half first cousins"),
  `ggp-pp` = list(ped = lin(3, sex = c(1,1,2)), ids = c(1,7), name = "Great-grandparent (pp)"),
  `ggp-mp` = list(ped = lin(3, sex = c(2,1,2)), ids = c(1,7), name = "Great-grandparent (mp)"),
  `ggp-mm` = list(ped = lin(3, sex = c(2,2,2)), ids = c(1,7), name = "Great-grandparent (mm)"),
  `fc-pp` = list(ped = cous(1, swp = 8), ids = 7:8, name = "First cousins (pp)"),
  `fc-mp` = list(ped = cous(1, swp = c(4,8)), ids = 7:8, name = "First cousins (mp)"),
  `fc-mm` = list(ped = cous(1, swp = c(4:5,8)), ids = 7:8, name = "First cousins (mm)"),
  `hav-pp` = list(ped = hcous(0, r = 1, swp = 7), ids = c(4,7), name = "Half avuncular (pp)"),
  `hav-pm` = list(ped = hcous(0, r = 1, swp = c(5,7)), ids = c(4,7), name = "Half avuncular (pm)"),
  `hav-mm` = list(ped = hcous(0, r = 1, swp = c(2,5,7)), ids = c(4,7), name = "Half avuncular (mm)"),
  `gav-pp` = list(ped = cous(0, r = 2, swp = 8), ids = c(3,8), name = "Grand avuncular (pp)"),
  `gav-mp` = list(ped = cous(0, r = 2, swp = c(4,8)), ids = c(3,8), name = "Grand avuncular (mp)"),
  `gav-mm` = list(ped = cous(0, r = 2, swp = c(4,6,8)), ids = c(3,8), name = "Grand avuncular (mm)"),
  `hfc-ppp` = list(ped = hcous(1, swp = 9), ids = 8:9, name = "Half first cousins (ppp)"),
  `hfc-ppm` = list(ped = hcous(1, swp = c(6,9)), ids = 8:9, name = "Half first cousins (ppm)"),
  `hfc-pmp` = list(ped = hcous(1, swp = c(2,9)), ids = 8:9, name = "Half first cousins (pmp)"),
  `hfc-mpm` = list(ped = hcous(1, swp = c(4,6,9)), ids = 8:9, name = "Half first cousins (mpm)"),
  `hfc-pmm` = list(ped = hcous(1, swp = c(2,6,9)), ids = 8:9, name = "Half first cousins (pmm)"),
  `hfc-mmm` = list(ped = hcous(1, swp = c(2,4,6,9)), ids = 8:9, name = "Half first cousins (mmm)"),
  `gggp-ppp` = list(ped = lin(4, sex = c(1,1,1,2)), ids = c(1,9), name = "Great-great-grandp (ppp)"),
  `gggp-ppm` = list(ped = lin(4, sex = c(1,1,2,2)), ids = c(1,9), name = "Great-great-grandp (ppm)"),
  `gggp-pmm` = list(ped = lin(4, sex = c(1,2,2,2)), ids = c(1,9), name = "Great-great-grandp (pmm)"),
  `gggp-mmm` = list(ped = lin(4, sex = c(2,2,2,2)), ids = c(1,9), name = "Great-great-grandp (mmm)"),
  `fc1r-ppp` = list(ped = cous(1, r = 1, swp = 10), ids = c(7, 10), name = "1st cousins 1r (ppp)"),
  `fc1r-ppm` = list(ped = cous(1, r = 1, swp = c(8,10)), ids = c(7,10), name = "1st cousins 1r (ppm)"),
  `fc1r-pmp` = list(ped = cous(1, r = 1, swp = c(5,10)), ids = c(7,10), name = "1st cousins 1r (pmp)"),
  `fc1r-mpp` = list(ped = cous(1, r = 1, swp = c(3,10)), ids = c(7,10), name = "1st cousins 1r (mpp)"),
  `fc1r-pmm` = list(ped = cous(1, r = 1, swp = c(5,8,10)), ids = c(7,10), name = "1st cousins 1r (pmm)"),
  `fc1r-mmm` = list(ped = cous(1, r = 1, swp = c(3,5,8,10)), ids = c(7,10), name = "1st cousins 1r (mmm)"),
  `sc-pppp` = list(ped = cous(2, swp = 12), ids = c(11, 12), name = "2nd cousins (pppp)"),
  `sc-pppm` = list(ped = cous(2, swp = c(9,12)), ids = c(11, 12), name = "2nd cousins (pppm)"),
  `sc-ppmm` = list(ped = cous(2, swp = c(5,9,12)), ids = c(11, 12), name = "2nd cousins (ppmm)"),
  `sc-pmmm` = list(ped = cous(2, swp = c(3,5,9,12)), ids = c(11, 12), name = "2nd cousins (pmmm)"),
  `sc-mmmm` = list(ped = cous(2, swp = c(3,5,7,9,12)), ids = c(11, 12), name = "2nd cousins (mmmm)")
)

# Add info
for(r in names(pairwise)) {
  dat = pairwise[[r]]
  y = list(
    kinship = ribd::kinship(dat$ped, ids = dat$ids),
    kappa = ribd::kappaIBD(dat$ped, ids = dat$ids),
    group = sub(" \\(.*\\)", "", dat$name),
    tag = r)

  pairwise[[r]] = modifyList(dat, y)
}


### Inbred versions: Add child to `ids`

inbred = lapply(pairwise, function(r) {
  ped = r$ped
  ids = r$ids
  newped = addChildren(ped, fa = ids[1], mo = ids[2], nch = 1)

  modifyList(r, list(ped = newped, ids = leaves(newped), inbreeding = r$kinship))
})

# k = 16:30; x11(width = 35, height = 23); nrw = floor(sqrt(length(k))); par(mfrow = c(nrw,ceiling(length(k)/nrw))); for(r in inbred[k]) plot(r$ped, hatched = r$ids, title = r$tag)


saveRDS(list(pairwise = pairwise, inbred = inbred), here("data/relationships.rds"))

`%||%` = function(x, y) {
  if(is.null(x)) y else x
}

safeMean = function(x) {
  if(length(x) == 0) 0 else mean(x)
}

aggreg = function(df, col, by) {
  res = aggregate(df[[col]], by = list(df[[by]]), FUN = sum)
  names(res) = c(by, col)
  res[order(res[[col]], decreasing = TRUE), , drop = FALSE]
}

frac2str = function(x) {
  if(length(x) > 1) 
    return(sapply(x, frac2str))
  a = x * 2^(1:8)
  k = match(T, as.integer(a) == a, nomatch = 0)
  if(k == 0) 
    return(as.character(x))
  numer = as.integer(a[k])
  paste(numer, 2^k, sep = "/")
}


bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")
link = function(s, href = s) a(s, href = href, .noWS = "outside")

BUILTIN_PEDS = c(Choose = "", "Trio", "Siblings", "Sibship of 3", "Half sibs, maternal", "Half sibs, paternal",
                 "3/4-siblings", "3/4-siblings + child", "Grandparent (female line)", "Grandparent (male line)",
                 "Great grandparent (female line)", "Great grandparent (male line)",
                 "1st cousins", "1st cousins + child", "2nd cousins", "2nd cousins + child",
                 "Half 1st cousins", "Half 1st cousins + child", "Half 2nd cousins", "Half 2nd cousins + child",
                 "Double 1st cousins", "Double 1st cousins + child", "Quad half 1st cousins",
                 "Full sib mating", "Half sib stack", "Father-daughter incest", "Mother-son incest")

errModal = function(...) {
  mess = paste(lapply(list(...), toString), collapse = "")
  showModal(modalDialog(mess))
}

loadBuiltin = function(choice) {
  switch(choice,
    "Trio" = nuclearPed(1),
    "Siblings" = nuclearPed(2),
    "Sibship of 3" = nuclearPed(3, sex = c(1,2,1)),
    "Half sibs, maternal" = halfSibPed(1, 1, type = "maternal"),
    "Half sibs, paternal" = halfSibPed(1, 1),
    "3/4-siblings" = addChildren(addChildren(nuclearPed(2), 3, mother = 5, 1), 4, mother = 5, nch = 1),
    "3/4-siblings + child" = addChildren(addChildren(addChildren(nuclearPed(2), 3, mother = 5, 1), 4, mother = 5, nch = 1, sex = 2), 6, 7, 1),
    "Grandparent (female line)" = linearPed(2, sex = 2),
    "Grandparent (male line)" = linearPed(2),
    "Great grandparent (female line)" = linearPed(3, sex = 2),
    "Great grandparent (male line)" = linearPed(3),
    "1st cousins"   = cousinPed(1),
    "1st cousins + child"   = cousinPed(1, child = TRUE),
    "2nd cousins"  = cousinPed(2),
    "2nd cousins + child"  = cousinPed(2, child = TRUE),
    "Half 1st cousins"   = halfCousinPed(1),
    "Half 1st cousins + child"   = halfCousinPed(1, child = TRUE),
    "Half 2nd cousins"  = halfCousinPed(2),
    "Half 2nd cousins + child"  = halfCousinPed(2, child = TRUE),
    "Double 1st cousins" = doubleFirstCousins(),
    "Double 1st cousins + child" = doubleCousins(1, 1, child = TRUE),
    "Quad half 1st cousins" = quadHalfFirstCousins(),
    "Full sib mating" = fullSibMating(1),
    "Half sib stack" = halfSibStack(2),
    "Father-daughter incest" = addChildren(nuclearPed(1, sex = 2), 1, 3, 1),
    "Mother-son incest" = addChildren(nuclearPed(1, sex = 1), 3, 2, 1),
  )
}


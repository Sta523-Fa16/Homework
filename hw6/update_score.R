suppressMessages(library(jsonlite))
suppressMessages(library(rgdal))
suppressMessages(library(rgeos))
suppressMessages(library(sf))

options(nwarnings = 50)

args = commandArgs(trailingOnly = TRUE)

stopifnot(length(args) == 1)
team = args[1]

if (!file.exists("precincts.json"))
{
    stop("No precincts.json to score!")    
} 

pred = gBuffer(readOGR("precincts.json", layer="OGRGeoJSON", verbose=FALSE),
               width=0, byid=TRUE)

load(file = "pp.Rdata")

pp = spTransform(as(pp,"Spatial"), CRS("+proj=longlat +datum=WGS84"))


if (!"Precinct" %in% names(pp))
    stop("Predictions must have Precinct attribute")

score = 0
for(p in pp$Precinct)
{
    pp_i = which(pp$Precinct == p)
    pred_i = which(pred$Precinct == p)

    if (length(pred_i) == 0)
    {
        warning("Precinct ",p," missing from predicted boundaries.")
        score = score + suppressWarnings(gArea(pp[pp_i,],byid=FALSE))
    } else {
        score = score + suppressWarnings(gArea(gSymdifference(pred[pred_i,], pp[pp_i,], byid=FALSE)))
    }
}

score = score / suppressWarnings(gArea(pp))

writeLines(toJSON(list("score" = score), auto_unbox=TRUE), paste0(team,".json"))

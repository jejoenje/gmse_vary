### Testing loading sim results
### 
library(svMisc)
library(scales)
library(RColorBrewer)

source("gmse_apply_helpers.R")
source("helpers.R")

# system.time({
#   test = load_sims(outdir = "out/basic/20191118172059/", no_files = 1)
#   print(object.size(test),units="Mb")
# })
# 
# system.time({
#   test = load_sims(outdir = "out/basic/20191118172059/", no_files = 10)
#   print(object.size(test),units="Mb")
# })

system.time({
  test = load_sims(outdir = "out/varyBudget/20191118172124/", no_files = 100)
  print(object.size(test),units="Mb")
})

# system.time({
#   test = load_sims(outdir = "out/basic/20191118172059/")
#   print(object.size(test),units="Mb")
# })

res = test$sims
gmse_paras = test$para
rm(test); gc()

sims = len(res)
extinctions = as.vector(NULL)
for(i in 1:sims) {
  extinctions =  c(extinctions,sum(is.na(res[[i]])))
}
extinctions




y_lims = c(bufRange(min(extract_gmse(res, "resources"), na.rm=T), end = "lo"),
           bufRange(max(extract_gmse(res, "resources"), na.rm=T), end = "hi"))
par(mfrow=c(2,2))
plot_resource(res, type="resources", sumtype = "none", ylim = y_lims)
plot_resource(res, type="observations", sumtype = "none", ylim = y_lims)
plot_actions(res, type = "mean")
plot_yield(res, type = "stakeholder_mean")

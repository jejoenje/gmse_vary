library(GMSE)

### All of these only run with 100x100 default landscape dimensions

### 4 stakeholders, no public land:
test1 = gmse(time_max = 2, land_ownership = T, stakeholders = 8, plotting = FALSE, ownership_var = 0.9)
image(test1$land[[1]][,,3])
table(test1$land[[1]][,,3])/sum(table(test1$land[[1]][,,3]))

ov = seq(0,0.95,0.05)

out8 = as.data.frame(NULL)
stakeholders = 8
for(i in 1:length(ov)) {
    test = gmse(time_max = 2, land_ownership = T, stakeholders = stakeholders, plotting = FALSE, ownership_var = ov[i])
    out8 = rbind(out8, c(min(table(test$land[[1]][,,3])),max(table(test$land[[1]][,,3])),sum(table(test$land[[1]][,,3]))))
}
names(out8) = c("lmin","lmax","ltotal")

out16 = as.data.frame(NULL)
stakeholders = 16
for(i in 1:length(ov)) {
    test = gmse(time_max = 2, land_ownership = T, stakeholders = stakeholders, plotting = FALSE, ownership_var = ov[i])
    out16 = rbind(out16, c(min(table(test$land[[1]][,,3])),max(table(test$land[[1]][,,3])),sum(table(test$land[[1]][,,3]))))
}
names(out16) = c("lmin","lmax","ltotal")

out32 = as.data.frame(NULL)
stakeholders = 32
for(i in 1:length(ov)) {
    test = gmse(time_max = 2, land_ownership = T, stakeholders = stakeholders, plotting = FALSE, ownership_var = ov[i])
    out32 = rbind(out32, c(min(table(test$land[[1]][,,3])),max(table(test$land[[1]][,,3])),sum(table(test$land[[1]][,,3]))))
}
names(out32) = c("lmin","lmax","ltotal")

out8$rratio = (out8$lmax-out8$lmin)/out8$ltotal
out16$rratio = (out16$lmax-out16$lmin)/out16$ltotal
out32$rratio = (out32$lmax-out32$lmin)/out32$ltotal

par(mfrow=c(1,1))
yl = max(c(out8$rratio, out16$rratio, out32$rratio)) 
plot(out8$rratio, type = "l", ylim = c(0,yl))
lines(out16$rratio, col = "red")
lines(out32$rratio, col = "red")

### Tests with public land:
test2 = gmse(time_max = 2, land_ownership = T, stakeholders = 4, plotting = FALSE, public_land = 0.2)
image(test2$land[[1]][,,3])
table(test2$land[[1]][,,3])/sum(table(test2$land[[1]][,,3]))




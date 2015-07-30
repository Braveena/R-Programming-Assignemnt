add2 <- function(x, y) {
        x+y
}
        
add2(3,2)


greater10 <- function(vec) {
        sub_vec <- vector(mode="numeric", length=0)
        j <- 1
        for (i in seq_along(vec)) {
                if (vec[i] > 10) {
                        sub_vec[j] <- vec[i]
                        j <- j+1
                }
        }
        sub_vec
}

k <- c(1, 3, 11, 3, 45, 1)
greater10(k)





above10 <- function(v){
        s_v <- v > 10
        v[s_v]
        
}

k <- c(1, 3, 11, 3, 45, 1)
above10(k)





colMean <- function(mat, removeNA = TRUE) {
        means1 <- vector(mode="numeric", length=0)
        for (i in 1:ncol(mat)){
                means1[i] <- mean(mat[,i], na.rm = removeNA)
        }
        means1
}

y <- 10

f <- function(x) {
        y <- 2
        y^2 + g(x)
}

g <- function(x) {
        x*y
}

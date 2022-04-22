

?"..."
?methods

decorate_string0 <- function(pattern, ...) {
     paste0(pattern, paste(...), stringi::stri_reverse(pattern))
}

decorate_string <- function(pattern, ...) {
  strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
  paste0(pattern, paste(...), strReverse(pattern))
}


decorate_string(pattern = "123", "abc")            # "123abc321"
decorate_string(pattern = "123", "abc", "def")     # "123abc def321"
decorate_string(pattern = "123", c("abc", "def"))  # "123abc321" "123def321" (вектор длины 2)

decorate_string(pattern = "123", "abc", "def", sep = "+")    # "123abc+def321"
decorate_string(pattern = "!", c("x", "x"), collapse = "_")  # "!x_x!"
decorate_string(pattern = ".:", 1:2, 3:4, 5:6, sep = "&")    # ".:1&3&5:." ".:2&4&6:." (вектор длины 2)

#############################################################

values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
card_deck <- outer(values, suits, paste, sep = " of ")
roulette_values <- c("Zero!", 1:36)

generator <- function(set, prob = rep(1/length(set), length(set))) {
  function(n) sample(set, n, replace = T, prob = prob)
}


card_generator <- generator(card_deck)
coin_generator <- generator(c("Heads", "Tails"))
fair_roulette <- generator(roulette_values)


n <-  length(roulette_values)
rigged_prob <- c(2/(n+2), rep(1/(n+2), n - 1))
rigged_roulette <- generator(roulette_values, prob = rigged_prob)

card_generator(10)
coin_generator(5)

print(fair_roulette(50))
print(rigged_roulette(50))


help(norm)

# мое решение
1:5 + c(1:2,NA, NA, NA)
"%+%" <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  if(n1<n2){
    for(i in n2) x[i] <- NA
  } else if (n1>n2){
    for(j in n1) y[j] <- NA
  }else{ return (x+y)
  }
  return (x+y)
}
# лучшее решение
'%+%' <- function(x, y) {
    length(x) <- length(y) <- max(length(x), length(y))
    x + y
}
# альтернатива лучшего
'%+%' <- function(x, y) {
  length(x) <- max(length(x),length(y)) -> length(y)
    x + y
}



1:5 %+% 1:2
5 %+% c(2, 6)
5%+%5
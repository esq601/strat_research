lst1 <- out[[1]]

visited <- which(unlist(lst1$n) > 2)

unlist(lst1$a[visited])

dt <- data.table(s = lst1$s[visited],
                 n = unlist(lst1$n[visited]),
                 sa = (lst1$sa[visited]),
                 q = unlist(lst1$q[visited]))




val <- which.max(unlist(lst1$q))

which(unlist(lst1$n) > 2)

lst1$s[val]
lst1$n[val]

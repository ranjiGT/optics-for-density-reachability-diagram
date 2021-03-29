library(dbscan)
library(ggplot2)
library(purrr)
#---------------------------------------------
k <- 6
optics_clu <- optics(student, eps = Inf, minPts = k)
student_clu <- student %>%
  bind_cols(., tibble(R_Dist = optics_clu$reachdist))
student_clu <- student_clu[optics_clu$order,]
student_clu$Order <- 1:nrow(student_clu)

ggplot(student_clu, aes(x = Order, xend = Order, y = 0, yend = R_Dist)) +
  geom_segment() +
  labs(y = "Reachability-dist")
#------------------------------------------------------------
eps <- seq(1,5, by = 0.5)
reach_plot_list <- vector("list", length(eps))
clust_plot_list <- vector("list", length(eps))

for(i in seq_along(eps)){
  clu <- extractDBSCAN(optics_clu, eps_cl = eps[i])
  student_clu <- student %>%
    bind_cols(., tibble(Cluster = clu$cluster, R_Dist = optics_clu$reachdist)) %>%
    mutate(Cluster = factor(Cluster)) %>%
    mutate(Cluster = fct_recode(Cluster, "Noise" = "0"))
  student_clu <- student_clu[optics_clu$order,]
  student_clu$Order <- 1:nrow(student_clu)
  
  reach_plot_list[[i]] <- ggplot(student_clu, aes(x = Order, xend = Order, y = 0, yend = R_Dist))+geom_segment(aes(color = Cluster)) +
    geom_hline(yintercept = eps[i], linetype = 2) +
    labs(y = "Reachability-dist", title = str_c("eps = ", eps[i]))
  student_hull <- student_clu %>%
    split(.$Cluster) %>%
    purrr::map(~ slice(., chull(.$Exam1, .$Exam2))) %>%
    do.call("rbind", .)
  
  clust_plot_list[[i]] <- ggplot(student_clu, aes(Exam1, Exam2, color = Cluster, fill = Cluster))+geom_polygon(data = student_hull %>% filter(!Cluster == "Noise"), alpha = .5, color = "black")+geom_point(pch = 21) +
    scale_fill_discrete(drop = F) +
    scale_color_discrete(drop = F) +
    labs(title = str_c("eps = ", eps[i]))
}
reach_plot_list %>% walk(print)
clust_plot_list %>% walk(print) 
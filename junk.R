
plot(ip_lb_09_1, col = viridis(100, option = "A"))
plot(pa_lb_09["PA"], add = TRUE, col = viridis(2, option = "A"))

ggplot()+
  geom_raster(data = ip_lb_09_1 %>% as.data.frame(., xy = TRUE), aes(x = x, y = y, fill = layer))

plot(pa_lb_09["PA"], col = viridis(2, option = "A"))

plot(pa_lb_09["PA"])



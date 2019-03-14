l_max_age$AllSppMaxAge.0 %>% plot(add= T)

ch_rst %>% plot()

mask(ch_rst, l_max_age)

plot(PROP_WET_FOREST)


p <- xyFromCell(l_max_age$AllSppMaxAge.0, which(!is.na(values(l_max_age$AllSppMaxAge.0))))
a <- l_max_age$AllSppMaxAge.0

values(a) <- extract(PROP_WET_FOREST, p)
plot(a)

plot(stack(a, l_max_age))

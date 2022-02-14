max_noinf <- function(...) {
  if (sum(unlist(map(..., ~!is.na(.)))) > 0) {
    max(..., na.rm = T)
  } else{
    return(NA)
  }
}

gt_style <- function(gt) {
  gt %>%
    tab_options(table.align = "left") %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Calibri"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      ),
      locations = cells_column_labels(
        columns = gt::everything()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        color = "white",
        weight = px(0)
      ),
      locations = cells_body()
    )
}

theme_fc_map <- function(...) {
  #  theme_fc(...) %+replace%
  ggplot2::theme(
    line = ggplot2::element_blank(),
    rect = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.ticks.length =  ggplot2::unit(0, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    legend.key.size = ggplot2::unit(15, "pt"),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 7),
    complete = TRUE
  )
}

verify_isid <- function(df, x) {
  stopifnot(eeptools::isid(df, x))
  
  return(df)
}

tablist_qc <- function(x, ...) {
  # Simple crosstab function
  x %>%
    group_by(...) %>%
    summarize(`_freq` = n()) %>%
    # calculate percent
    mutate(`_perc` = round(100*(`_freq` / nrow(x)), digits = 1)) %>% 
    # order by values
    arrange(...) %>%
    as.data.frame() %>%
    pander::pandoc.table(split.tables = Inf, multi.line = TRUE)
}

# to remove repetitive code,
# general cleaning process for spatial data
clean_base_file <- function(df) {
  df %>% 
    unique() %>% 
    mutate(cd = ifelse(is.na(cd), cd2, cd),
           ownername = ifelse(is.na(ownername), str_trim(owner), str_trim(ownername))) %>% 
    mutate(borough = str_sub(cd, 1, 1)) %>% 
    select(-"cd2") %>% 
    verify(!is.na(borough)) %>% 
    filter(!(ownername %in% c("YOON, SOOK NYU", "YOON, SUK NYU", "ARA HOLDINGS OF NYU L",
                              "ARA HOLDINGS OF NYU LLC"))) %>% 
    filter(!(bbl == "3012920067")) %>% 
    # filter out polytechnic
    filter(!(str_detect(ownername, "POLYTECHNIC"))) %>% 
    mutate(bbl = case_when(bbl == "2057530140" ~ "2057520121",
                           TRUE ~ bbl)) %>% 
    mutate(hnum_lo = if_else(is.na(hnum_lo), hnum_hi, hnum_lo),
           address_form = case_when(!is.na(address) ~ address,
                                    !is.na(str_name) ~ str_c(paste(as.numeric(hnum_lo), 
                                                                   str_to_sentence(str_trim(str_name))), 
                                                             "New York", "New York", zip, sep = ", "),
                                    TRUE ~ NA_character_),
           address_form = case_when(hnum_lo == "00000001 1/2"  | hnum_lo == "1 1/2" ~ 
                                      "1 1/2 Waverly Place, New York, New York 10003",
                                    address_form == "425 Main street, New York, New York, 0" ~ 
                                      "425 Main street , New York, New York, 10044",
                                    address_form == "NA West tremont avenue, New York, New York, 0" ~ 
                                      "500 Mac Cracken Avenue, New York, New York, 10453",
                                    address_form == "NA River road, New York, New York, 0" ~
                                      "2 River Avenue, New York, New York, 10463",
                                    TRUE ~ address_form))
}

# function to clip and export by cd in 2021
# programmatically exporting plots
expt_cd <- function(cds, shp, cd_num, yr, path1, path2, parks) {
  cd_clip <- cds %>% 
    filter(boro_cd == cd_num)
  
  ext <- cd_clip %>% 
    st_bbox()
  
  
  cd_clip %>% 
    st_write(dsn = path(path1, paste0("cd_", cd_num, ".geojson")),
             delete_dsn = T,
             append = F)
  
  shp %>% 
    filter(year == yr) %>% 
    st_join(cd_clip, join = st_intersects) %>%
    st_write(dsn = path(path1, paste0("nyu_cd_", cd_num, "_", yr, ".geojson")),
             delete_dsn = T,
             append = F)
  
  # create an export plot
  shp %>% 
    filter(year == yr) %>% 
    ggplot() +
    geom_sf(data = parks,
            fill = "#777777", color = NA) +
    geom_sf(fill = "#9057FF", color = NA) +
    geom_sf(data = cds, color = "blue", fill = NA, size = 3) +
    coord_sf(xlim = c(ext[1], ext[3]), ylim = c(ext[2], ext[4]),
             expand = F) +
    theme_void() +
    theme(plot.background = element_rect(fill = '#424242'))
  
  ggsave(path(path2, paste0("nyu_cd_", cd_num, "_", yr, ".pdf")))
}
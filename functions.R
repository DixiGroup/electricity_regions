ua_monthes <<- c("січень", "лютий", "березень", "квітень", "травень", "червень", "липень", 
                "серпень", "вересень", "жовтень", "листопад", "грудень")
ua_monthes_gen <<- locale(date_names = "uk")$date_names$mon

# draws line plot ----
linegraph <- function(d, var, region, category, year1, month1, year2, month2) {
    if (region == "Вся Україна") {
        regions <- unique(d$oblast)
    } else {
        regions <- region
    }
    params <- switch (var,
                      debt_month_start = list(units = "грн", 
                                              title = "Сумарна заборгованість за електроенергію, грн", 
                                              legend_title = "Борг, грн",
                                              caption_part = "Заборгованість станом на початку місяця з "),
                      consumption_tkwth = list(units = "кВт/год", 
                                               title = "Споживання електроенергії, кВт/год", 
                                               legend_title = "Споживання, кВт/год",
                                               caption_part = "Місячне споживання електроенергії з "),
                      consumption_tuah = list(units = "грн", 
                                              title = "Вартість спожитої електроенергії, грн", 
                                              legend_title = "Споживання, грн",
                                              caption_part = "Вартість спожитої за місяць електроенергії з "),
                      payment_percent = list(units = "%",
                                             title = "Рівень оплати за спожиту електроенергію, %",
                                             legend_title = "Відсоток оплати",
                                             caption_part = "Відсоток оплати за електроенергію з "))
    date1_gen <- paste(ua_monthes_gen[month1], year1)
    date2_gen <- paste(ua_monthes_gen[month2], year2)
    if (category == "Усі") {
        category <- unique(d$consumer_cat)
        subt <- ifelse(grepl("ька",region), paste(region, "область"), region)
    } else {
        subt <- paste(ifelse(grepl("ька",region), paste(region, "область"), region), category, sep = ", ")
    }
    debt <- d %>% 
        dplyr::filter(to_date(year, month) <= to_date(year2, month2), 
                      to_date(year, month) >= to_date(year1, month1), 
                      oblast %in% regions, consumer_cat %in% category)
    if (length(unique(debt$consumer_cat)) > 1) {
        color_var <-  "consumer_cat"
    } else {
        color_var <- "consumer_type"
    }
    debt <- debt %>% 
        dplyr::mutate(consumer = !!sym(color_var)) %>% 
        dplyr::group_by(year, month, consumer)
    if (var != "payment_percent") {
        debt <- debt %>% 
            dplyr::summarise(total = sum(!!sym(var), na.rm = TRUE) * 1000) %>% 
            na.exclude()
    } else {
        debt <- debt %>% 
            dplyr::summarise(total = round(sum(paid_month_tuah) / sum(consumption_tuah)  * 100,2)) %>% 
            na.exclude()
    }
        
    number_of_periods <- debt %>% 
        select(month, year) %>% 
        distinct() %>% 
        nrow()
    if (number_of_periods < 9) {
        minor_grids <- element_blank()
    } else {
        minor_grids <- element_line(colour = "grey", linetype  = "dotted")
    } 
    
    n_br_y <- 5
    br_y = seq(min(0, min(debt$total)),max(debt$total), length.out = n_br_y)
    if (min(br_y) > 0) {
        br_y <- c(0,br_y)
    }
    y_limits <- create_limits(debt$total)
    debt$year <- as.character(debt$year)
    debt$month <- as.character(debt$month)
    debt$month[nchar(debt$month) == 1] <- paste0("0", debt$month[nchar(debt$month) == 1])
    subtitle <- paste0("\n", params$caption_part, 
                       date1_gen, " до ", date2_gen, ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")

    debt$date <- as.Date(paste(debt$year, debt$month, "01"), format = "%Y %m %d")
    pl <- ggplot(debt, aes(x = date, y = total, col = consumer)) +
        geom_line(alpha = 0.8, size = 1.2) +
        ggtitle(params$title, subtitle = subt) +
        scale_x_date(labels = date_format, expand = c(0,0), 
                     date_breaks = paste(((number_of_periods - 1) %/% 8 )+ 1, "month")  ) +
        scale_y_continuous(#trans = tr_cbrt, 
                           #minor_breaks = NULL,
                           #breaks = pr_br,
                           breaks = br_y,
                           limits = y_limits,
                           labels = function(br){
                               #br <- pr_br
                               all_labels <- sapply(br, function(d) {
                                   millions_n <- round(d / 1000000, 1)
                                   millions <- format(millions_n, decimal.mark = ",", big.mark = "")
                                   billions_n <- round(d / 1000000000, 1)
                                   billions <- format(billions_n, decimal.mark = ",", big.mark = "")
                                   ifelse(abs(billions_n) >= 1, paste0(billions, " млрд"), ifelse(abs(millions_n) > 0, paste0(millions, " млн"), ifelse(d >= 1000, paste0(round(d, 2), " тис."), round(d,1)  )))})
                               all_labels
                           }
        ) +
        theme(
            plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
            axis.ticks = element_blank(), axis.title = element_blank(),
            rect = element_rect(fill = "white"),
            legend.position = "bottom",
            text = element_text(family = "Bliss Pro Light"),
            axis.text.y = element_text(size = 8, vjust = 1),
            axis.text.x = element_text(vjust = 0),
            #panel.grid.minor.y = element_blank(),
            #panel.grid.minor.x = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_line(colour = "grey", linetype  = "dotted", size = 0.3),
            panel.grid.minor.x =  minor_grids,
            panel.grid.minor.y = element_line(colour = "grey", linetype  = "dotted"),
            plot.margin = unit(c(0.2,0.8,0.2,0.4), "cm"),
            #panel.background = element_rect(fill =  "#deebf7"),
            strip.background = element_rect(fill = "#c6dbef"),
            legend.title = element_blank()) +
        labs(caption = subtitle)
}


# draws bar plot by categories or types  ------
bargraph <- function(d, var, region, category, year1, month1, year2, month2) {
    if (region == "Вся Україна") {
        regions <- unique(d$oblast)
    } else {
        regions <- region
    }
    params <- switch (var,
                      debt_month_start = list(units = "грн", 
                                              title = "Сумарна заборгованість за електроенергію, грн", 
                                              legend_title = "Борг, грн",
                                              caption_part = "Заборгованість станом на початку місяця за "),
                      consumption_tkwth = list(units = "кВт/год", 
                                               title = "Споживання електроенергії, кВт/год", 
                                               legend_title = "Споживання, кВт/год",
                                               caption_part = "Місячне споживання електроенергії за "),
                      consumption_tuah = list(units = "грн", 
                                              title = "Вартість спожитої електроенергії, грн", 
                                              legend_title = "Споживання, грн",
                                              caption_part = "Вартість спожитої за місяць електроенергії за "),
                      payment_percent = list(units = "%",
                                             title = "Рівень оплати за спожиту електроенергію, %",
                                             legend_title = "Відсоток оплати",
                                             caption_part = "Відсоток оплати за електроенергію за "))
    date1_nom <- paste(ua_monthes[month1], year1)
    date2_nom <- paste(ua_monthes[month2], year2)
    if (category == "Усі") {
        category <- unique(d$consumer_cat)
        subt <- ifelse(grepl("ька",region), paste(region, "область"), region)
    } else {
        subt <- paste(ifelse(grepl("ька",region), paste(region, "область"), region), category, sep = ", ")
    }
    d$date <- paste(ua_monthes[d$month], d$year)
    debt <- d %>% 
        dplyr::filter(to_date(year, month) == to_date(year2, month2) | to_date(year, month) == to_date(year1, month1), 
                      oblast %in% regions, consumer_cat %in% category)
    if (length(unique(debt$consumer_cat)) > 1) {
        color_var <-  "consumer_cat"
    } else {
        color_var <- "consumer_type"
    }
    debt <- debt %>% 
        dplyr::mutate(consumer = !!sym(color_var)) %>% 
        dplyr::group_by(date, consumer, year, month) 
    
    if (var != "payment_percent") {
        debt <- debt %>% 
            dplyr::summarise(total = sum(!!sym(var), na.rm = TRUE) * 1000) %>% 
            dplyr::arrange(year, month, total) %>% 
            na.exclude()
    } else {
        debt <- debt %>% 
            dplyr::summarise(total = round(sum(paid_month_tuah) / sum(consumption_tuah)  * 100,2)) %>% 
            dplyr::arrange(year, month, total) %>% 
            na.exclude()
    }  
    colors <- c("#fdd0a2", "#fd8d3c")
    names(colors) <- unique(debt$date)
    #color <- unique(debt$date)
    #names(colors) <- c("lightcoral", "firebrick4")
    debt$date <- factor(debt$date, levels = unique(debt$date), ordered = T)
    debt <- debt %>% 
        arrange(desc(year), desc(year), total)
    debt$consumer <- sapply(debt$consumer, split_to_rows)
    debt$consumer <- factor(debt$consumer, levels = unique(debt$consumer), ordered = T)
    n_br_y <- 8
    br_y = seq(min(0, min(debt$total)),max(debt$total), length.out = n_br_y)
    if (min(br_y) < 0) {
        br_y <- c(0,br_y)
    }
    y_limits <- create_limits(debt$total)
    subtitle <- paste0("\n", params$caption_part, 
                       date1_nom, " та ", date2_nom, ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")
    pl <-  debt %>% 
        ggplot(aes(x = consumer, fill = date, y = total)) +
        ggtitle(params$title, subtitle = subt) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.65), alpha = 0.9, width = 0.6) + 
        coord_flip() +
        scale_fill_manual(values = colors) +
        scale_x_discrete(expand = c(0,0)) +
        scale_y_continuous(#trans = tr_cbrt, 
            minor_breaks = NULL,
            expand = c(0,0),
            breaks = br_y,
            limits = y_limits,
            #breaks = pr_br,
            labels = function(br){
                #br <- pr_br
                all_labels <- sapply(br, function(d) {
                    millions_n <- round(d / 1000000, 1)
                    millions <- format(millions_n, decimal.mark = ",", big.mark = "")
                    billions_n <- round(d / 1000000000, 1)
                    billions <- format(billions_n, decimal.mark = ",", big.mark = "")
                    ifelse(abs(billions_n) >= 1, paste0(billions, " млрд"), ifelse(abs(millions_n) > 0, paste0(millions, " млн"), ifelse(d >= 1000, paste0(round(d, 2), " тис"),   ifelse(d > 0, round(d,1), ifelse(min(br) == 0, "0", ""))  )))})
                all_labels
            }) +
        
        theme(
            plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
            axis.ticks = element_blank(), axis.title = element_blank(),
            rect = element_rect(fill = "white"),
            legend.position = "bottom",
            text = element_text(family = "Bliss Pro Light"),
            axis.text.y = element_text(size = 10, hjust = 1, margin = NULL),
            #axis.text.x = element_text(vjust = 0),
            #panel.grid.minor.y = element_blank(),
            #panel.grid.minor.x = element_blank(),
            panel.background = element_blank(),
            plot.margin = unit(c(0.2,1,0.2,0.2), "cm"),
            panel.grid.major = element_line(colour = "grey", linetype  = "dotted"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_line(colour = "grey", linetype  = "dotted"),
            #panel.background = element_rect(fill =  "#deebf7"),
            strip.background = element_rect(fill = "#c6dbef"),
            legend.title = element_blank()) +
        labs(caption = subtitle)
    
}

# converts year and month to date ----
to_date <- function(year, month) {
    month <- as.character(month)
    month[nchar(month) == 1] <- paste0("0", month[nchar(month) == 1])
    as.Date(paste(year, month, "01", sep = "-"))
}

# replaces first letter in string with capital ----
capitalize <- function(s) {
    first <- substr(s, 1, 1)
    other <- substr(s, 2, nchar(s))
    paste0(toupper(first), other)
}

# beatify long string by splitting them on different rows -----
split_to_rows <- function(s, threshold = 20) {
    s <- as.character(s)
    if (nchar(s) >= threshold) {
        s <- str_trim(s)
        positions <- str_locate_all(s, "\\s")[[1]]
        if (nrow(positions) > 0) {
            half <- round(nchar(s) / 2)
            distance <- abs(half - positions[,"start"])
            row_number <- which(distance == min(distance))
            start = positions[row_number, 'start'] - 1
            end = positions[row_number, 'end'] + 1
            s <- paste(str_sub(s, 1, start), str_sub(s, end, nchar(s)), sep = "\n")
        }
    } 
    s
}

# month format
date_format <- function(br) {
    monthes <- as.integer(format(br, format = "%m"))
    years <- as.integer(format(br, format = "%Y"))
    paste(str_sub(ua_monthes[monthes], 1, 3),years)
}

# add 0 to limits 
create_limits <- function(v) {
    lim <- range(v)
    if (lim[1] > 0) {
        lim[1] <- 0
    }
    lim
}
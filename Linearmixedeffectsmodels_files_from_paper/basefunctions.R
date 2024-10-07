.base_lme <- function(in.df = "lme.data", in.groups=c("cohort", "variable"), 
  in.fixed.f = 'value ~ time', in.random.f = '~ 1 | case_id', in.ctrl = '', ...) {

  require(dplyr)
  require(nlme)

  # set default lme/REML control parameters with higher Ns for all iterations
  # use 'optim' optimizer instead of default -- generally returns fewer convergence errors and has
  #   minimal effect on estimated parameters; see:
  #     http://stats.stackexchange.com/questions/40647/lme-error-iteration-limit-reached
  if (in.ctrl == '') {
    ctrl.optim <- lmeControl(opt='optim', maxIter=5000, msMaxIter=5000, niterEM=2500, mxMaxEval=20000)   
  } else { 
    ctrl.optim <- in.ctrl
  }

  # convert grouping variables to symbols
  #   http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
  dots <- lapply(in.groups, as.symbol)

  # convert name of data.frame to object
  f.lme.data <- get(in.df)  
  
  print("  ... running lme ...")
  f.lmes <- f.lme.data %>% group_by_(.dots=dots) %>% do(
    model = try(lme(fixed = as.formula(in.fixed.f), random = as.formula(in.random.f), data = ., control = ctrl.optim, ...))
  ) 

  # add formulae and data info back to lme$call
  # name data in standard format: in.df..cohort..variable (necessary for use with covariatemeans)
  for (i in 1:dim(f.lmes)[1]) {
    f.lmes$model[[i]]$call$fixed <- formula(in.fixed.f)
    f.lmes$model[[i]]$call$random <- formula(in.random.f)
    f.lmes$model[[i]]$call$data  <- as.symbol(
      make.names(paste(in.df, f.lmes[[in.groups[1]]][i], f.lmes[[in.groups[2]]][i], sep="..")))
  }

  # report on try - catch errors (missing data/fail to converge, etc.) - add column to omit
  f.lmes$omit <- NA 
  for (i in 1:dim(f.lmes)[1]) { 
    if(class(f.lmes$model[[i]]) == "try-error") {
      f.lmes$omit[[i]] <- "omit"
      print(paste0("Cohort ", f.lmes$cohort[i], " variable ", f.lmes$variable[i], " lme error:\n"))
      print(f.lmes$model[[i]])
    }
  }
  
  f.lmes
}


## .lme_summ - simple extraction of lme fixed effects coefficients and estimates
#     extracts table of Fixed.Effects
.lme_summ <- function(in.lme_objs.df, in.groups=c("cohort", "variable")) {

  require(dplyr) 
  require(nlme) 

  dots <- lapply(in.groups, as.symbol)

  f.summ_tab.df <- in.lme_objs.df %>% ungroup() %>% group_by_(.dots = dots) %>% do(
    data.frame(
      Fixed.Effects = names(fixed.effects(.$model[[1]])),
      summary(.$model[[1]])$tTable,
      intervals(.$model[[1]])$fixed
    )
  )
  f.summ_tab.df
}


### .lmePlots - lme fitted trend lines (from predictmeans::covariatemeans package
#    takes dplyr::do data.frame of lme model objects (output of .base_lme)
#      and creates one plot per model (row)
#    in.points and in.ci create plot with individual data points and shaded CIs for fitted line
.lmePlots <- function(in.lme_objs.df,  # input tbl_df
    in.x_col="x", in.y_col="y",  # columns in original input data with x and y values
    in.groups = c("cohort", "variable"),  # names of column with grouping variables in in.lme_objs.df 
    in.lme_group=NULL,   # column with grouping variable used for lme (term in model -- use NULL if no groups)
    in.xlab = "time",  # label for x axis
    in.ylab_col = "variable_desc",   # name of column in input data containing variable description
    in.colors = c("blue", "red", "darkgreen", "orange", "cyan"),  # colors for each group
    in.title_pre = "",  # optional prefix to add to title
    in.line_wd = 1.5,  # line width
    in.ci=TRUE,  # if line plot should include colored CIs
    in.points=TRUE,   # if observed values of points should be overlaid on line plots
    in.pt_size=0.8,  # factor to manually scale point size
    in.jitterv=0,  # point jitter value
    in.trellis=FALSE,  # if plot should be split into facets based on groups 
    in.legend_pos="bottom"  # legend position ("bottom" or "right")
    ) {

    require(dplyr); require(ggplot2); require(predictmeans);

    print("  ... creating ggplots ...")
    pb <- txtProgressBar(min=0, max=nrow(in.lme_objs.df), style=3)

    # plotting function
    p.func <- function(plot.df, lme_group=in.lme_group, plot_colors=in.colors,
        groups_title, plot_xlab=in.xlab, y_axis_lab.col=in.ylab_col, 
        line_wid=in.line_wd, ci=in.ci, points=in.points, jitterv=in.jitterv,
        pt_size=in.pt_size, trellis=in.trellis, legend_pos = in.legend_pos) {

        pl <- ggplot(data=(plot.df %>% filter(!is.na(Mean))),
            aes(x=xvar, y=Mean, colour=factors)) +
            geom_line(size=line_wid) +
            ylab(plot.df[[y_axis_lab.col]][1]) + xlab(in.xlab) +
            ggtitle(groups_title)

        if(!(is.null(lme.group))) {
            pl <- pl + scale_colour_manual(name=in.lme_group, values=in.colors)
	} else {
            pl <- pl + labs(colour=in.lme_group, guide=FALSE)
 	}

        if(in.ci) {
            pl <- pl + geom_smooth(data=(plot.df %>% filter(!is.na(Mean))),
                aes(ymin=LL, ymax=UL, fill=factors), alpha=0.2, stat="identity") +
                scale_fill_manual(values=in.colors, guide=FALSE) 
        }

        if(in.points) {
            pl <- pl + geom_point(data=(plot.df %>% filter(!is.na(yvar))),
                aes(x=xvar, y=yvar), size=in.pt_size, 
                position=position_jitter(width=jitterv, height=jitterv)) 
        }

	if(legend_pos == "bottom") {  
	    pl <- pl + theme(legend.position="bottom")
        } else if (legend_pos != "right") { 
	    stop("invalid option for legend position...") 
        }

        if(in.trellis) {
            pl <- pl + facet_wrap(~factors)
        }

        pl
    } 


    # iterate through models (rows) in input data.frame
    #   find mean predicted value with CIs by group using covariatemeans
    #   create list of ggplots for each
    plot.ls <- list()

    for (i in 1:nrow(in.lme_objs.df)) {

	model.i <- in.lme_objs.df$model[[i]]
	raw_data.i <- in.lme_objs.df$model[[i]]$data

        # this statement places the data into the global environment 
        # (needed for covariate means to correctly grab data for plots)
        assign(as.character(model.i$call$data), model.i$data, envir = .GlobalEnv)

        # values from columns needed for plot titles and axis labels
        tmp_groups.df <- data.frame(lapply(raw_data.i[1, in.groups], as.character), 
            stringsAsFactors=FALSE)
	tmp_names <- paste(tmp_groups.df[1,], collapse = " ")

        # prep plot.df -- data.frame with in.groups, values, fitted values
        if(is.null(in.lme_group)) {
	    cmeans <- covariatemeans(model.i,
                modelterm=in.lme_group, covariate=in.x_col, trillis=FALSE, 
                newwd=FALSE, mtitle=paste(tmp_names,
                    "Fitted and observed relationship with 95% CI", sep="\n"))
	} else {    
	    cmeans <- covariatemeans(model.i,
                modelterm=in.lme_group, covariate=in.x_col, trillis=FALSE, 
                newwd=FALSE, mtitle=paste(tmp_names,
                    "Fitted and observed relationship with 95% CI", sep="\n"))
	}

	# combined predicted values from covariatemeans with observed data
	#   frame, matching needed columns and column names
        cmeans.df <- cmeans$data
        obs.df <- raw_data.i %>% select_("xvar"=in.x_col, "yvar"=in.y_col)

	if (is.null(in.lme_group)) {
            obs.df$factors <- as.factor(1)
	} else {
            obs.df$factors <- raw_data.i[[in.lme_group]]
	}
        tmp_plot.df <- bind_rows(cmeans.df, obs.df)

        tmp_plot.df <- bind_cols(tmp_plot.df, 
            tmp_groups.df[rep(seq_len(nrow(tmp_groups.df)), each=nrow(tmp_plot.df)),,drop=FALSE])

	# add y axis variable description column
	tmp_plot.df <- cbind(tmp_plot.df, data.frame(var_desc=raw_data.i[[in.ylab_col]][1]))


	plot.ls[[i]] <- p.func(tmp_plot.df, lme_group=in.lme_group, 
            plot_colors=in.colors, groups_title=paste(in.title_pre, tmp_names),
            plot_xlab=in.xlab, y_axis_lab.col="var_desc", ci=in.ci, points=in.points,
            pt_size=in.pt_size, jitterv=in.jitterv, trellis=in.trellis, legend_pos=in.legend_pos)

        # clean-up temporary global environment objects
        #rm(list=ls(model.i$call$data)

	setTxtProgressBar(pb, i)
    }


    out.lme_objs.df <- in.lme_objs.df %>% select(-model)
    out.lme_objs.df$plot <- plot.ls

    close(pb)

    return(out.lme_objs.df)

}


## .lme_nVisGrobs - create tableGrobs of Ns (subjects person visits) by grouping variable 
#      takes dplyr::do tbl_df of lme objects, retruns dplyr::do tbl_df of table grobs
#      summarizing N sujbects, N person visits, Mean visits per subject split by in.lme_group
#    NOTE: grid v. 3.2+ no longer supports following syntax for font size
#      gpar.coretext=gpar(fontsize=7), gpar.coltext=gpar(fontsize=8, fontface='bold'), show.rownames=F)
#   - use in.font_adj to adjust text relative to full size

.lme_nVisGrobs <- function(in.lme_objs.df, in.groups=c("cohort", "variable"), in.lme_group="group",
    in.font_adj=0.8) {

  require(grid)
  require(gridExtra)
  require(ggplot2)
 
  dots <- lapply(in.groups, as.symbol)
  
  summTheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex=in.font_adj)),
    colhead = list(fg_params=list(cex=in.font_adj)),
    rowhead = list(fg_params=list(cex=in.font_adj)))

  f.lme_nVisGrobs.df <- in.lme_objs.df %>% ungroup() %>% group_by_(.dots=dots) %>% do(
    nVisGrob = tableGrob(
      (.$model[[1]]$data %>% group_by_(in.lme_group) %>% summarise(
        N.cases=n_distinct(case_id), N.case.visits=n(), visits.per.case=round(N.case.visits/N.cases, 2))),
      theme=summTheme)
  )

  f.lme_nVisGrobs.df 
}


## .lme_coeffGrobs - create table grobs with lme coeffients, SEs, p values from summary tTable
#      takes dplyr::do tbl_df of lme objects, retruns dplyr::do tbl_df of table grobs
#      summarizing N sujbects, N person visits, Mean visits per subject split by in.lme_group
#    NOTE: grid v. 3.2+ no longer supports following syntax for font size
#      #gpar.coretext=gpar(fontsize=7), gpar.coltext=gpar(fontsize=8, fontface='bold'), show.rownames=F)
.lme_coeffGrobs <- function(in.lme_objs.df, in.groups=c("cohort", "variable"), in.lme_group="group", 
    in.font_adj=0.8) {

  require(grid)
  require(gridExtra)
  require(ggplot2)

  summTheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex=in.font_adj)),
    colhead = list(fg_params=list(cex=in.font_adj)),
    rowhead = list(fg_params=list(cex=in.font_adj)))
 
  dots <- lapply(in.groups, as.symbol)

  f.lme_coeffGrobs.df <- in.lme_objs.df %>% ungroup() %>% group_by_(.dots=dots) %>% do(
    tTabGrob = tableGrob(
      (data.frame(Fixed.Effects = names(fixed.effects(.$mod[[1]])), summary(.$mod[[1]])$tTable) %>% 
         select(Fixed.Effects, Value, Std.Error, p.value) %>%
          mutate(Value = round(Value, 2), 
            Std.Error = ifelse(Std.Error < 0.001, signif(Std.Error, 3), sprintf("%.3f", Std.Error)),
            p.value = ifelse(p.value < 0.001, signif(p.value, 3), sprintf("%.3f", p.value))) %>%
          rename(SE = Std.Error)),
      theme=summTheme)
  )

  f.lme_coeffGrobs.df 
}



### .gg_mult_page - wrapper function for printing multi-page ggplots to file using gridExtra in a loop
#     takes input data.frame of ggplots from dplyr::do loop,
#       plot layout number parameters (numeber of rows/columns (n.rows/n.cols) per page)
#       and pdf file parameters (height, width, filename
#
#     NOTE: requires gridExtra 2.0.0 (grid.arrange changed parameter name from "main" to "top")
#
#   example:
#     eg_plots <- diamonds %>% tbl_df() %>% rename(variable=cut) %>% group_by(variable) %>% 
#       do(plot=qplot(carat, price, main=.$variable[1], data=.))
#     .gg_mult_page(eg_plots, n.col=2, n.row=3, out.page_title="diamonds test", out.filename="diamonds_plots_test.pdf")


.gg_mult_page <- function(in.plots.df, n.col=1, n.row=3, out.page_title="", out.pdf_height=11, out.pdf_width=8.5, 
                         out.filename="plots_out.pdf") {
  require(dplyr); require(gridExtra); require(ggplot2);
  
  plots.per.page <- n.col * n.row
  var.sub.vars <- split(in.plots.df$variable, ceiling(seq_along(in.plots.df$variable)/plots.per.page))
  
  pdf(out.filename, height=out.pdf_height, width=out.pdf_width)
  
  print("  ... writing plots to file ...")
  pb <- txtProgressBar(min=0, max=length(var.sub.vars), style=3)
  
  for (i in 1:length(var.sub.vars)){
    page.title <- paste(out.page_title, " - Pg.", i)
    
    p.plots <- filter(in.plots.df, variable %in% var.sub.vars[[i]])
    p.plots_list <- p.plots$plot; names(p.plots_list) <- p.plots$variable
    
    plot.args <- c(p.plots_list, list(nrow=n.row, ncol=n.col, top=page.title))
    do.call(gridExtra::grid.arrange, plot.args)
    
    setTxtProgressBar(pb, i)
  }
  
  dev.off()
  close(pb)
}

#' Generate gprofiler barplot
#'
#' Function to make a bar plot of top enrichments (sorted by pval) in gprofiler output
#' 
#' @param DF dataframe. Dataframe of gprofiler 
#' @param num_categories int. Number of gprofiler results to show. Default is all.
#' @param plot_title chr. Name of plot.
#' @param facet logical. Facet plot by ...
#'
#' @return
#' @export
#'

gen_gprofiler_barplot = function(DF, num_categories=nrow(DF), plot_title="", facet=FALSE){
  
  library(scales)
  library(gridExtra)
  library(data.table)
  library(plyr)

  if(is.null(DF) == FALSE){
    
    num_categories = as.numeric(num_categories)
    # DF$region_name = as.character(DF$region_name)
    # DF$dir_label = as.character(DF$dir_label)
    
    DF$p_value = as.numeric(DF$p_value)
    
    if(facet == FALSE){
      
      DF = DF[DF$significant == "TRUE", ]
      
      DF = DF[,colnames(DF) %in% c("p_value", "term_name", "precision", "source", "recall")]
      DF = ddply(DF, .(term_name, source), summarise, p_value=mean(p_value), precision=mean(precision), recall=mean(recall))
      
      DF = DF[order(DF$p_value),] # sort results by pvalue
      
      if(length(rownames(DF)) >= num_categories){
        DF = DF[1:num_categories, ]
      }
      
      ymax = max(abs(log10(DF["p_value"])))
      
      return(ggplot(DF, aes(x=reorder(term_name, -p_value), y=abs(log10(p_value)), fill = source)) +
              theme_light() +
              geom_col() + 
              coord_flip() +
              labs(x="", y="log10 adjusted p-value", title=plot_title, fill="Adjusted p-value", guide="colourbar") +
              scale_x_discrete(labels = wrap_format(60)) +
              ylim(0,ymax+5) +
              geom_text(data=DF, aes(label=paste("recall = ",paste0(round(recall,2)*100,"%")),vjust=0.5, hjust=-0.1, colour=source),show.legend = FALSE) +
              theme(plot.title = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20))))
    }
    
    if(facet == TRUE){
      
      DF = DF[DF$significant == "TRUE", ]
      
      DF = DF[,colnames(DF) %in% c("p_value", "term_name", "precision", "region_name", "source", "intersection_size", "dir_label")]
      DF = ddply(DF, .(term_name, region_name, source, dir_label), summarise, p_value=mean(p_value), precision=mean(precision), intersection_size=mean(intersection_size))
      
      DF = DF[order(DF$p_value),] # sort results by pvalue
      DF = DF %>% group_by(region_name, dir_label) %>% top_n(n=num_categories, wt=p_value)
      
      p=ggplot(DF, aes(x=reorder(term_name, -p_value), y=abs(log10(p_value)), fill = source)) +
        theme_grey(base_size = 8) +
        facet_wrap(region_name~dir_label, scales="free_y", ncol = 2) +
        theme_light() +
        geom_col() +
        coord_flip() +
        labs(x="", y="log10 adjusted p-value", title=plot_title, fill="", guide="colourbar") +
        scale_x_discrete(labels = wrap_format(100)) +
        theme(legend.position='top') +
        theme(legend.title = element_text("")) +
        theme(plot.title = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)))
      # scale_y_discrete(labels = function(labels) {
      #   fixedLabels <- c()
      #   for (l in 1:length(labels)) {
      #     fixedLabels[l] <- paste0(ifelse(l %% 2 == 0, '', '\n'), labels[l])}
      #   return(fixedLabels)
      #   })
      return(p)
    }
  }
  else{
    print("empty gost result")
  }
}

variable="Part_lab"

graph_solution=dplyr::filter(df.melted_i, country %in% c('FRA'),temps%in%c(2000:2016))[c("country","temps",variable)]%>%full_join(solution)%>%arrange(temps)%>%rename(Legende=country)
graph_solution$Legende[graph_solution$Legende=="FRA"]<-"France"
graph_solution$Legende[graph_solution$Legende=="FRA_Synth"]<-"France Synthétique"
graph_solution
style=c("solid","dashed")
color=c("red","darkred")

ggplot(data=graph_solution,aes(x=temps, group=Legende, y=Part_lab))+ geom_line(aes(linetype=Legende,color=Legende),show.legend = TRUE)+ geom_point(show.legend = TRUE) + scale_linetype_manual(values=style)+scale_color_manual(values=color)+theme_minimal()+geom_vline(xintercept = 2012.5, colour = 'grey', size = 1, linetype = 'dashed')+ labs(title = "Trajectoire de la part du travail dans la valeur ajoutée",
                                                                                                                                                                                                                                                                                                                                                    x = "Année",
                                                                                                                                                                                                                                                                                                                                                    y = "Part de la valeur ajoutée (en %)")+ylim(c(0.62,0.72))
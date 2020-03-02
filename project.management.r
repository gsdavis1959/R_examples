# Import plotrix package
library(plotrix)

# Set date format
Ymd.format <- "%Y/%m/%d"

# Define tasks, starting times and ending times 
gantt.info <- list(labels =
                     c("Define",
                       "Measure",
                       "Analyze",
                       "Improve",
                       "Control"),
                   starts = as.POSIXct(strptime(
                     c("2020/01/01",
                       "2020/02/01",
                       "2020/04/01",
                       "2020/06/01",
                       "2020/11/01"),
                     format = Ymd.format)),
                   ends = as.POSIXct(strptime(
                     c("2020/01/31",
                       "2020/03/31",
                       "2020/05/31",
                       "2020/10/31",
                       "2020/12/31"),
                     format = Ymd.format)))


# Define vertical grids positions
vgridpos <- as.POSIXct(strptime(c(
  "2020/01/01",
  "2020/02/01",
  "2020/03/01",
  "2020/04/01",
  "2020/05/01",
  "2020/06/01",
  "2020/07/01",
  "2020/08/01",
  "2020/09/01",
  "2020/10/01",
  "2020/11/01",
  "2020/12/01"),
  format = Ymd.format))

# Define vertical grids labels
vgridlab <- month.abb

# Build Gantt chart
gantt.chart(gantt.info,
            main = "DMAIC Project - Gantt Chart 2020",
            priority.legend = FALSE,
            vgridpos = vgridpos,
            vgridlab = vgridlab,
            hgrid = TRUE,
            half.height = 0.45,
            border.col = "black",
            taskcolors = c("blue", "blue", "blue", "blue", "blue"))

# Import timelineS package
library(timelineS)

# Build data frame with deliverables and their corresponding dates
project <- data.frame(Deliverables = c("Project Start",
                                       "Diagnosis Report",
                                       "Pre-feseability Studies",
                                       "Feseability Studies",
                                       "Project Planning",
                                       "Detailed Project Report",
                                       "Project Network for Implementation",
                                       "Final Report",
                                       "Presentation to Stakeholders"),
                      Deliverables_Dates = as.Date(c("2020-01-01",
                                                     "2020-02-01",
                                                     "2020-02-15",
                                                     "2020-03-15",
                                                     "2020-04-01",
                                                     "2020-05-01",
                                                     "2020-06-01",
                                                     "2020-08-01",
                                                     "2020-08-15")))

# Build project deliverables timeline
timelineS(project,
          main = "Project Deliverables Timeline",
          xlab = "Year 2020",
          buffer.days = 35,
          scale = "month",
          scale.format = "%b",
          label.direction = "downup",
          label.cex = 0.75)
box()

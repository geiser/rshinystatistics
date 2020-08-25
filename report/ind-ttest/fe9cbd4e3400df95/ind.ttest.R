# title: "Summary of independent t-test for `dimensao8.fss`,`dimensao7.fss`,`dimensao5.fss`,`dimensao2.fss` with `cenário`"
# author: Geiser C. Challco <geiser@usp.br>
# comment: This file is automatically generate by Shiny-Statistic app (https://statistic.geiser.tech/)
#         Author - Geiser C. Challco <geiser@usp.br>
#
#         Shiny-Statistic is distributed in the hope that it will be useful,
#         but WITHOUT ANY WARRANTY; without even the implied warranty of
#         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#         GNU General Public License for more details.
#
#         You should have received a copy of the GNU General Public License.
#         If not, see <https://www.gnu.org/licenses/>.
#
if (!'remotes' %in% rownames(installed.packages())) install.packages('remotes')
if (!"rshinystatistics" %in% rownames(installed.packages())) {
  remotes::install_github("geiser/rshinystatistics")
} else if (packageVersion("rshinystatistics") < "0.0.0.9028") {
  remotes::install_github("geiser/rshinystatistics")
}

wants <- c('ggplot2','ggpubr','rshinystatistics','utils')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(ggpubr)
library(ggplot2)
library(rshinystatistics)

## Loading initial data

data <- read.csv("/home/rstudio/rshinystatistics/report/ind-ttest/fe9cbd4e3400df95/data.csv")
rownames(data) <- data[["nome"]]

### Setting identificator, dependent and independent variables

dvs <- c("dimensao8.fss","dimensao7.fss","dimensao5.fss","dimensao2.fss")
iv <- "cenário"
wid <- "nome"
dat <- set_datatable(data, dvs, "var")

## Check Assumptions

### Removing outliers from the data

outliers <- list(
"dimensao8.fss" = c("P091","P122","P179")
)
rdat <- remove_from_datatable(dat, outliers, wid, "var")

### Normality assumption

#### Applying transformation for skewness data when normality is not achieved


##### Applying normality in dimensao8.fss to reduce skewness
density_res_plot(rdat,"dimensao8.fss",iv,dv.var="var")
rdat[["dimensao8.fss"]] <- sqrt(rdat[["dimensao8.fss"]])
density_res_plot(rdat,"dimensao8.fss",iv,dv.var="var")



##### Applying normality in dimensao2.fss to reduce skewness
density_res_plot(rdat,"dimensao2.fss",iv,dv.var="var")
rdat[["dimensao2.fss"]] <- 1/(max(rdat[["dimensao2.fss"]]+1) - rdat[["dimensao2.fss"]])
density_res_plot(rdat,"dimensao2.fss",iv,dv.var="var")

#### Removing data that affect normality (extreme values)

non.normal <- list(

)
sdat <- remove_from_datatable(rdat, non.normal, wid, "var")

#### Checking normality assumption in the groups

normality_test_per_group(sdat, dvs, iv, dv.var = 'var')

### Homogeneity of variance assumption

homogeneity_test(sdat, dvs, iv, dv.var = 'var')

## Computation t-test and effect size

res <- ind_ttest(sdat, dvs, iv, "two.sided", FALSE, FALSE, dv.var = 'var', as.list = T)
(res$t.test)

## Descriptive Statistic and T-Test Plots

descriptive_statistics(sdat, dvs, iv, "common", "var")



### T-Test plot for the dependent variable "dimensao8.fss"
ggPlotTTest(sdat[which(sdat[["var"]] == "dimensao8.fss"),], "cenário", "dimensao8.fss"
, res$tt[["dimensao8.fss"]], c("jitter"), font.label.size=10)


### T-Test plot for the dependent variable "dimensao7.fss"
ggPlotTTest(sdat[which(sdat[["var"]] == "dimensao7.fss"),], "cenário", "dimensao7.fss"
, res$tt[["dimensao7.fss"]], c("jitter"), font.label.size=10)


### T-Test plot for the dependent variable "dimensao5.fss"
ggPlotTTest(sdat[which(sdat[["var"]] == "dimensao5.fss"),], "cenário", "dimensao5.fss"
, res$tt[["dimensao5.fss"]], c("jitter"), font.label.size=10)


### T-Test plot for the dependent variable "dimensao2.fss"
ggPlotTTest(sdat[which(sdat[["var"]] == "dimensao2.fss"),], "cenário", "dimensao2.fss"
, res$tt[["dimensao2.fss"]], c("jitter"), font.label.size=10)


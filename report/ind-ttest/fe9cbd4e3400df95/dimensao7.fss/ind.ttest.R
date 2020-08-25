# title: "Independent sample t-test for `dimensao7.fss` ~ `cenário`"
# author: Geiser C. Challco <geiser@usp.br>
# comment: This file is automatically generate by Shiny-Statistic app (https://statistic.geiser.tech/)
#          Author - Geiser C. Challco <geiser@usp.br>
#
#          Shiny-Statistic is distributed in the hope that it will be useful,
#          but WITHOUT ANY WARRANTY; without even the implied warranty of
#          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#          GNU General Public License for more details.
#
#          You should have received a copy of the GNU General Public License.
#          If not, see <https://www.gnu.org/licenses/>.
#
if (!'remotes' %in% rownames(installed.packages())) install.packages('remotes')
if (!"rshinystatistics" %in% rownames(installed.packages())) {
  remotes::install_github("geiser/rshinystatistics")
} else if (packageVersion("rshinystatistics") < "0.0.0.9028") {
  remotes::install_github("geiser/rshinystatistics")
}

wants <- c('ggplot2','ggpubr','rshinystatistics','car','stats','rstatix','utils','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(rshinystatistics)

## Initial Data

dat <- read.csv("/home/rstudio/rshinystatistics/report/ind-ttest/fe9cbd4e3400df95/dimensao7.fss/data.csv")
rownames(dat) <- dat[["nome"]]


## Check Assumptions

### Identifying outliers

identify_outliers(dplyr::group_by(dat, `cenário`), `dimensao7.fss`)

car::Boxplot(`dimensao7.fss` ~ `cenário`, data = dat, id = list(n = Inf))

### Removing outliers from the data

outliers <- c("")
rdat <- dat[!dat[["nome"]] %in% outliers,]


### Normality assumption

#### Applying transformation for skewness data when normality is not achieved



#### Removing data that affect normality (extreme values)

non.normal <- c("")
sdat <- rdat[!rdat[["nome"]] %in% non.normal,]
rownames(sdat) <- sdat[["nome"]]

#### Checking normality assumption in the groups

normality_test_per_group(sdat, "dimensao7.fss", "cenário")

car::qqPlot(`dimensao7.fss` ~ `cenário`, data = sdat, ylab = "sample")

### Homogeneity of variance assumption

homogeneity_test(sdat, "dimensao7.fss", "cenário")


## Computation t-test and effect size

res <- ind_ttest(sdat, "dimensao7.fss", "cenário", "two.sided", FALSE, FALSE, as.list=T)
(res$t.test)

## Descriptive Statistic and T-Test Plots

descriptive_statistics(sdat, "dimensao7.fss", "cenário", "common")

ggPlotTTest(sdat, "cenário", "dimensao7.fss", res$tt[["dimensao7.fss"]], c("jitter"), font.label.size=10)
# title: "Independent sample t-test for `{{ dv }}` ~ `{{ iv }}`"
# author: {{ author }} <{{ email }}>
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
} else if (packageVersion("rshinystatistics") < "{{ rshinystatistics.version }}") {
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

dat <- read.csv("{{ path }}/data.csv")
rownames(dat) <- dat[["{{ wid }}"]]


## Check Assumptions

### Identifying outliers

identify_outliers(dplyr::group_by(dat, `{{ iv }}`), `{{ dv }}`)

car::Boxplot(`{{ dv }}` ~ `{{ iv }}`, data = dat, id = list(n = Inf))

### Removing outliers from the data

outliers <- c({{ paste0(paste0('"',outlier.ids,'"'),collapse = ',') }})
rdat <- dat[!dat[["{{ wid }}"]] %in% outliers,]


### Normality assumption

#### Applying transformation for skewness data when normality is not achieved

{{ code.skewness }}

#### Removing data that affect normality (extreme values)

non.normal <- c({{ paste0(paste0('"',non.normal.ids,'"'),collapse = ',') }})
sdat <- rdat[!rdat[["{{ wid }}"]] %in% non.normal,]
rownames(sdat) <- sdat[["{{ wid }}"]]

#### Checking normality assumption in the groups

normality_test_per_group(sdat, "{{ dv }}", "{{ iv }}")

car::qqPlot(`{{ dv }}` ~ `{{ iv }}`, data = sdat, ylab = "sample")

### Homogeneity of variance assumption

homogeneity_test(sdat, "{{ dv }}", "{{ iv }}")


## Computation t-test and effect size

res <- ind_ttest(sdat, "{{ dv }}", "{{ iv }}", "{{ alternative }}", {{ var.equal }}, {{ hedges.correction }}, as.list=T)
(res$t.test)

## Descriptive Statistic and T-Test Plots

descriptive_statistics(sdat, "{{ dv }}", "{{ iv }}", "common")

ggPlotTTest(sdat, "{{ iv }}", "{{ dv }}", res$tt[["{{ dv }}"]], c({{ paste0(paste0('"',addParam,'"'),collapse=',') }}), font.label.size={{ font.label.size }})

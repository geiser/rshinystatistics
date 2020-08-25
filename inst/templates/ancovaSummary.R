# title: "Summary of ANCOVA for {{ paste0(paste0('`', dvs, '`'), collapse = ',') }}"
# author: {{ author }} <{{ email }}>
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

if (!'remotes' %in% rownames(installed.packages())) install.packages('remotes')
if (!"rshinystatistics" %in% rownames(installed.packages())) {
  remotes::install_github("geiser/rshinystatistics")
} else if (packageVersion("rshinystatistics") < "{{ rshinystatistics.version }}") {
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

data <- read.csv("{{ path }}/data.csv")
rownames(data) <- data[["{{ wid }}"]]

### Setting identificator, dependent and independent variables

wid <- "{{ wid }}"
covar <- "{{ covar }}"
between <- c({{ paste0(paste0('"', between, '"'), collapse = ',') }})
dvs <- c({{ paste0(paste0('"', dvs, '"'), collapse = ',') }})
dat <- set_datatable(data, dvs, "var")

## Check Assumptions

### Removing outliers from the data

outliers <- {{ code.outliers }}
rdat <- remove_from_datatable(dat, outliers, wid, "var")

### Normality assumption

#### Applying transformation for skewness data when normality is not achieved

{{ code.skewness }}

#### Removing data that affect normality (extreme values)

non.normal <- {{ code.non.normal }}
sdat <- remove_from_datatable(rdat, non.normal, wid, "var")

#### Checking normality assumption in the residual model

normality_test_by_res(sdat, dvs, between, c(), covar, dv.var='var')

### Linearity assumption

{{ linearity.code }}

### Homogeneity assumption

homogeneity_test(sdat, dvs, between, c(), covar, dv.var='var')

## Computation ANCOVA and Pairwise comparison

### ANCOVA test

aov <- ancova.test(sdat, dvs, between, covar, "{{ type }}", "{{ effect.size }}", "var")
(get.ancova.table(aov))

### Pairwise comparison showing only significant differences

pwc <- ancova.pwc(sdat, dvs, between, covar, "{{ p.adjust.method }}", "var")
(get.ancova.pwc.table(pwc, only.sig = T))

## Estimated Marginal Means and ANCOVA Plots

get.ancova.emmeans.with.ds(pwc, sdat, dvs, between, "common", "var")

{{ ancova.plots  }}

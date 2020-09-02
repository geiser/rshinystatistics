# title: "Summary of factorial ANOVA for {{ paste0(paste0('`', dvs, '`'), collapse = ',') }}"
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
#         If not, see <https://www.gnu.org/licenses/>.
#

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

### Setting identificator, dependent and independent variables

wid <- "{{ wid }}"
between <- c({{ paste0(paste0('"', between, '"'), collapse = ',') }})
dvs <- c({{ paste0(paste0('"', dvs, '"'), collapse = ',') }})
ldvs <- as.list(dvs); names(ldvs) <- dvs
dat <- lapply(ldvs, FUN = function(dv) {
  data <- read.csv(paste0("{{ path }}/data-",dv,".csv"))
  rownames(data) <- data[["{{ wid }}"]]
  return(data)
})

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

normality_test_by_res(sdat, dvs, between, dv.var='var')

#### Checking normality assumption in the groups

normality_test_per_group(sdat, dvs, between, dv.var = 'var')

### Homogeneity assumption

homogeneity_test(sdat, dvs, between, dv.var='var')

## Computation ANOVA and Pairwise comparison

### ANOVA test

aov <- get.anova.test(sdat, dvs, between, type = {{ type }}, effect.size = "{{ effect.size }}", dv.var = "var")
(get.anova.table(aov))

### Pairwise comparison showing only significant differences

pwc <- get.anova.pwc(sdat, dvs, between, p.adjust.method = "{{ p.adjust.method }}", dv.var = "var")
(get.anova.pwc.table(pwc, only.sig = T))


## Estimated Marginal Means and ANOVA Plots

get.anova.emmeans.with.ds(pwc, sdat, dvs, between, "common", "var")

{{ anova.plots  }}



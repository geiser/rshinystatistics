# title: "Summary of independent t-test for {{ paste0(paste0('`', dvs, '`'), collapse = ',') }} with `{{ iv }}`"
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

dvs <- c({{ paste0(paste0('"', dvs, '"'), collapse = ',') }})
iv <- "{{ iv }}"
wid <- "{{ wid }}"

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

#### Checking normality assumption in the groups

normality_test_per_group(sdat, dvs, iv, dv.var = 'var')

### Homogeneity of variance assumption

homogeneity_test(sdat, dvs, iv, dv.var = 'var')

## Computation t-test and effect size

res <- ind_ttest(sdat, dvs, iv, "{{ alternative }}", {{ var.equal }}, {{ hedges.correction }}, dv.var = 'var', as.list = T)
(res$t.test)

## Descriptive Statistic and T-Test Plots

descriptive_statistics(sdat, dvs, iv, "common", "var")


{{ ttest.plots  }}


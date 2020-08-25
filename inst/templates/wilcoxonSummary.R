# title: "Wilcoxon test for {{ paste0(paste0('`', dvs, '`'), collapse = ',') }} with `{{ iv }}`"
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

## Initial Data

data <- read.csv("data.csv")
rownames(data) <- data[["{{ wid }}"]]

### Setting identificator, dependent and independent variables

dvs <- c({{ paste0(paste0('"', dvs, '"'), collapse = ',') }})
iv <- "{{ iv }}"
wid <- "{{ wid }}"
dat <- set_datatable(data, dvs, "var")

## Removing outliers from the data

outliers <- {{ code.outliers }}
sdat <- remove_from_datatable(dat, outliers, wid, "var")


## Computation Wilcoxon Test and effect size

res <- wilcoxon_test(sdat, dvs, iv, "{{ alternative }}", dv.var='var', as.list=T)
(res$wilcoxon.test)

## Descriptive Statistic and Wilcoxon Plots

descriptive_statistics(sdat, dvs, iv, "common", "var")

{{ wtest.plots  }}

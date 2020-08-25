# title: "ANOVA for {{ paste0('`',dv,'` ~ ',paste0(paste0('`',between,'`'), collapse='*')) }}"
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

identify_outliers(dplyr::group_by(dat, {{ paste0(paste0('`',between,'`'), collapse = ',') }}), `{{ dv }}`)

car::Boxplot(`{{ dv }}` ~ {{ paste0(paste0('`',between,'`'), collapse = '*') }}, data = dat, id = list(n = Inf))

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

#### Checking normality assumption in the residual model

normality_test_by_res(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}))

res <- stats::residuals(stats::lm(`{{ dv }}` ~ {{ paste0(paste0('`',between,'`'), collapse='*') }}, data = sdat))
car::qqPlot(res, ylab = "sample")

#### Checking normality assumption in the groups

normality_test_per_group(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}))

{{ group.qqplot }}


### Homogeneity assumption

homogeneity_test(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}))

## Computation ANOVA and Pairwise comparison

### ANOVA test

aov <- get.anova.test(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}), type={{ type }}, effect.size="{{ effect.size }}")
(get.anova.table(aov))


### Pairwise comparison showing all comparisons

pwc <- get.anova.pwc(sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse = ',') }}), p.adjust.method="{{ p.adjust.method }}")
(get.anova.pwc.table(pwc))

## Estimated Marginal Means and ANOVA Plots

get.anova.emmeans.with.ds(pwc, sdat, "{{ dv }}", c({{ paste0(paste0('"',between,'"'), collapse=',') }}), "common")

{{ anova.plots  }}

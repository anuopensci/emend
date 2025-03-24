
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emend

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Save time and effort by using emend to clean and standardise your data.
The emend package is your artificial intelligence (AI) assistant that
contain a collection of functions to help you with your data cleaning
tasks.

WARNING: validate that the method works for your own data context.

The default setting has been set so that the results are reproducible
and less prone to “creativity”, as desired for data processing. The
reproducibility is achieved within the same system (i.e. computer) only
and not necessarily across different systems.

## Installation

### Large Language Model

You will also need to set up a large language model (LLM), either by
downloading a local LLM via Ollama or API access. To do this see
[`vignette("setup-LLM", package = "emend")`](https://emitanaka.org/emend/articles/setup-LLM.html).

### Package

You can install the development version of emend like below:

``` r
# install.packages("pak")
pak::pak("emitanaka/emend")
```

## Examples

``` r
library(emend)
```

### Specifying the LLM

Before using any functions in `emend`, you need to set up a “Chat”
object which is from the package `ellmer`, then use this “Chat” object
as an input argument for functions in `emend`.

**If you have set up a local LLM using Ollama:**

You could run the below code to set up your `Chat` object. Our function
is tested with the LLM `llama3.1:8b` and with a seed of 0, so these will
be the preferred parameters.

``` r
chat <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none") 
```

**If you want to use LLM via provider APIs:** Follow the instructions on
the `ellmer` [website](https://ellmer.tidyverse.org/) to set up the API
keys and create a desired `Chat` object.

### Categorise text

Some categorical variables can have simple typos or alternative
representations. For example below we have “UK” written also as “United
Kingdom”.

``` r
messy$country
#>  [1] "UK"             "US"             "Canada"         "UK"            
#>  [5] "US"             "Canada"         "United Kingdom" "USA"           
#>  [9] "New Zealand"    "NZ"             "Australia"      "New Zealand"   
#> [13] "UK"             "United Kingdom" "UK"             "US"            
#> [17] "United Kingdom" "Australia"      "US"             "Australia"
```

While you can manually fix this, again, this can be tedious. We can map
this automatically using `emend_fct_match()`.

``` r
emend_fct_match(messy$country, levels = c("UK", "USA", "Canada", "Australia", "NZ"), chat = chat)
#>  [1] UK        USA       Canada    UK        USA       Canada    UK       
#>  [8] USA       NZ        NZ        Australia NZ        UK        UK       
#> [15] UK        USA       UK        Australia USA       Australia
#> Levels: UK USA Canada Australia NZ
```

The function actually works to match a continent as well! Let’s use
`emend_lvl_match()` to more easily see the conversion on the levels
alone.

``` r
emend_lvl_match(messy$country, levels = c("Asia", "Europe", "North America", "Oceania", "South America"), chat = chat)
#>              UK              US          Canada  United Kingdom             USA 
#>        "Europe" "North America" "North America"        "Europe" "North America" 
#>     New Zealand              NZ       Australia 
#>       "Oceania"       "Oceania"       "Oceania"
#> 
#> ── Converted by emend: ─────────────────────────────────────────────────────────
#>         original     converted
#> 1             UK        Europe
#> 2 United Kingdom        Europe
#> 3             US North America
#> 4         Canada North America
#> 5            USA North America
#> 6    New Zealand       Oceania
#> 7             NZ       Oceania
#> 8      Australia       Oceania
```

The above process required specification of all the levels but sometimes
you may not know ahead all of the levels. The `emend_get_levels()`
function will attempt to clean up the levels.

``` r
levels <- emend_lvl_unique(messy$country, chat = chat)
print(levels)
#> [1] "United Kingdom" "United States"  "Canada"         "New Zealand"   
#> [5] "Australia"
```

Then you can use the cleaned levels to map the messy data to the correct
levels.

``` r
emend_fct_match(messy$country, levels = levels, chat = chat)
#>  [1] United Kingdom United States  Canada         United Kingdom United States 
#>  [6] Canada         United Kingdom United States  New Zealand    New Zealand   
#> [11] Australia      New Zealand    United Kingdom United Kingdom United Kingdom
#> [16] United States  United Kingdom Australia      United States  Australia     
#> Levels: United Kingdom United States Canada New Zealand Australia
```

### Correcting order of levels for ordinal variables

The levels of categorical variables by default are ordered
alphabetically. This can be problematic when the levels have a natural
order.

``` r
factor(likerts$likert1)
#>  [1] Strongly Disagree Neutral           Strongly Agree    Strongly Disagree
#>  [5] Disagree          Somewhat Agree    Strongly Agree    Somewhat Disagree
#>  [9] Agree             Disagree          Somewhat Disagree Somewhat Disagree
#> [13] Strongly Disagree Somewhat Agree    Somewhat Agree    Disagree         
#> [17] Agree             Agree             Disagree          Strongly Agree   
#> [21] Strongly Disagree Strongly Agree    Somewhat Agree    Somewhat Agree   
#> [25] Strongly Disagree Strongly Disagree Agree             Somewhat Agree   
#> [29] Somewhat Agree    Disagree          Disagree          Agree            
#> [33] Strongly Disagree Neutral           Strongly Agree    Strongly Disagree
#> [37] Neutral           Somewhat Disagree Agree             Disagree         
#> 7 Levels: Agree Disagree Neutral Somewhat Agree ... Strongly Disagree
```

A correct order may need to be manually specified like below, but it can
be a tedious task.

``` r
factor(likerts$likert1, 
       levels = c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree")) 
#>  [1] Strongly Disagree Neutral           Strongly Agree    Strongly Disagree
#>  [5] Disagree          Somewhat Agree    Strongly Agree    Somewhat Disagree
#>  [9] Agree             Disagree          Somewhat Disagree Somewhat Disagree
#> [13] Strongly Disagree Somewhat Agree    Somewhat Agree    Disagree         
#> [17] Agree             Agree             Disagree          Strongly Agree   
#> [21] Strongly Disagree Strongly Agree    Somewhat Agree    Somewhat Agree   
#> [25] Strongly Disagree Strongly Disagree Agree             Somewhat Agree   
#> [29] Somewhat Agree    Disagree          Disagree          Agree            
#> [33] Strongly Disagree Neutral           Strongly Agree    Strongly Disagree
#> [37] Neutral           Somewhat Disagree Agree             Disagree         
#> 7 Levels: Strongly Disagree Disagree Somewhat Disagree ... Strongly Agree
```

The `emend_fct_reorder()` function will try to reorder the levels of the
factor in a meaningful way *automatically* using a large language model.

``` r
emend_fct_reorder(likerts$likert1, chat = chat) |> levels()
#> [1] "Strongly Disagree" "Disagree"          "Somewhat Disagree"
#> [4] "Neutral"           "Somewhat Agree"    "Agree"            
#> [7] "Strongly Agree"
```

### Translate

The `emend_translate()` function can be used to translate text to
another language (default English). The text can be a mix of different
languages.

``` r
text <- c("猿も木から落ちる", "你好", "bon appetit")
emend_translate(text, chat = chat)
#> [1] "Even monkeys fall from trees." "Hello."                       
#> [3] "Enjoy your meal."
```

You can also try to identify the language in the text.

``` r
emend_what_language(text, chat = chat)
#> [1] "Japanese"         "Mandarin Chinese" "French"
```

## Dates

When combining data from different sources, inconsistencies in date
formats can occur frequently. Reformatting dates to a single format
using traditional programming requires listing all possible date formats
and can be time-consuming. The `emend_clean_date()` function uses an LLM
to standardise the dates to the international standard “YYYY-MM-DD”.

``` r
x <- c("16/02/1997", "20 November 2024", "24 Mar 2022", "2000-01-01", "Jason", 
       "Dec 25, 2030", "11/05/2024", "March 10, 1999")
emend_clean_date(x, chat = chat)
#> [1] "1997-02-16" "2024-11-20" "2022-03-24" "2000-01-01" NA          
#> [6] "2020-12-25" "2024-05-11" "1999-03-10"
```

## Addresses

When scraping data from websites or APIs, especially property-related
information, addresses can present challenges. The
`emend_clean_address()` function uses an LLM to standardise addresses
into a consistent format and returns an empty value for items that are
not addresses.

``` r
x <- c("154 university avenue, acton act 2601",
       "76/2 Cape Street, Dickson ACT 2602",
       "Shop 4/96 Bunda St, Canberra ACT 2601",
       "11 E Row, Canberra ACT 2601",
       "173/46 Macquarie St, Barton ACT 2600",
       "Unit 189/260 City walk, Canberra ACT 2601",
       "the kebab place",
       "i don't know the address")
emend_clean_address(x, chat = chat)
#> [1] "154 University Ave, Acton ACT 2601"   
#> [2] "76/2 Cape St, Dickson ACT 2602"       
#> [3] "Shop 4/96 Bunda St, Canberra ACT 2601"
#> [4] "11 E Row, Canberra ACT 2601"          
#> [5] "173/46 Macquarie St, Barton ACT 2600" 
#> [6] "189/260 City Walk, Canberra ACT 2601" 
#> [7] "INVALID ADDRESS"                      
#> [8] "INVALID ADDRESS"
```

## Related packages

- `air`
- `askgpt`
- `chatgpt`
- `ellmer`
- `gptchatteR`
- `gptstudio`
- `gpttools`
- `TheOpenAIR`
- `tidychatmodels`

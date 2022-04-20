
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rusfl

<!-- badges: start -->
<!-- badges: end -->

The goal of rusfl is to provide functions to load and parse USFL data.

## Installation

You can install the development version of rusfl from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mrcaseb/rusfl")
```

## NOTE

This USFL api requires an api key that is not provided with this
package.

## Examples

### Load Basic Team Info

This will be loaded from Anthony Reinhard’s Github.

``` r
teams <- rusfl::usfl_load_teams()
dplyr::glimpse(teams)
#> Rows: 8
#> Columns: 15
#> $ team              <chr> "MI", "NJ", "PHI", "PIT", "BIR", "HOU", "NO", "TB"
#> $ team_city         <chr> "Michigan", "New Jersey", "Philadelphia", "Pittsburg…
#> $ team_nick         <chr> "Panthers", "Generals", "Stars", "Maulers", "Stallio…
#> $ team_name         <chr> "Michigan Panthers", "New Jersey Generals", "Philade…
#> $ team_twitter      <chr> "@USFLPanthers", "@USFLGenerals", "@USFLStars", "@US…
#> $ team_coach        <chr> "Jeff Fisher", "Mike Riley", "Bart Andrus", "Kirby W…
#> $ team_div          <chr> "North", "North", "North", "North", "South", "South"…
#> $ official_logo_url <chr> "https://www.usfl2.com/images/Logos_USFL_Panthers.pn…
#> $ team_color_1      <chr> "#702E3E", "#C10230", "#C10230", "#31006F", "#A32035…
#> $ team_color_2      <chr> "#B2A77E", "#F0D283", "#FFB71B", "#FF4D00", "#C6B784…
#> $ team_color_3      <chr> "#4197CB", "#011E41", "#FF6B00", "#FFFFFF", "#FFFFFF…
#> $ full_logo         <chr> "https://raw.githubusercontent.com/ajreinhard/USFL/m…
#> $ basic_logo        <chr> "https://raw.githubusercontent.com/ajreinhard/USFL/m…
#> $ full_wordmark     <chr> "https://raw.githubusercontent.com/ajreinhard/USFL/m…
#> $ name_wordmark     <chr> "https://raw.githubusercontent.com/ajreinhard/USFL/m…
```

### Load Game Info for multiple Weeks

The best way is to use the wrapper `rusfl::usfl_load_scores()` and
choose the weeks.

``` r
games <- rusfl::usfl_load_scores(1:2)
dplyr::glimpse(games)
#> Rows: 8
#> Columns: 60
#> $ week_day                   <chr> "w1sat", "w1sun", "w1sun", "w1mon", "w2fri"…
#> $ sectionDate                <chr> "2022-04-16T04:00:00Z", "2022-04-17T04:00:0…
#> $ segmentId                  <chr> "w1", "w1", "w1", "w1", "w2", "w2", "w2", "…
#> $ selectionId                <chr> "w1", "w1", "w1", "w1", "w2", "w2", "w2", "…
#> $ title                      <chr> "WEEK 1", NA, NA, NA, "WEEK 2", NA, NA, NA
#> $ subtitle                   <chr> "SATURDAY, APR 16", "SUNDAY, APR 17", "SUND…
#> $ menuTitle                  <chr> "SATURDAY, APR 16", "SUNDAY, APR 17", "SUND…
#> $ menuTitlePrefix            <chr> "WEEK 1 - ", "WEEK 1 - ", "WEEK 1 - ", "WEE…
#> $ template                   <chr> "scores-team", "scores-team", "scores-team"…
#> $ id                         <chr> "usfl1", "usfl2", "usfl3", "usfl4", "usfl5"…
#> $ uri                        <chr> "https://api.foxsports.com/bifrost/v1/usfl/…
#> $ contentUri                 <chr> "football/usfl/events/1", "football/usfl/ev…
#> $ contentType                <chr> "event", "event", "event", "event", "event"…
#> $ eventStatus                <int> 3, 3, 3, 3, 2, 2, 2, 2
#> $ eventTime                  <chr> "2022-04-16T23:30:00Z", "2022-04-17T16:00:0…
#> $ sortKey                    <chr> "113202204162330269000001", "13320220417160…
#> $ altSortKey                 <chr> "113202204162330269000001", "13320220417160…
#> $ statusLine                 <chr> "FINAL", "FINAL", "FINAL", "FINAL", NA, NA,…
#> $ isSuperSix                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ isTba                      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ league                     <chr> "USFL", "USFL", "USFL", "USFL", "USFL", "US…
#> $ importance                 <int> 2, 0, 0, 2, 0, 0, 0, 0
#> $ venueName                  <chr> "Protective Stadium", "Protective Stadium",…
#> $ venueLocation              <chr> "Birmingham, AL", "Birmingham, AL", "Birmin…
#> $ away_team_name             <chr> "NJ", "HOU", "PHI", "TB", "MICH", "PIT", "B…
#> $ away_team_longName         <chr> "GENERALS", "GAMBLERS", "STARS", "BANDITS",…
#> $ away_team_alternateName    <chr> "New Jersey Generals", "Houston Gamblers", …
#> $ away_team_images           <df[,2]> <data.frame[8 x 2]>
#> $ away_team_logoUrl          <chr> "https://b.fssta.com/uploads/application/us…
#> $ away_team_imageType        <chr> "image-logo", "image-logo", "image-logo", "…
#> $ away_team_imageAltText     <chr> "New Jersey Generals", "Houston Gamblers", …
#> $ away_team_record           <chr> "0-1", "1-0", "0-1", "1-0", "0-1", "0-1"…
#> $ away_team_score            <int> 24, 17, 17, 17, NA, NA, NA, NA
#> $ away_team_isLoser          <lgl> TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FAL…
#> $ away_team_hasPossession    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ away_team_alternateLogoUrl <chr> "https://b.fssta.com/uploads/application/us…
#> $ away_team_primaryColor     <chr> "0, 193, 2, 48", "0, 0, 0, 0", "0, 193, 2, …
#> $ away_team_alternateColor   <chr> "0, 204, 159, 83", "0, 234, 0, 41", "0, 255…
#> $ home_team_name             <chr> "BHAM", "MICH", "NO", "PIT", "NJ", "PHI", "…
#> $ home_team_longName         <chr> "STALLIONS", "PANTHERS", "BREAKERS", "MAULE…
#> $ home_team_alternateName    <chr> "Birmingham Stallions", "Michigan Panthers"…
#> $ home_team_images           <df[,2]> <data.frame[8 x 2]>
#> $ home_team_logoUrl          <chr> "https://b.fssta.com/uploads/application/us…
#> $ home_team_imageType        <chr> "image-logo", "image-logo", "image-logo", "…
#> $ home_team_imageAltText     <chr> "Birmingham Stallions", "Michigan Panthers"…
#> $ home_team_record           <chr> "1-0", "0-1", "1-0", "0-1", "0-1", "0-1", "…
#> $ home_team_score            <int> 28, 12, 23, 3, NA, NA, NA, NA
#> $ home_team_isLoser          <lgl> FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FAL…
#> $ home_team_hasPossession    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ home_team_alternateLogoUrl <chr> "https://b.fssta.com/uploads/application/us…
#> $ home_team_primaryColor     <chr> "0, 163, 32, 53", "0, 112, 46, 62", "0, 0, …
#> $ home_team_alternateColor   <chr> "0, 198, 183, 132", "0, 255, 77, 0", "0, 65…
#> $ subtitlePrefix             <chr> NA, "WEEK 1 - ", "WEEK 1 - ", "WEEK 1 - ", …
#> $ url                        <chr> "/week-1-new-jersey-generals-vs-birmingham-…
#> $ liveStartTime              <chr> NA, NA, NA, NA, "2022-04-22T23:30:00Z", "20…
#> $ tvStation                  <chr> NA, NA, NA, NA, "USA", "FOX", "FS1", "NBC"
#> $ broadcasts                 <list> <NULL>, <NULL>, <NULL>, <NULL>, [<data.fram…
#> $ ticketLink                 <chr> NA, NA, NA, NA, "https://www.ticketmaster.c…
#> $ oddsLine                   <chr> NA, NA, NA, NA, "NJ -1.5", "PHI -6.5", "BHA…
#> $ overUnderLine              <chr> NA, NA, NA, NA, "TOTAL 41.5", "TOTAL 36", "…
```

### Load Rosters

The best way is to use the wrapper `rusfl::usfl_load_rosters()` and
choose the teams. The default loads all teams.

``` r
roster <- rusfl::usfl_load_rosters()
roster
#> # A tibble: 371 × 9
#>    team  player_name    pos     age ht      wt    college player_jersey headshot
#>    <chr> <chr>          <chr> <int> <chr>   <chr> <chr>           <int> <chr>   
#>  1 BHAM  Alex McGough   QB       26 "6'3\"" 214 … Florid…             2 https:/…
#>  2 BHAM  Bobby Holly    FB       24 "5'11\… 240 … Louisi…            44 https:/…
#>  3 BHAM  CJ Marable     RB       25 "5'11\… 194 … Coasta…            11 https:/…
#>  4 BHAM  Cameron Hunt   G        27 "6'4\"" 305 … Oregon             78 https:/…
#>  5 BHAM  Cary Angeline  TE       24 "6'7\"" 250 … North …            86 https:/…
#>  6 BHAM  Darius Harper  T        24 "6'6\"" 310 … Cincin…            73 https:/…
#>  7 BHAM  J'Mar Smith    QB       25 "6'1\"" 218 … Louisi…             6 https:/…
#>  8 BHAM  Jordan Chunn   RB       27 "6'0\"" 230 … Troy               38 https:/…
#>  9 BHAM  Jordan McCray  C        29 "6'2\"" 280 … UCF                51 https:/…
#> 10 BHAM  Justice Powers T        26 "6'3\"" 295 … UAB                50 https:/…
#> # … with 361 more rows
```

### Load Raw Game Data

Boxscores and Play by Play will be extracted from raw game data. So we
have to load the raw game data first. The game ids are included in
`games`. In this example these are `r`cli::cli_text(“{games$id}”)\`.

``` r
raw <- rusfl::usfl_load_game_data("usfl3")
```

This will be a list of deeply nested lists. The structure is

``` r
str(raw, max.level = 2)
#> List of 8
#>  $ header            :List of 22
#>   ..$ template            : chr "event-header-team"
#>   ..$ id                  : chr "3"
#>   ..$ uri                 : chr "https://api.foxsports.com/bifrost/v1/usfl/usflcom/event/3/data"
#>   ..$ eventStatus         : int 3
#>   ..$ eventTime           : chr "2022-04-17T21:00:00Z"
#>   ..$ isTba               : logi FALSE
#>   ..$ statusLine          : chr "FINAL"
#>   ..$ venueName           : chr "Protective Stadium"
#>   ..$ venueLocation       : chr "Birmingham, AL"
#>   ..$ analyticsSport      : chr "usfl"
#>   ..$ analyticsDescription: chr "philadelphia-stars-vs-new-orleans-breakers-20220417"
#>   ..$ socialStartTime     : chr "2022-04-16T21:00:00Z"
#>   ..$ socialStopTime      : chr "2022-04-19T00:00:00Z"
#>   ..$ contentType         : chr "event"
#>   ..$ contentUri          : chr "football/usfl/events/3"
#>   ..$ sportLogoUrl        : chr "https://b.fssta.com/uploads/application/leagues/logos/USFL.vresize.200.200.medium.0.png"
#>   ..$ sportLogoAltText    : chr "United States Football League"
#>   ..$ shareText           : chr "FOX Sports: Philadelphia Stars at New Orleans Breakers"
#>   ..$ oddsStartTime       : chr "2022-04-20T18:31:55Z"
#>   ..$ importance          : int 0
#>   ..$ leftTeam            :List of 17
#>   ..$ rightTeam           :List of 17
#>  $ boxscore          :List of 1
#>   ..$ boxscoreSections:'data.frame': 3 obs. of  3 variables:
#>  $ pbp               :List of 1
#>   ..$ sections:'data.frame': 4 obs. of  2 variables:
#>  $ linescore         :List of 3
#>   ..$ template: chr "table-linescore"
#>   ..$ headers :'data.frame': 1 obs. of  2 variables:
#>   ..$ rows    :'data.frame': 2 obs. of  4 variables:
#>  $ alternateLinescore:List of 3
#>   ..$ template: chr "table-linescore"
#>   ..$ headers :'data.frame': 1 obs. of  2 variables:
#>   ..$ rows    :'data.frame': 2 obs. of  4 variables:
#>  $ keyPlays          :List of 2
#>   ..$ title       : chr "KEY PLAYS"
#>   ..$ keyPlaysList:'data.frame': 16 obs. of  15 variables:
#>  $ trackingData      :List of 1
#>   ..$ contentEntityUri: chr "team:football/usfl/teams/4,team:football/usfl/teams/7"
#>  $ metadata          :List of 1
#>   ..$ parameters:List of 3
```

Play by Play can be parsed with

``` r
pbp <- rusfl::usfl_parse_pbp(raw)
dplyr::glimpse(pbp)
#> Rows: 179
#> Columns: 32
#> $ game_id                    <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3"…
#> $ play_qtr                   <chr> "1ST QUARTER", "1ST QUARTER", "1ST QUARTER"…
#> $ drive_id                   <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1"…
#> $ drive_title                <chr> "MISSED FG", "MISSED FG", "MISSED FG", "MIS…
#> $ drive_subtitle             <chr> "10 plays · 47 yards · 5:05", "10 plays · 4…
#> $ drive_imageUrl             <chr> "https://b.fssta.com/uploads/application/us…
#> $ drive_imageType            <chr> "image-logo", "image-logo", "image-logo", "…
#> $ drive_imageAltText         <chr> "Philadelphia Stars", "Philadelphia Stars",…
#> $ drive_leftTeamScore        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0"…
#> $ drive_rightTeamScore       <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0"…
#> $ drive_leftTeamScoreChange  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ drive_rightTeamScoreChange <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ drive_leftTeamAbbr         <chr> "PHI", "PHI", "PHI", "PHI", "PHI", "PHI", "…
#> $ drive_rightTeamAbbr        <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "…
#> $ play_id                    <chr> "1", "2", "3", "4", "5", "6", "8", "9", "10…
#> $ play_imageUrl              <chr> "https://b.fssta.com/uploads/application/us…
#> $ play_imageType             <chr> "image-headshot", "image-headshot", "image-…
#> $ play_imageAltText          <chr> "Austin MacGinnis", "Davin Bellamy", "Darne…
#> $ play_title                 <chr> "KICKOFF", "1ST AND 10", "2ND AND 15", "3RD…
#> $ play_subtitle              <chr> "NO25", "PHI26", "PHI21", "PHI25", "PHI41",…
#> $ play_playDescription       <chr> "A.MacGinnis kicks 66 yards from NO 25 to t…
#> $ play_leftTeamScoreChange   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ play_rightTeamScoreChange  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
#> $ play_timeOfPlay            <chr> "15:00", "14:33", "14:17", "13:39", "12:57"…
#> $ play_alternateImageUrl     <chr> "https://b.fssta.com/uploads/application/us…
#> $ play_leftTeamScore         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ play_rightTeamScore        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ play_leftTeamAbbr          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ play_rightTeamAbbr         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ play_periodOfPlay          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ drive_entityLink           <df[,12]> <data.frame[26 x 12]>
#> $ drive_alternateImageUrl    <chr> "https://b.fssta.com/uploads/applicatio…
```

Boxscore data can be parsed on the player level or on the team level.

Team level will look like this

``` r
rusfl::usfl_parse_boxscores(raw, "teams")
#> # A tibble: 19 × 7
#>    game_id type       title              away_team away_stat home_stat home_team
#>    <chr>   <chr>      <chr>              <chr>     <chr>     <chr>     <chr>    
#>  1 3       POSSESSION Time Of Possession PHI       31:36     28:24     NO       
#>  2 3       POSSESSION Total Drives       PHI       12        11        NO       
#>  3 3       POSSESSION Total Plays        PHI       63        66        NO       
#>  4 3       POSSESSION Total Yards        PHI       246       321       NO       
#>  5 3       POSSESSION Yards Per Play     PHI       3.9       4.9       NO       
#>  6 3       POSSESSION Red Zone TDs       PHI       1         2         NO       
#>  7 3       POSSESSION Red Zone Attempts  PHI       3         3         NO       
#>  8 3       PASSING    Total Yards        PHI       173       150       NO       
#>  9 3       PASSING    Completions        PHI       25        17        NO       
#> 10 3       PASSING    Passing Attempts   PHI       36        27        NO       
#> 11 3       PASSING    Yards Per Attempt  PHI       4.8       5.6       NO       
#> 12 3       PASSING    Passing TDs        PHI       1         0         NO       
#> 13 3       RUSHING    Total Yards        PHI       73        171       NO       
#> 14 3       RUSHING    Attempts           PHI       21        39        NO       
#> 15 3       RUSHING    Yards Per Rush     PHI       3.5       4.4       NO       
#> 16 3       RUSHING    Rushing TDs        PHI       1         2         NO       
#> 17 3       TURNOVERS  Total              PHI       2         1         NO       
#> 18 3       TURNOVERS  Fumbles Lost       PHI       1         0         NO       
#> 19 3       TURNOVERS  Interceptions      PHI       1         1         NO
```

ans player level will look like this

``` r
player_stats <- rusfl::usfl_parse_boxscores(raw)
dplyr::glimpse(player_stats)
#> Rows: 55
#> Columns: 53
#> $ game_id         <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3", "3", "3",…
#> $ team            <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", …
#> $ player_name     <chr> "Alexander", "Bellamy", "Bibbs", "Cannella", "Chavis",…
#> $ passing_com     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ passing_pct     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ passing_yds     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ passing_avg     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ passing_td      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ passing_int     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ passing_qbr     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ rushing_att     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 18, NA, NA, NA, NA, 15…
#> $ rushing_yds     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 89, NA, NA, NA, NA, 56…
#> $ rushing_avg     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 4.9, NA, NA, NA, NA, 3…
#> $ rushing_td      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, 1, …
#> $ rushing_lng     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 18, NA, NA, NA, NA, 12…
#> $ receiving_rec   <dbl> NA, NA, 1, 6, NA, NA, 4, NA, NA, NA, NA, 1, NA, 4, NA,…
#> $ receiving_yds   <dbl> NA, NA, 2, 58, NA, NA, 37, NA, NA, NA, NA, 10, NA, 38,…
#> $ receiving_avg   <dbl> NA, NA, 2.0, 9.7, NA, NA, 9.2, NA, NA, NA, NA, 10.0, N…
#> $ receiving_td    <dbl> NA, NA, 0, 0, NA, NA, 0, NA, NA, NA, NA, 0, NA, 0, NA,…
#> $ receiving_lng   <dbl> NA, NA, 2, 16, NA, NA, 12, NA, NA, NA, NA, 10, NA, 21,…
#> $ receiving_tgt   <dbl> NA, NA, 2, 7, NA, NA, 6, NA, NA, NA, NA, 4, NA, 4, NA,…
#> $ defensive_tck   <dbl> 5, 6, NA, NA, 3, 11, NA, 6, NA, 12, 2, NA, 1, NA, NA, …
#> $ defensive_sol   <dbl> 4, 4, NA, NA, 1, 7, NA, 4, NA, 8, 0, NA, 1, NA, NA, 3,…
#> $ defensive_sck   <dbl> 0.0, 3.0, NA, NA, 0.5, 0.0, NA, 0.0, NA, 0.0, 0.0, NA,…
#> $ defensive_tfl   <dbl> 0, 3, NA, NA, 0, 2, NA, 0, NA, 1, 0, NA, 0, NA, NA, 1,…
#> $ defensive_int   <dbl> 0, 0, NA, NA, 0, 1, NA, 0, NA, 0, 0, NA, 0, NA, NA, 0,…
#> $ defensive_pd    <dbl> 0, 0, NA, NA, 0, 1, NA, 1, NA, 0, 0, NA, 0, NA, NA, 0,…
#> $ defensive_td    <dbl> 0, 0, NA, NA, 0, 1, NA, 0, NA, 0, 0, NA, 0, NA, NA, 0,…
#> $ fumbles_fum     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1,…
#> $ fumbles_lst     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0,…
#> $ fumbles_ff      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0,…
#> $ fumbles_rec     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0,…
#> $ kick_return_ret <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4,…
#> $ kick_return_yds <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 87…
#> $ kick_return_avg <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 21…
#> $ kick_return_lng <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 26…
#> $ kick_return_td  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0,…
#> $ punt_return_ret <dbl> NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA,…
#> $ punt_return_yds <dbl> NA, NA, NA, NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, NA,…
#> $ punt_return_avg <dbl> NA, NA, NA, NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, NA,…
#> $ punt_return_lng <dbl> NA, NA, NA, NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, NA,…
#> $ punt_return_td  <dbl> NA, NA, NA, NA, NA, NA, 0, NA, NA, NA, NA, NA, NA, NA,…
#> $ kicking_fg      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ kicking_pct     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ kicking_lng     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ kicking_xp      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ kicking_pts     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ punting_no      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ punting_avg     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ punting_20      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ punting_tb      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ punting_lng     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ punting_blk     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```

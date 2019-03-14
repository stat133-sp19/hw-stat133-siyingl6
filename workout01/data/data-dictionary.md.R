##Data dictionary

* **team_name**
  + the name of the team
  + factor
  + example: Golden State Warriors

* **game_date**
  + the date of the game, represented by month/date/year
  + factor
  + example: 3/24/17

* **season**
  + the year of the season
  + integer
  + example: 2016

* **period**
  + an NBA game is divided in 4 periods of 12 mins each. For example, a value for period = 1 refers to the first period (the first 12 mins of the game)
  + integer
  + example:3

* **minutes_remaining**
  + amount of minutes remained to be played in a given period for that player
  + integer
  + example: 2

* **seconds_remaining**
  + amount of seconds remained to be played in a given period for that player
  + integer
  + example: 35

* **shot_made_flag**
  + indicates whether a shot was made or missed: y represents a shot was made, n represents a shot was missed
  + factor
  + example: n

* **action_type
  + indicates the basketball moves used by players, either to pass by defenders to gain access to the basket, or to get a clean pass to a teammate to score a
two pointer or three pointer
  + factor
  + example: Alley Oop Dunk Shot

* **shot_type**
  + indicates whether a shot is a 2-point field goal, or a 3-point field goal
  + factor
  + example: 2PT Field Goal

* **shot distance**
  + distance to the basket
  + integer
  + unit: feet
  + example: 0

* **opponent**
  + the name of the opposing team
  + factor
  + example: Sacramento Kings

* **x**
  + x court-coordinate where a shot occurred
  + integer
  + unit: inch
  + example: 0

* **y**
  + y court-coordinate where a shot occurred
  + integer
  + unit: inch
  + example: 1

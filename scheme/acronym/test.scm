(load "acronym.scm")

(use-modules (srfi srfi-64))

(test-begin "acronym")

; (test-skip "basic")
(test-equal "basic"
  (acronym "Portable Network Graphics")
  "PNG")

(test-equal "lowercase words"
  (acronym "Ruby on Rails")
  "ROR")

(test-equal "punctuation"
  (acronym "First In, First Out")
  "FIFO")

(test-equal "all caps word"
  (acronym "GNU Image Manipulation Program")
  "GIMP")

(test-equal "colon"
  (acronym "PHP: Hypertext Preprocessor")
  "PHP")

(test-equal "punctuation without whitespace"
  (acronym "Complementary metal-oxide semiconductor")
  "CMOS")

(test-equal "very long abbreviation"
  (acronym "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me")
  "ROTFLSHTMDCOALM")

(test-equal "consecutive delimiters"
  (acronym "Something - I made up from thin air")
  "SIMUFTA")

(test-equal "apostrophes"
  (acronym "Halley's Comet")
  "HC")

(test-equal "underscore emphasis"
  (acronym "The Road _Not_ Taken")
  "TRNT")

(test-end "acronym")

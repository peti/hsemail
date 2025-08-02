# Change Log for hsemail

## v2.2.2

* Introduce a flags development builds can enable to compile the examples.
* update example code

## v2.2.1

* Improve test cases for the 'day' parser.
* Fix day of month parser by also accepting the 31. of a month

## v2.2.0

* Drop the `parsec2read` function. `Read` is not supposed to be defined
  manually, really. It's supposed to be a dual to the derived `Show` instance.
* Drop the Rfc2821 module. This code is not generic enough to be useful,
  really. I use it in [Postmaster](http://hackage.haskell.org/package/postmaster),
  and there it will live henceforth.
* `caseString` no longer returns a string; it just returns `()`.
* Make use of `DayOfWeek` type from new `time` library.
* Drop the obsolete dependency on `mtl`.

## v2.1.0

* Re-write code to use the modern `time` library rather than `old-time`.
* rfc2821: drop the entire smtp FSM stuff
* hsemail.cabal: drop unnecessary build-depends
* Drop support for GHC versions prior to 7.10.x.

## v2

* Import Data.Monoid to fix build with GHC 7.8.x.
* Ensure that `body` consumes remaining input.
* Refrain from parsing body.

## v1.7.7

* rfc2822: allow 8 bit characters is message bodys

## v1.7.6

* move the project to github

## v1.7.5

* rfc2822: support obsolete local_part syntax
* rfc2822: support obsolete domain syntax
* rfc2822: fixed typo in the parser for domain literals
* rfc2822: support obsolete quoted-pair syntax
* Greatly extend the test suite.

## v1.7.4

* rfc2822: fix `return_path` parser
* rfc2822: improve documentation (especially `subject`, `comments`)

## v1.7.3

* rfc2822: fix infinite recursion between `day` and `obs_day`

## v1.7.2

* `word` parser failed for quoted string prefixed by ws

## v1.7.1

* Updated Gero's e-mail address.

## v1.7

* Fixed plenty of GHC and HLint warnings.

## v1.6

* rfc2822: derive `Show` for new `GenericMessage` type

## v1.5

* `Message` is now usable with `ByteString` or other types as body.

## 1.4

* Initial version.

port module HtmlTests exposing (..)

import Tests
import Test.Runner.Html exposing (run)



main : Test.Runner.Html.TestProgram
main =
    run Tests.all


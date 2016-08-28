module WordWrapTests

open WordWrap
open NUnit.Framework
open FsUnit

[<Test>]
let ``Wrapping a single word that's shorter than the line length returns that word``() =
    wrap "foo" 5 |> should equal "foo"

[<Test>]
let ``Wrapping a single word that's longer than the line length still returns that word``() =
    wrap "antidisestablishmentarianism" 5 |> should equal "antidisestablishmentarianism"

[<Test>]
let ``Wrapping two words that, combined, are shorter than the line length returns one line``() =
    wrap "foo bar" 8 |> should equal "foo bar"

[<Test>]
let ``Wrapping two words that exceed the line length returns two lines``() =
    wrap "foo bar" 4 |> should equal "foo\nbar"

[<Test>]
let ``Wrapping two words, with the line end falling within the second, replaces the separating space with a newline``() =
    wrap "foo bar" 5 |> should equal "foo\nbar"

let BareFrom s = Bare (s :BareWord)
let WithSpaceFrom w = WithSep {Word = w; Sep = " ";}

[<Test>]
let ``Words on a single word returns it as a bareword``() =
    words "foo" |> should equal [BareFrom "foo"]

[<Test>]
let ``Words on a space and a word returns it as a word-with-sep``() =
    words " foo" |> should equal [WithSpaceFrom "foo"]

[<Test>]
let ``Words on two tokens works``() =
    words "foo bar" |> should equal [(BareFrom "foo"); (WithSpaceFrom "bar")]

[<Test>]
let ``Words on a more complicated case also works``() =
    words "foo bar baz-baz quux" |> should equal [(BareFrom "foo");
                                                  (WithSpaceFrom "bar");
                                                  (WithSpaceFrom "baz-baz");
                                                  (WithSpaceFrom "quux")
                                                 ]

[<Test>]
let ``Length of a bareword is what you'd expect``() =
    length (BareFrom "foo") |> should equal 3

[<Test>]
let ``Length of a word-with-space is the length of the word plus the space``() =
    length (WithSpaceFrom "foo") |> should equal 4

[<Test>]
let ``Length of a word-with-other-sep takes both into account``() =
    length (WithSep {Word = "foo"; Sep = "--"}) |> should equal 5
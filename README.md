# golem

[WIP] Yet another **G**ame **o**f **L**ife **em**ulator.

🚨 Not done but has parts worth sharing! 🚨

# why

This is an excuse for the author to experiment with reactive UI patterns and
types first, and a mildly interesting game of life viewer / editor second.
Inspired by the work of [Arthur Xavier][xavierpdf] and
[Phil Freeman][freemanpdf].

Specifically, the *behavior* of an application can be thought of as a **space**
exposing certain capabilities or resources; when this space is a *comonad*
then one may automatically define a language of valid **actions** to *move
around* the space.

# building

This project uses [Cabal 3.x][cabal].

```console
> git clone https://github.com/gatlin/golem
> cd golem
> git submodule update --init
> cabal configure
> cabal run golem -- examples/pulsar.golem # or examples/glider.golem
```

There is an external dependency on [my bindings to termbox2][tb2hs] which is
included as a submodule for your convenience.

# issues / concerns / merge requests

Feel free to use the Github interface or contact me at
<gatlin+golem@niltag.net>.

[xavierpdf]: https://arthurxavierx.github.io/RealWorldAppComonadicUI.pdf
[freemanpdf]: http://functorial.com/the-future-is-comonadic/main.pdf
[tb2hs]: https://github.com/gatlin/termbox2-hs
[cabal]: https://cabal.readthedocs.io/en/stable/index.html

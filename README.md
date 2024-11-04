# Demo Chess UI in Elm

A sample chess frontend via Type Driven Development and some FRP (Relational and Reactive)

## Prerequisites

1. git
2. [Node](https://nodejs.org/en/)
3. [Elm](http://elm-lang.org/)

Optional:

4. [Yarn](https://yarnpkg.com/en/)

## Instructions

1. Clone or download source code:
```bash
git clone https://github.com/proc0/center-game.git
```

2. Navigate to root directory:
```bash
cd elm-chess
```

3. Build: 
```bash
yarn start
```

or 

```bash
elm make source/Chess.elm --output target/index.js
```

4. Run:

If you see this message:
```bash
Success! Compiled X module.
Successfully generated elm.js
$ elm reactor
elm-reactor 0.18.0
Listening on http://localhost:8000
```
Then open [http://localhost:8000](http://localhost:8000) in a browser

or 

Manually open index.html in a browser.



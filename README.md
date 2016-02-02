# purescript-pux-boilerplate

Skeleton [Pux](https://github.com/alexmingoia/purescript-pux/) application
with hot-reloading, using webpack, babel, and gulp.

See the
[Getting Started](http://alexmingoia.github.io/purescript-pux/docs/GettingStarted.html)
guide for help learning Pux.

## Installation

```sh
git clone git://github.com/alexmingoia/purescript-pux-boilerplate.git example
cd example
npm install
npm start
```

Visit `http://localhost:3000/webpack-dev-server` in your browser, edit `src/purs/App.purs` and
watch the magic!

## Available tasks

### watch

`npm run watch` will start a webpack development server, which hot-reloads your
application when sources changes.

### build

`npm run build` bundles and minifies your application.

# pux-starter-app

The [starter app](http://github.com/alexmingoia/pux-starter-app) is a skeleton
Pux project configured with webpack and supporting hot-reload for rapid
development.

For isomorphic rendering and routing a more complex configuration is available in the
[`isomorphic`](https://github.com/alexmingoia/pux-starter-app/tree/isomorphic) branch.

## Installation

Clone the repository and run `npm install` to get started:

```sh
git clone git://github.com/alexmingoia/pux-starter-app.git my-awesome-pux-app
cd my-awesome-pux-app
npm install
npm start
```

After compiling the app should be available at `http://localhost:8080`.

### Directory structure

- `src`: Application source code.
  - `src/App/Config.js`: Configuration values.
  - `src/App/Config.purs`: Configuration type.
  - `src/App/Events.purs`: Application event type and foldp function.
  - `src/App/Routes.purs`: Routes.
  - `src/App/State.purs`: Application state type and init function.
  - `src/App/View/Homepage.purs`: Home page.
  - `src/App/View/Layout.purs`: App layout.
  - `src/App/View/NotFound.purs`: 404 page.
  - `src/Main.purs`: PureScript entry point.
- `static`: Static files served with application.
- `support`: Support files for building.
  - `support/entry.js`: Webpack entry point. Handles hot reloading.
- `psc-package.json`: psc-package configuration.
- `package.json`: Node package configuration.
- `webpack.config.js`: Webpack configuration.

### NPM scripts

#### watch

`npm start` or `npm run watch` will start a development server, which
hot-reloads your application when sources changes.

#### serve

`NODE_ENV=production npm run serve` builds your application and starts a
production server.

#### build

`npm run build` builds application client and server bundles.

# Elmish HackerNews

This is a simple Fable application which enables you to browse Hackernews stories. It was implemented when completing an excercise in [Elmish Book](https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/elmish-hackernews).

The app uses [HackerNews API](https://github.com/HackerNews/API) and retrieves 4 different types of stories:

- new
- top
- best
- jobs

Stories are loaded lazily - you can retrieve 10 more stories to be displayed with a button at the bottom of the page.

## Building and running the app

First of all, start with installing the project's npm dependencies

```bash
npm install
```

Once this is finished, you can then build and compile the project:

```bash
npm run build
```

You can start developing the application in watch mode using the webpack development server:

```bash
npm start
```

After the first compilation is finished, navigate to http://localhost:8080 in your browser to see the application.

### VS Code

If you happen to use Visual Studio Code, simply hitting F5 will start the development watch mode for you and opens your default browser navigating to http://localhost:8080.
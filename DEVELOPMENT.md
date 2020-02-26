This repository is set up with both Dockerfiles and docker-compose so you can easily run
and test the app.

Using docker compose, you can run the app by running
`docker-compose up server`
and then the app will be available on port `3000`.

You can run the tests by running
`docker-compose run tests`
and the tests should run. If you'd like to see what is happening, you can open a VNC
viewer and connect to `localhost:5900` to see the app in Chrome (which will be being
driven by Selenium).

If you don't have docker installed, to run the tests you'll first need to have the
app running on port `3000`, and a selenium server ready to run Chrome on port `4444`.
When using docker, this is done for you.

Please note that the docker images can take over 20 minutes to build, as a lot of the
packages are compiled from source on linux.

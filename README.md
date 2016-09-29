# Demonstration of reactive chat application using ELM #

### Elm is a functional language that compiles to JavaScript. It competes with projects like React as a tool for creating websites and web apps. ###

### Application Stack ###
* Frontend (elm)
* Web server (NodeJS)
* Storage (Rethinkdb)

### Requirement ###
* NodeJS
* npm
* elm 0.17.1 (Major Update)
* elm-format (Optional)


### How to install ###
```
For ubuntu,

sudo apt-get update
sudo apt-get install nodejs
sudo apt-get install npm

npm install -g elm

# For MacOS,
brew install node

# Elm: Install from installer  
http://install.elm-lang.org/Elm-Platform-0.17.1.pkg
```

### elm-format ###
Download from https://github.com/avh4/elm-format

Move `elm-format` to `/usr/local/bin/elm-format`  

Setup elm-format for IntelliJ IDE  
GoTo **Preferences > External Tools > +**  

![elm-format.png](https://bitbucket.org/repo/LGgLdL/images/2512652711-elm-format.png)

### Others ###
Try elm online  
elm-lang.org/try  
Elm repl (Console)  
`elm repl`  
Install project required package  
`elm package install -y`  
Compile elm script  
`elm make path/to/source`
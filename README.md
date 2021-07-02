# Hangman
A sadistic command-line app disguised as a classic game.

![2021-07-02-134124_1021x749_scrot](https://user-images.githubusercontent.com/64954972/124272878-9f7b1b80-db3f-11eb-80bf-0c5be46a419c.png)

## Rules
Pretty much everything you would expect from a hangman game - you have to guess the secret word.
* every round, you can either guess a character or try and guess the whole word;
* if your word guess is correct (or reveal all hidden characters) you WIN!
* if you fail to do so before the whole stickman is drawn, you lose :(

## What is it
This exists only in order for me to practice basic IO and monad actions in Haskell, really. 

## TODOs
Features that will be implemented as soon as I figure out how:
* Multi-language support (maybe via user-prompt, it would be cool)
* Multiple games in one execution
* Better print for puzzle word and guesses

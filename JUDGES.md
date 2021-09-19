# Judges Guide to BEAMoji

hi! we understand that you may not be emoji-native, here's a document to
evaluate our submision.

## Description

The idea of the project is to provide a transpiler/formatter that takes erlang
modules and replaces the atoms in them with one or more emojis using different
translators.

Just replacing them won't work since many atoms have specific meanings like
`true`, `false`, `undefined`, `error`, `ok`, `not_found` or module and function
names when calling functions in other modules.

To make the emojified modules work we add attributes during the translation to
run a parse transform and tell it which translator was originally used so that
the parse transform can "demojify" the module before it's sent to the compiler.

In this way emojis are for humans, the compiler sees the module as if no
translation was done.

## Components

The project consists of 5 parts:

### The Emojifier (Formatter)

A code formatter that uses the specified translator to translate from atoms to
emojis and adds module attributes that add a parse transform and information
about the translator used.

### The Demojifier (Parse Transform)

A parse transform that uses the translator specified in the module attribute to
"demojify" the module to allow the compiler to generate code that works as if
no emojification was performed.

### The Translators

Different translation strategies that can be configured to be used by the Emojifier:

#### Emojilist

A bidirectional mapping from atom name to emoji, we call it the "nice" translator since
we tried to pick emojis that had a relationship with the atom they replaced.

#### Base Emoji

Like Base 64 but with emojis, at first it was pretty random but later we hand
picked locally sourced emojis for the 0-255 latin1 range, see if you can find
the relations :)

This is the "hard" translator since it takes more work to guess.

### Multiword

Splits atoms on the underscore character and tries to replace each word with an
emoji, it doesn't replace most atoms, but atoms composed of multiple words look
nice with it when translated.

### The rebar3 Plugin

A rebar3 plugin that adds the `rebar3 emojify` command and allows to emojify a
complete project.

### The Quiz

A [quiz](https://quiz.elixircards.co.uk/BEAMoji) using elixircards.co.uk that
shows a snippet of emojified code and asks different questions like what's the
name of the function, what's the algorithm, what's the output if we call that
function providing specific inputs and so on, the idea was to see how much
people can read from code if the identifiers are emojified with different
translators.

## Status

We completed our plans, the project does what's described above, it's has a 
[release on hex](https://hex.pm/packages/beamoji), has test and an 
[emoji-native landing page](https://spawnfest.org/beamoji/).

During the weekend we acted as a fuzzer for many projects, we documented our
achievements in the ACHIEVEMENTS.md file you can find at the root of the
project.

## How to Try

Follow the instructions on the [Readme's 🧪 Section](https://github.com/spawnfest/beamoji#-7)
or watch the video on the landing page for the same thing.

Try the quiz too!

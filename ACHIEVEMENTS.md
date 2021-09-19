# Broken Software

- [x] **Github**.
    * It crashes if you want to create a team with an emoji in its name.
- [x] `rebar3`. [erlang/rebar3#2618](https://github.com/erlang/rebar3/issues/2618)
    * It crashes if you want to create a provider with an emoji in their name.
    * It crashes if you include emojis in your calls to `rebar_api:warn/2`.
- [x] Sublime Text.
    * For composite emojis, like ⚛ and ⚛️, depending on what character you write after the simple one (⚛), it gets displayed as the complex one and your character disappears.
- [x] Franz.
    * The opposite as Sublime Text, it always displays the basic emoji.
- [x] Dialyzer. [erlang/otp#5210](https://github.com/erlang/otp/issues/5210)
    * Having an opaque type with an emoji breaks the pretty printing of errors.
- [x] rebar3_format. [AdRoll/rebar3_format#283](https://github.com/AdRoll/rebar3_format/issues/283)
    * Custom attributes with emojis will be formatted without quotes the first time and as an error the second time.
- [x] common_test. [erlang/otp#5212](https://github.com/erlang/otp/issues/5212)
    *  Mismatches with emojis crash the test when they're attempted to be printed out.
- [x] nvim.
    * Highlights the wrong colorcolumn when the line has emojis

# Limits Found

- [x] **Erlang/OTP**.
    * Module names with non-latin1 characters are not supported

# Projects Improved

- [x] `eflambe`
    * [spawnfest/eflambe#1](https://github.com/spawnfest/eflambe/pull/1)
- [x] `eArangoDB`
    * [spawnfest/eArangoDB](https://github.com/spawnfest/eArangoDB/pull/1) 

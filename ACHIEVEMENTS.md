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
- [x] rebar3_format.
    * Custom attributes with emojis will be formatted without quotes the first time and as an error the second time. [AdRoll/rebar3_format#283](https://github.com/AdRoll/rebar3_format/issues/283)
- [x] common_test. [erlang/otp#5212](https://github.com/erlang/otp/issues/5212)
    *  Mismatches with emojis crash the test when they're attempted to be printed out.

# Limits Found

- [x] **Erlang/OTP**.
    * Module names with non-latin1 characters are not supported

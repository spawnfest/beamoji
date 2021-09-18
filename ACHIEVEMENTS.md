# Broken Software

- [x] **Github**.
    * It crashes if you want to create a team with an emoji in its name.
- [x] `rebar3`. [erlang/rebar3#2618](https://github.com/erlang/rebar3/issues/2618)
    * It crashes if you want to create a provider with an emoji in their name.
    * It crashes if you include emojis in your calls to `rebar_api:warn/2`.

# Limits Found

- [x] **Erlang/OTP**.
    * Module names with non-latin1 characters are not supported

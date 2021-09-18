# 🪄

[![Build Status](https://github.com/spawnfest/beamoji/actions/workflows/erlang.yml/badge.svg)](https://github.com/spawnfest/beamoji)
[![Hex pm](http://img.shields.io/hexpm/v/beamoji.svg?style=flat)](https://hex.pm/packages/beamoji)

## ▶️

### ✍️
```erlang
{deps, [beamoji]}
{project_plugins, [beamoji]}.
```

### ⏭

```bash
$ rebar3 emojify
```

## 👷

```bash
$ rebar3 compile
```

## ✅

```bash
$ rebar3 test

```

## 🧪


```bash
rebar3 new lib name=beamojilib
cd ./beamojilib/
echo '\n{project_plugins, [{beamoji, {git, "git://github.com/spawnfest/beamoji.git", {branch, "Ⓜ️"}}}]}.' >> rebar.config
```

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
echo "" >> rebar.config
echo '{project_plugins, [beamoji]}.' >> rebar.config
wget https://raw.githubusercontent.com/spawnfest/beamoji/%E2%93%82%EF%B8%8F/test/beamoji_roundtrip_SUITE_data/emojifyme.erl -O src/emojifyme.erl
rebar3 emojify
```

## 🧩
https://quiz.elixircards.co.uk/BEAMoji

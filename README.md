# ๐ช

[![Build Status](https://github.com/spawnfest/beamoji/actions/workflows/erlang.yml/badge.svg)](https://github.com/spawnfest/beamoji)
[![Hex pm](http://img.shields.io/hexpm/v/beamoji.svg?style=flat)](https://hex.pm/packages/beamoji)

## โถ๏ธ

### โ๏ธ
```erlang
{deps, [beamoji]}
{project_plugins, [beamoji]}.
```

### โญ

```bash
$ rebar3 emojify
```

## ๐ท

```bash
$ rebar3 compile
```

## โ

```bash
$ rebar3 test

```

## ๐ค

### ๐

```erlang
'๐๏ธ'(P, [H | T]) ->
    case P(H) of
        'โ๏ธ' ->
            {'โ๏ธ', H};
        'โ' ->
            '๐๏ธ'(P, T)
    end;
'๐๏ธ'(P, []) ->
    'โ'.
```

### ๐ฃ

```erlang
'๐๐๐๐๐ฟ๏ธโค๏ธ'(P, [H | T]) ->
    case P(H) of
        '๐ฆ๐๐ฆ๐' ->
            {'โ๏ธ๐๐ฆ๐ฆ๐', H};
        '๐ฅ๐๐ฆ๐๐' ->
            '๐๐๐๐๐ฟ๏ธโค๏ธ'(P, T)
    end;
'๐๐๐๐๐ฟ๏ธโค๏ธ'(P, []) ->
    '๐ฅ๐๐ฆ๐๐'.
```
 

## ๐งช


```bash
rebar3 new lib name=beamojilib
cd ./beamojilib/
echo "" >> rebar.config
echo '{project_plugins, [beamoji]}.' >> rebar.config
wget https://raw.githubusercontent.com/spawnfest/beamoji/%E2%93%82%EF%B8%8F/test/beamoji_roundtrip_SUITE_data/emojifyme.erl -O src/emojifyme.erl
head -n 25 src/emojifyme.erl
rebar3 emojify
# or
# rebar3 emojify --translator beamoji_baseemoji_translator
head -n 25 src/emojifyme.erl
rebar3 shell
```

```erlang
emojifyme:sum([1, 2, 3, 4]).
```

## ๐งฉ
https://quiz.elixircards.co.uk/BEAMoji

---

## ๐ช๐ค๐โโ๏ธ๐งโโ๏ธโ๏ธ
### ๐ช: ๐ฆ๐ท ๐ง ๐น๏ธ โฉ ๐ฅ๏ธ ๐ค ๐
### ๐ค: ๐ช๐ธ ๐น๏ธ ๐ธ ๐ฅ๏ธ ๐ ๐
### ๐โโ๏ธ: ๐ป ๐ ๐ช ๐ฐ ๐๏ธ
### ๐งโโ๏ธ: ๐ง ๐พ โฉ ๐ป โ๏ธ ๐ฃ๏ธ ๐

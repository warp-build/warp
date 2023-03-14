defmodule TricorderDepsTest do
  use ExUnit.Case
  doctest Gitignore

  test "scans mix lock" do
    assert %{
             system_monitor: %{
               opts: [tag: "3.0.3"],
               protocol: :git,
               ref: "95db1792e2763ba2af1cf79484cac383ebec8192",
               url: "https://github.com/ieQu1/system_monitor.git"
             },
             mria: %{
               opts: [tag: "0.2.12"],
               protocol: :git,
               ref: "ab651457515ca0a6552b8f50911429e570af726f",
               url: "https://github.com/emqx/mria"
             },
             esasl: %{
               opts: [tag: "0.2.0"],
               protocol: :git,
               ref: "96d7ac9f6c156017dd35b30df2dd722ae469c7f0",
               url: "https://github.com/emqx/esasl.git"
             },
             jsx: %{
               opts: [tag: "v3.1.0"],
               protocol: :git,
               ref: "bb9b3e570a7efe331eed0900c3a5188043a850d7",
               url: "https://github.com/talentdeficit/jsx.git"
             },
             sext: %{host: "hexpm", package: "sext", protocol: :hexpm, version: "1.8.0"},
             ehttpc: %{
               opts: [tag: "0.3.0"],
               protocol: :git,
               ref: "2ba4b8a370bf0607c61647354b02a02411fe75c0",
               url: "https://github.com/emqx/ehttpc.git"
             },
             jq: %{
               opts: [tag: "v0.3.6"],
               protocol: :git,
               ref: "82a09c09e03b90e4cbefd74f6f3a413ee2865a38",
               url: "https://github.com/emqx/jq.git"
             },
             estatsd: %{
               opts: [tag: "0.1.0"],
               protocol: :git,
               ref: "5184d846b7ecb83509bd4d32695c60428c0198cd",
               url: "https://github.com/emqx/estatsd"
             },
             mysql: %{
               opts: [tag: "1.7.1"],
               protocol: :git,
               ref: "bdabac44cc8836a9e23897b7e1b77c7df7e04f70",
               url: "https://github.com/emqx/mysql-otp"
             },
             trails: %{
               host: "hexpm",
               package: "trails",
               protocol: :hexpm,
               version: "2.3.0"
             },
             lc: %{
               opts: [tag: "0.3.1"],
               protocol: :git,
               ref: "a6ceb0b67493ab4d0fcb9bd4b902a0e5b6413934",
               url: "https://github.com/emqx/lc.git"
             },
             grpc: %{
               opts: [tag: "0.6.7"],
               protocol: :git,
               ref: "b924b39b7f46f7b3604b2f36ab3c824d320d68a1",
               url: "https://github.com/emqx/grpc-erl.git"
             },
             ekka: %{
               opts: [tag: "0.13.5"],
               protocol: :git,
               ref: "2a2dec9b434855f0d5d507a19d5b8d05d5ff9ddb",
               url: "https://github.com/emqx/ekka.git"
             },
             observer_cli: %{
               host: "hexpm",
               package: "observer_cli",
               protocol: :hexpm,
               version: "1.7.1"
             },
             eredis_cluster: %{
               opts: [tag: "0.7.1"],
               protocol: :git,
               ref: "f60b132c3d08b44a1df8f1d98b91b20ecc47f831",
               url: "https://github.com/emqx/eredis_cluster"
             },
             bcrypt: %{
               opts: [tag: "0.6.0"],
               protocol: :git,
               ref: "dc2ba66acf2332c111362d01137746eefecc5e90",
               url: "https://github.com/emqx/erlang-bcrypt.git"
             },
             minirest: %{
               opts: [tag: "1.3.7"],
               protocol: :git,
               ref: "54f7919c06303a07088a2cee118b3233a777d07c",
               url: "https://github.com/emqx/minirest.git"
             },
             mnesia_rocksdb: %{
               opts: [tag: "0.1.10"],
               protocol: :git,
               ref: "e234ee615854ffc10a5242296bbba4b0c61511a8",
               url: "https://github.com/emqx/mnesia_rocksdb"
             },
             jiffy: %{
               opts: [tag: "1.0.5"],
               protocol: :git,
               ref: "baa1f4e750ae3c5c9e54f9c2e52280b7fc24a8d9",
               url: "https://github.com/emqx/jiffy.git"
             },
             cowboy_swagger: %{
               opts: [tag: "2.5.0"],
               protocol: :git,
               ref: "bc441df7988da0f5c5d11ae0861c394dc30995c5",
               url: "https://github.com/inaka/cowboy_swagger"
             },
             eetcd: %{
               opts: [tag: "v0.3.4"],
               protocol: :git,
               ref: "69d50aca98247953ee8a3ff58423a693f8318d90",
               url: "https://github.com/zhongwencool/eetcd"
             },
             snabbkaffe: %{
               opts: [tag: "1.0.0"],
               protocol: :git,
               ref: "758ab63171dc71e775fae4be06a3d8b1f80da1f1",
               url: "https://github.com/kafka4beam/snabbkaffe.git"
             },
             eredis: %{
               opts: [tag: "1.2.6"],
               protocol: :git,
               ref: "deb3c6c49485113f3b60faf10f9e90e330f89cc6",
               url: "https://github.com/emqx/eredis"
             },
             getopt: %{
               host: "hexpm",
               package: "getopt",
               protocol: :hexpm,
               version: "1.0.2"
             },
             replayq: %{
               host: "hexpm",
               package: "replayq",
               protocol: :hexpm,
               version: "0.3.4"
             },
             rulesql: %{
               opts: [tag: "0.1.4"],
               protocol: :git,
               ref: "fec11b1a3cbf98480d19c06d3aca10442e1e02a9",
               url: "https://github.com/emqx/rulesql.git"
             },
             rocksdb: %{
               opts: [tag: "1.7.2-emqx-6"],
               protocol: :git,
               ref: "84928ef578e3d0252f0b424f2d65dec75176e6d9",
               url: "https://github.com/emqx/erlang-rocksdb.git"
             },
             prometheus: %{
               opts: [tag: "v4.8.1"],
               protocol: :git,
               ref: "3245220e5b51c8005c84c2683fda1108b736badd",
               url: "https://github.com/deadtrickster/prometheus.erl"
             },
             emqx_http_lib: %{
               opts: [tag: "0.5.1"],
               protocol: :git,
               ref: "b76c2f69013de0bf80b44e5f51c57ef6c9bb1baf",
               url: "https://github.com/emqx/emqx_http_lib.git"
             },
             emqtt: %{
               opts: [tag: "1.6.0"],
               protocol: :git,
               ref: "51c809ff9081bde73d7996d7d40472789c4038b6",
               url: "https://github.com/emqx/emqtt.git"
             },
             jose: %{
               opts: [tag: "1.11.2"],
               protocol: :git,
               ref: "991649695aaccd92c8effb1c1e88e6159fe8e9a6",
               url: "https://github.com/potatosalad/erlang-jose.git"
             },
             typerefl: %{
               opts: [tag: "0.9.1"],
               protocol: :git,
               ref: "310b82ff02f96207c519b9556491433b6ea02d01",
               url: "https://github.com/ieQu1/typerefl.git"
             },
             quantile_estimator: %{
               host: "hexpm",
               package: "quantile_estimator",
               protocol: :hexpm,
               version: "0.2.1"
             },
             ranch: %{
               opts: [ref: "a692f44567034dacf5efcaa24a24183788594eb7"],
               protocol: :git,
               ref: "a692f44567034dacf5efcaa24a24183788594eb7",
               url: "https://github.com/ninenines/ranch.git"
             },
             gpb: %{host: "hexpm", package: "gpb", protocol: :hexpm, version: "4.19.5"},
             cowlib: %{
               opts: [ref: "c6553f8308a2ca5dcd69d845f0a7d098c40c3363"],
               protocol: :git,
               ref: "c6553f8308a2ca5dcd69d845f0a7d098c40c3363",
               url: "https://github.com/ninenines/cowlib.git"
             },
             mongodb: %{
               opts: [tag: "v3.0.13"],
               protocol: :git,
               ref: "e017c4e88163a36edd7cfa658c6dc89e327b4f41",
               url: "https://github.com/emqx/mongodb-erlang"
             },
             eldap2: %{
               opts: [tag: "v0.2.2"],
               protocol: :git,
               ref: "f595f67b094db3b9dc07941337706621e815431f",
               url: "https://github.com/emqx/eldap2"
             },
             bson: %{
               opts: [tag: "v0.2.2"],
               protocol: :git,
               ref: "14308ab927cfa69324742c3de720578094e0bb19",
               url: "https://github.com/comtihon/bson-erlang.git"
             },
             gun: %{
               opts: [tag: "1.3.7"],
               protocol: :git,
               ref: "4faea40b9a8ca1eac5288355f8202e0cea379d50",
               url: "https://github.com/emqx/gun.git"
             },
             epgsql: %{
               opts: [tag: "4.7-emqx.2"],
               protocol: :git,
               ref: "f2a97ea8c7690d8d263a6d4c44427da0712c4cd7",
               url: "https://github.com/emqx/epgsql.git"
             },
             recon: %{
               opts: [tag: "2.5.1"],
               protocol: :git,
               ref: "f7b6c08e6e9e2219db58bfb012c58c178822e01e",
               url: "https://github.com/ferd/recon.git"
             },
             supervisor3: %{
               host: "hexpm",
               package: "supervisor3",
               protocol: :hexpm,
               version: "1.1.9"
             },
             redbug: %{
               host: "hexpm",
               package: "redbug",
               protocol: :hexpm,
               version: "2.0.7"
             },
             gproc: %{
               opts: [tag: "0.8.0"],
               protocol: :git,
               ref: "ce7397809aca0d6eb3aac6db65953752e47fb511",
               url: "https://github.com/uwiger/gproc.git"
             },
             hocon: %{
               opts: [tag: "0.30.0"],
               protocol: :git,
               ref: "bf12c0836683284ba7139644cd856834904372ae",
               url: "https://github.com/emqx/hocon.git"
             },
             esockd: %{
               opts: [tag: "5.9.4"],
               protocol: :git,
               ref: "26fffb6caad8f9c9ad6fecc6ac9db232150af39f",
               url: "https://github.com/emqx/esockd.git"
             },
             pbkdf2: %{
               opts: [tag: "2.0.4"],
               protocol: :git,
               ref: "45d9981209ea07a83a58cf85aaf8236457da4342",
               url: "https://github.com/emqx/erlang-pbkdf2.git"
             },
             gen_rpc: %{
               opts: [tag: "2.8.1"],
               protocol: :git,
               ref: "b4e170f507d766b82bdd0d7929f6f7e0847670c3",
               url: "https://github.com/emqx/gen_rpc.git"
             },
             ecpool: %{
               opts: [tag: "0.5.2"],
               protocol: :git,
               ref: "8cd58f169239b96f8aa9a6cfdb5fa0275038ed1b",
               url: "https://github.com/emqx/ecpool.git"
             },
             poolboy: %{
               opts: [tag: "1.5.2"],
               protocol: :git,
               ref: "29be47db8c2be38b18c908e43a80ebb7b9b6116b",
               url: "https://github.com/emqx/poolboy.git"
             },
             cowboy: %{
               opts: [tag: "2.9.0"],
               protocol: :git,
               ref: "c8e2b9e069655ad166702a7deb935cabddc85888",
               url: "https://github.com/emqx/cowboy.git"
             }
           } = Tricorder.Deps.MixLock.load("test/fixtures/mix.lock")
  end

  test "scans rebar lock" do
    assert %{
             base16: %{package: "base16", protocol: :hexpm, version: "1.0.0"},
             lrw: %{package: "lrw", protocol: :hexpm, version: "2.0.1"},
             supervisor3: %{package: "supervisor3", protocol: :hexpm, version: "1.1.11"},
             enacl: %{package: "enacl", protocol: :hexpm, version: "1.2.1"},
             mops: %{
               protocol: :git,
               ref: "3955142d3720dc5b7a481e28124a2c5421dcd078",
               url: "https://gitlab.com/leapsight/mops.git"
             },
             metrics: %{package: "metrics", protocol: :hexpm, version: "1.0.1"},
             prometheus_httpd: %{
               package: "prometheus_httpd",
               protocol: :hexpm,
               version: "2.1.11"
             },
             ranch: %{package: "ranch", protocol: :hexpm, version: "1.8.0"},
             p1_utils: %{package: "p1_utils", protocol: :hexpm, version: "1.0.23"},
             email: %{
               protocol: :git,
               ref: "b62e070111e635e739628254eab8db2835631d28",
               url: "https://github.com/kivra/email.git"
             },
             setup: %{package: "setup", protocol: :hexpm, version: "2.1.0"},
             idna: %{package: "idna", protocol: :hexpm, version: "6.1.1"},
             bert: %{package: "bert", protocol: :hexpm, version: "0.1.0"},
             cowboy: %{package: "cowboy", protocol: :hexpm, version: "2.9.0"},
             mimerl: %{package: "mimerl", protocol: :hexpm, version: "1.2.0"},
             sidejob: %{package: "sidejob", protocol: :hexpm, version: "2.1.0"},
             bear: %{package: "bear", protocol: :hexpm, version: "1.0.0"},
             msgpack: %{package: "msgpack", protocol: :hexpm, version: "0.7.0"},
             brod: %{
               protocol: :git,
               ref: "5d9f189623070f4927cb88af563552695b305fce",
               url: "https://github.com/klarna/brod.git"
             },
             plum_db: %{
               protocol: :git,
               ref: "31ce10c4eb353d7a1fccb0ff4dac74e5d4d41021",
               url: "https://github.com/Leapsight/plum_db.git"
             },
             eleveldb: %{
               protocol: :git,
               ref: "f7339ad2a57537b3f7e185a38341664d3cc22cfb",
               url: "https://github.com/Leapsight/eleveldb.git"
             },
             tuplespace: %{
               protocol: :git,
               ref: "d5e540dc2e1d3a4b75dbf3841de99d4547e118ff",
               url: "https://gitlab.com/leapsight/tuplespace.git"
             },
             lhttpc: %{package: "lhttpc", protocol: :hexpm, version: "1.6.2"},
             hash: %{
               protocol: :git,
               ref: "dfbcc9ee089626f84068a1dcee3b1753a716871e",
               url: "https://github.com/leapsight/hash"
             },
             art: %{
               protocol: :git,
               ref: "35f8db5c877def1b4916791dce9a7858dc94dd33",
               url: "https://gitlab.com/leapsight/art.git"
             },
             kafka_protocol: %{
               package: "kafka_protocol",
               protocol: :hexpm,
               version: "4.0.1"
             },
             stringprep: %{package: "stringprep", protocol: :hexpm, version: "1.0.27"},
             pbkdf2: %{
               protocol: :git,
               ref: "84f964b1875b047c3ad93d43cc350c45d6e27f9c",
               url: "https://github.com/leapsight-oss/erlang-pbkdf2.git"
             },
             sext: %{package: "sext", protocol: :hexpm, version: "1.8.0"},
             types: %{package: "types", protocol: :hexpm, version: "0.1.8"},
             prometheus: %{package: "prometheus", protocol: :hexpm, version: "4.8.1"},
             backoff: %{package: "backoff", protocol: :hexpm, version: "1.1.6"},
             wamp: %{
               protocol: :git,
               ref: "ded9e3b7fffd25e7d18755bf1c085e2d027100ed",
               url: "https://github.com/Leapsight/wamp.git"
             },
             partisan: %{
               protocol: :git,
               ref: "73e1477fe64fc39ae415e7ef9e467736d58cefb6",
               url: "http://github.com/aramallo/partisan.git"
             },
             base62: %{
               protocol: :git,
               ref: "6e1c622c2e9bba51fadba44fa050d4b4ae706890",
               url: "https://gitlab.com/leapsight/base62.git"
             },
             erlcloud: %{package: "erlcloud", protocol: :hexpm, version: "3.6.1"},
             parse_trans: %{package: "parse_trans", protocol: :hexpm, version: "3.4.1"},
             uuid: %{package: "uuid_erl", protocol: :hexpm, version: "2.0.4"},
             unicode_util_compat: %{
               package: "unicode_util_compat",
               protocol: :hexpm,
               version: "0.7.0"
             },
             crc32cer: %{package: "crc32cer", protocol: :hexpm, version: "0.1.8"},
             leap: %{
               protocol: :git,
               ref: "5dcd5c162704048bcd3d37e9ffc7b83f70d66b77",
               url: "https://gitlab.com/leapsight/leap.git"
             },
             certifi: %{package: "certifi", protocol: :hexpm, version: "2.9.0"},
             ksuid: %{
               protocol: :git,
               ref: "a5016017edd1e99d46c9a6d4c348cb818e542b13",
               url: "https://gitlab.com/leapsight/ksuid.git"
             },
             quantile_estimator: %{
               package: "quantile_estimator",
               protocol: :hexpm,
               version: "0.2.1"
             },
             cowlib: %{package: "cowlib", protocol: :hexpm, version: "2.11.0"},
             riak_sysmon: %{
               protocol: :git,
               ref: "726df1f4c31108bb9366fb767b480f286e51f7fc",
               url: "https://github.com/Leapsight/riak_sysmon.git"
             },
             jsone: %{package: "jsone", protocol: :hexpm, version: "1.6.1"},
             gproc: %{package: "gproc", protocol: :hexpm, version: "0.9.0"},
             jobs: %{package: "jobs", protocol: :hexpm, version: "0.10.0"},
             telemetry: %{package: "telemetry", protocol: :hexpm, version: "1.0.0"},
             jose: %{
               protocol: :git,
               ref: "991649695aaccd92c8effb1c1e88e6159fe8e9a6",
               url: "https://github.com/potatosalad/erlang-jose.git"
             },
             snappyer: %{package: "snappyer", protocol: :hexpm, version: "1.2.8"},
             eini: %{package: "eini", protocol: :hexpm, version: "1.2.9"},
             app_config: %{
               protocol: :git,
               ref: "e6a4dc99c0c9f17a6d4d865e40aefc409986f849",
               url: "https://gitlab.com/leapsight/app_config.git"
             },
             utils: %{
               protocol: :git,
               ref: "881e59760358bf22979d6ef1fcd96fc476c8b3ca",
               url: "https://gitlab.com/leapsight/utils.git"
             },
             ssl_verify_fun: %{
               package: "ssl_verify_fun",
               protocol: :hexpm,
               version: "1.1.6"
             },
             key_value: %{
               protocol: :git,
               ref: "414fb19cd067b368666ceb5a2a63f1f24109562b",
               url: "https://gitlab.com/leapsight/key_value.git"
             },
             acceptor_pool: %{package: "acceptor_pool", protocol: :hexpm, version: "1.0.0"},
             quickrand: %{package: "quickrand", protocol: :hexpm, version: "2.0.4"},
             hackney: %{package: "hackney", protocol: :hexpm, version: "1.18.1"},
             recon: %{package: "recon", protocol: :hexpm, version: "2.5.2"},
             observer_cli: %{package: "observer_cli", protocol: :hexpm, version: "1.7.2"},
             prometheus_cowboy: %{
               package: "prometheus_cowboy",
               protocol: :hexpm,
               version: "0.1.8"
             },
             accept: %{package: "accept", protocol: :hexpm, version: "0.3.5"},
             logger_colorful: %{
               package: "logger_colorful",
               protocol: :hexpm,
               version: "0.1.0"
             },
             jsx: %{package: "jsx", protocol: :hexpm, version: "2.11.0"}
           } = Tricorder.Deps.RebarLock.load("test/fixtures/rebar.lock")
  end
end

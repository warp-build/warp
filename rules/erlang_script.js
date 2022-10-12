import ErlangToolchain, { BEAM_EXT, HEADER_EXT } from "https://pkgs.warp.build/toolchains/erlang.js";

const impl = (ctx) => {
  const { label, name, main, apps } = ctx.cfg();

  const cwd = Label.path(label);

  const transitiveDeps = ctx.transitiveDeps().flatMap(dep => dep.outs);

  const includePaths = transitiveDeps
    .filter(path => path.endsWith(HEADER_EXT))
    .unique();

  const transitiveBeams = transitiveDeps
    .filter(path => path.endsWith(BEAM_EXT) || path.endsWith(".app") || path.endsWith(".app.src"))
    .unique();

  const transitiveApps = ctx.transitiveDeps()
    .flatMap(dep => {
      let app = dep.outs.find(path => path.endsWith(".app"));
      if (app) {
        return [{
          name: File.filename(app).replace(".app", ""),
          ebin: File.parent(app),
        }];
      }
      return [];
    })
    .unique();

  ctx.action().runShell({ script: `mkdir -p ${cwd}/ebin` });
  transitiveApps.forEach((app) => {
    ctx.action().runShell({ script: `cp -R ${app.ebin} ${cwd}/ebin/${app.name}`})
  });

  const build = `${cwd}/${name}_escript_builder.erl`;
  const run = `${main}script`;

  /*
   * NOTE(@ostera): to create the final escript we will use the `escript` module
   * in Erlang itself. It's easier than figuring out the internal data format ourselves.
   *
   * So we will just template a tiny temporary Erlang module that will build
   * this exact escript for us.
   */
  ctx.action().writeFile({
    dst: build,
    data: `
-module(${name}_escript_builder).

-export([main/1]).

main(_argv) ->
  % The raw list of files.
  DepBeamFiles = [
    ${
      transitiveBeams
      .map(dep => `"${dep}"`)
      .join(",\n    ")
    }
  ],

  DepApps = maps:from_list([
    ${
      transitiveApps
      .map(({name}) => `{<<"${name}">>, <<"${cwd}/ebin/${name}/${name}.app">>}`)
      .join(",\n    ")
    }
  ]),

  AppNames = [
    ${apps.map(dep => `<<"${dep}">>`).join(",\n    ")}
  ],

  Apps = lists:flatmap(fun (AppName) ->
    AppFile = binary:bin_to_list(<<AppName/binary, ".app">>),
    {AppRoot, EbinPath, PrivPath} = case code:where_is_file(AppFile) of
                non_existing -> 
                  case maps:get(AppName, DepApps, non_existing) of
                    non_existing -> erlang:throw(<<"Missing application: ", AppName/binary>>);
                    AppPath ->
                      AppRoot0 = filename:dirname(filename:dirname(AppPath)),
                      EbinPath0 = binary:bin_to_list(<<AppRoot0/binary, "/", AppName/binary, "/*{app,beam}">>),
                      PrivPath0 = binary:bin_to_list(<<AppRoot0/binary, "/", AppName/binary, "/priv/**/*">>),
                      {AppRoot0, EbinPath0, PrivPath0}
                  end;
                Path ->
                  AppPath = binary:list_to_bin(Path),
                  AppRoot0 = filename:dirname(filename:dirname(AppPath)),
                  EbinPath0 = binary:bin_to_list(<<AppRoot0/binary, "/ebin/*{app,beam}">>),
                  PrivPath0 = binary:bin_to_list(<<AppRoot0/binary, "/priv/**/*">>),
                  {AppRoot0, EbinPath0, PrivPath0}
              end,

    io:format("~p\n", [AppRoot]),
    io:format("~p\n", [EbinPath]),
    io:format("~p\n", [PrivPath]),

    Files = filelib:wildcard(EbinPath) ++ filelib:wildcard(PrivPath),

    lists:map(fun (File) ->
      io:format("~p\n", [File]),
      {ok, Data} = file:read_file(File),
      {File, Data}
    end, Files)
  end, AppNames),

  % Read all the beam bytecode into {"hello.beam", <<bytecode>>} tuples.
  DepBeams = lists:map(fun (BeamFile) ->
    {ok, Data} = file:read_file(BeamFile),
    {filename:basename(BeamFile), Data}
  end, DepBeamFiles),

  SrcFiles = [
    ${
      [main]
      .map(dep => `"${dep}"`)
      .join(",\n    ")
    }
  ],

  % The file names with their contents compiled to bytecode.
  SrcBeams = lists:map(fun (SrcFile) ->
    {ok, _, BeamCode} = compile:file(SrcFile, [binary, debug_info]),
    BeamFile = filename:join([filename:basename(SrcFile, "https://pkgs.warp.build/rules/.erl"), ".beam"]),
    {filename:basename(BeamFile), BeamCode}
  end, SrcFiles),

  Files = Apps ++ SrcBeams ++ DepBeams,

  % The layout of our escript archive.
  Cfg = [
    shebang,
    comment,
    {emu_args, "-pa ebin/jiffy"},
    {archive, Files, []}
  ],

  {ok, Bin} = escript:create(binary, Cfg),
  ok = file:write_file(<<"${File.filename(run)}">>, Bin).
`
  });

  /*
   * NOTE(@ostera): to create the final escript archive, we actually have to
   * go _into_ the folder.
   */
  ctx.action().runShell({
    script: `

escript ${build} || exit 1
mv ${File.filename(run)} ${run}

` })

  ctx.action().declareOutputs([run]);
  ctx.action().setPermissions({file: run, executable: true});
  ctx.action().declareRunScript(run);
};

export default Warp.Rule({
  runnable: true,
  name: "https://pkgs.warp.build/rules/erlang_script",
  mnemonic: "ErlScript",
  impl,
  cfg: {
    name: label(),
    main: file(),
    deps: [label()],
    apps: [string()],
  },
  defaults: {
    deps: [],
    apps: ["kernel"],
  },
  toolchains: [ErlangToolchain],
});

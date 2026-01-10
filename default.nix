{
  buildDotnetModule,
  dotnet-sdk_9,
  dotnet-runtime_9,
}:

buildDotnetModule rec {
  pname = "night-light";
  version = "0.0.1";

  src = ./.;

  projectFile = "NightLight/NightLight.fsproj";
  nugetDeps = ./deps.json;

  dotnet-sdk = dotnet-sdk_9;
  dotnet-runtime = dotnet-runtime_9;

  executables = [ "NightLight" ];
}

{
  buildDotnetModule,
  dotnet-sdk_10,
  dotnet-runtime_10,
}:

buildDotnetModule rec {
  pname = "night-light";
  version = "0.0.1";

  src = ./.;

  projectFile = "NightLight/NightLight.fsproj";
  nugetDeps = ./deps.json;

  dotnet-sdk = dotnet-sdk_10;
  dotnet-runtime = dotnet-runtime_10;

  executables = [ "NightLight" ];
}
